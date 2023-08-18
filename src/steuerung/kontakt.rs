//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::{
    fmt::Debug,
    sync::{
        mpsc::{channel, Receiver, RecvError, SendError, Sender},
        Arc,
    },
};

use either::Either;
use log::error;
use nonempty::NonEmpty;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        level::Level,
        trigger::Trigger,
        Fehler, InputAnschluss, InputSerialisiert,
    },
    typen::MitName,
};

/// Name eines [Kontaktes](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

/// Hilfs-Trait um existential types zu ermöglichen (Verstecke T).
trait LevelSender {
    fn send(&mut self, level: Level) -> Result<(), SendError<Level>>;
}

impl LevelSender for Sender<Level> {
    fn send(&mut self, level: Level) -> Result<(), SendError<Level>> {
        Sender::send(self, level)
    }
}

impl<T, F: FnMut(Level) -> T> LevelSender for (Sender<T>, F) {
    fn send(&mut self, level: Level) -> Result<(), SendError<Level>> {
        let (sender, f) = self;
        Sender::send(sender, f(level)).map_err(|SendError(_)| SendError(level))
    }
}

/// Ein `Kontakt` erlaubt warten auf ein bestimmtes [Trigger]-Ereignis.
#[derive(Clone)]
pub struct Kontakt {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
    /// Der Anschluss des Kontaktes.
    anschluss: Arc<Mutex<Either<InputAnschluss, InputSerialisiert>>>,
    /// Wer interessiert sich für das [Trigger]-Event.
    senders: Arc<Mutex<Vec<Box<dyn LevelSender + Send>>>>,
}

impl Debug for Kontakt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Kontakt")
            .field("name", &self.name)
            .field("trigger", &self.trigger)
            .field("anschluss", &self.anschluss)
            .field(
                "senders",
                &format!(
                    "Arc(Mutex({:?}))",
                    self.senders.lock().iter().map(|_| "<LevelSender>").collect::<Vec<_>>()
                ),
            )
            .finish()
    }
}

fn entferne_anschluss<T: Serialisiere<S>, S: Clone>(either: &mut Either<T, S>) -> Either<T, S> {
    let serialisiert = serialisiere_anschluss(either);
    std::mem::replace(either, Either::Right(serialisiert))
}

fn serialisiere_anschluss<T: Serialisiere<S>, S: Clone>(either: &Either<T, S>) -> S {
    match either {
        Either::Left(anschluss) => anschluss.serialisiere(),
        Either::Right(serialisiert) => serialisiert.clone(),
    }
}

fn interrupt_zurücksetzen(anschluss: &mut InputAnschluss, kontakt_name: &Name) {
    if let Err(fehler) = anschluss.lösche_async_interrupt() {
        error!(
            "Fehler beim zurücksetzten des interrupts bei Kontakt {}: {:?}",
            kontakt_name.0, fehler
        )
    }
}

impl Drop for Kontakt {
    fn drop(&mut self) {
        let mut kontakt_anschluss = self.anschluss.lock();
        if let Either::Left(anschluss) = &mut *kontakt_anschluss {
            interrupt_zurücksetzen(anschluss, &self.name)
        }
    }
}

impl Kontakt {
    /// Erzeuge einen neuen Kontakt.
    pub fn neu(
        name: Name,
        mut anschluss: InputAnschluss,
        trigger: Trigger,
    ) -> Result<Self, (Fehler, InputAnschluss)> {
        let senders: Arc<Mutex<Vec<Box<dyn LevelSender + Send>>>> =
            Arc::new(Mutex::new(Vec::new()));
        let senders_clone = senders.clone();
        let set_async_interrupt_result = anschluss.setze_async_interrupt(trigger, move |level| {
            let senders = &mut *senders_clone.lock();
            // Iteriere über alle registrierten Kanäle und schicke über sie das neue Level
            let mut next = senders.len().checked_sub(1);
            while let Some(i) = next {
                match senders[i].send(level) {
                    Ok(()) => next = i.checked_sub(1),
                    Err(SendError(_level)) => {
                        // channel was disconnected, so no need to send to it anymore
                        let _ = senders.swap_remove(i);
                    },
                }
            }
        });
        match set_async_interrupt_result {
            Ok(()) => Ok(Kontakt {
                name,
                anschluss: Arc::new(Mutex::new(Either::Left(anschluss))),
                trigger,
                senders,
            }),
            Err(fehler) => Err((fehler, anschluss)),
        }
    }

    /// Registriere einen neuen Channel, der auf das Trigger-Event reagiert.
    /// Rückgabewert ist der zugehörige `Receiver`.
    pub fn registriere_trigger_channel(&mut self) -> Receiver<Level> {
        let (sender, receiver) = channel();
        let senders = &mut *self.senders.lock();
        senders.push(Box::new(sender));
        receiver
    }

    /// Registriere einen neuen Channel, der auf das Trigger-Event reagiert.
    /// Das übergebene Funktion wird mit dem neuen [Level] aufgerufen,
    /// das Ergebnis wird mit dem [Sender] geschickt.
    pub fn registriere_trigger_sender<T: 'static + Send, F: 'static + FnMut(Level) -> T + Send>(
        &mut self,
        sender: Sender<T>,
        f: F,
    ) {
        let senders = &mut *self.senders.lock();
        senders.push(Box::new((sender, f)));
    }

    /// Blockiere den aktuellen Thread bis das aktuelle Trigger-Event ausgelöst wird.
    pub fn warte_auf_trigger(&mut self) -> Result<Level, RecvError> {
        let receiver = self.registriere_trigger_channel();
        receiver.recv()
    }
}

impl MitName for Option<Kontakt> {
    fn name(&self) -> Option<&str> {
        self.as_ref().map(|kontakt| kontakt.name.0.as_str())
    }
}

/// Serialisierbare Variante eines [Kontaktes](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct KontaktSerialisiert {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Der Anschluss des Kontaktes.
    pub anschluss: InputSerialisiert,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
}

impl Serialisiere<KontaktSerialisiert> for Kontakt {
    fn serialisiere(&self) -> KontaktSerialisiert {
        KontaktSerialisiert {
            name: self.name.clone(),
            anschluss: serialisiere_anschluss(&*self.anschluss.lock()),
            trigger: self.trigger,
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        if let Either::Left(mut anschluss) = entferne_anschluss(&mut *self.anschluss.lock()) {
            interrupt_zurücksetzen(&mut anschluss, &self.name);
            anschluss.anschlüsse()
        } else {
            Anschlüsse::default()
        }
    }
}

impl Reserviere<Kontakt> for KontaktSerialisiert {
    type Arg = ();

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        arg: (),
    ) -> Ergebnis<Kontakt> {
        use Ergebnis::*;
        let (anschluss, fehler, mut anschlüsse) =
            match self.anschluss.reserviere(lager, anschlüsse, arg) {
                Wert { anschluss, anschlüsse } => (anschluss, None, anschlüsse),
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    (anschluss, Some(fehler), anschlüsse)
                },
                Fehler { fehler, anschlüsse } => return Fehler { fehler, anschlüsse },
            };
        match (Kontakt::neu(self.name, anschluss, self.trigger), fehler) {
            (Ok(anschluss), None) => Wert { anschluss, anschlüsse },
            (Ok(anschluss), Some(fehler)) => FehlerMitErsatzwert { anschluss, fehler, anschlüsse },
            (Err((kontakt_fehler, anschluss)), fehler) => {
                anschlüsse.input_anschlüsse.push(anschluss);
                let fehler = if let Some(mut fehler) = fehler {
                    fehler.push(kontakt_fehler);
                    fehler
                } else {
                    NonEmpty::singleton(kontakt_fehler)
                };
                Fehler { fehler, anschlüsse }
            },
        }
    }
}

impl MitName for Option<KontaktSerialisiert> {
    fn name(&self) -> Option<&str> {
        self.as_ref().map(|kontakt| kontakt.name.0.as_str())
    }
}
