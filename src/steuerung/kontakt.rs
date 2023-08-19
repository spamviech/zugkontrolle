//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::{
    fmt::{self, Debug},
    ops::{Deref, DerefMut},
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
    gleis::gleise::steuerung::Steuerung,
    typen::{canvas::Cache, MitName},
};

/// Name eines [Kontaktes](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

macro_rules! erstelle_sender_trait_existential {
    ($(($vis: vis),)? $trait: ident, $trait_doc: literal, $existential: ident, $existential_doc: literal, $msg: ty $(,)?) => {
        #[doc = $trait_doc]
        #[dyn_clonable::clonable]
        $($vis)? trait $trait: Clone + Send {
            #[doc = "Sende eine [$msg]-Nachricht."]
            fn send(&self, level: $msg) -> Result<(), SendError<$msg>>;

            #[doc = "[Debug]-Ausgabe zur Darstellung eines [$existential]."]
            fn debug_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
        }

        impl $trait for Sender<$msg> {
            fn send(&self, value: $msg) -> Result<(), SendError<$msg>> {
                Sender::send(self, value)
            }

            fn debug_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                <Self as Debug>::fmt(self, f)
            }
        }

        impl<T: Send, F: Fn($msg) -> T + Clone + Send> $trait for (Sender<T>, F) {
            fn send(&self, msg: $msg) -> Result<(), SendError<$msg>> {
                let (sender, f) = self;
                Sender::send(sender, f(msg)).map_err(|SendError(_)| SendError(msg))
            }

            fn debug_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple("").field(&self.0).field(&"<closure>").finish()
            }
        }

        #[doc = $existential_doc]
        #[derive(Clone)]
        $($vis)? struct $existential(Box<dyn $trait>);

        impl Debug for $existential {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("SomeLevelSender(")?;
                self.0.debug_fmt(f)?;
                f.write_str(")")
            }
        }

        impl<T: 'static + $trait + Send> From<T> for $existential {
            fn from(value: T) -> Self {
                $existential(Box::new(value))
            }
        }

        impl Deref for $existential {
            type Target = dyn $trait;

            fn deref(&self) -> &Self::Target {
                self.0.as_ref()
            }
        }

        impl DerefMut for $existential {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.0.as_mut()
            }
        }
    };
}

erstelle_sender_trait_existential! {
    LevelSender,
    "Hilfs-Trait um existential types zu ermöglichen (Verstecke T).",
    SomeLevelSender,
    "Ein beliebiger [LevelSender] mit Debug-Implementierung.",
    Level,
}

/// Das aktuelle level hat sich geändert, das UI muss aktualisiert werden.
#[derive(Debug, Clone, Copy)]
pub struct Aktualisieren;

erstelle_sender_trait_existential! {
    (pub),
    AktualisierenSender,
    "Sende eine [Aktualisieren]-Nachricht.",
    SomeAktualisierenSender,
    "Ein beliebiger [AktualisierenSender].",
    Aktualisieren,
}

/// Hilfs-Trait um Trait-Objekte für beide Traits zu erstellen.
trait LetztesLevel: Debug + AsRef<Option<Level>> + AsMut<Option<Level>> + Send {}
impl<T: Debug + AsRef<Option<Level>> + AsMut<Option<Level>> + Send> LetztesLevel for T {}

/// Ein `Kontakt` erlaubt warten auf ein bestimmtes [Trigger]-Ereignis.
#[derive(Debug, Clone)]
pub struct Kontakt {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
    /// Die letzte bekannte [Level] eines [Kontaktes](Kontakt).
    letztes_level: Arc<Mutex<dyn LetztesLevel>>,
    /// Der Anschluss des Kontaktes.
    anschluss: Arc<Mutex<Either<InputAnschluss, InputSerialisiert>>>,
    /// Wer interessiert sich für das [Trigger]-Event.
    senders: Arc<Mutex<Vec<SomeLevelSender>>>,
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
        letztes_level: impl 'static + Debug + AsRef<Option<Level>> + AsMut<Option<Level>> + Send,
        aktualisieren_sender: SomeAktualisierenSender,
    ) -> Result<Self, (Fehler, InputAnschluss)> {
        let senders: Arc<Mutex<Vec<SomeLevelSender>>> = Arc::new(Mutex::new(Vec::new()));
        let letztes_level = Arc::new(Mutex::new(letztes_level));

        let name_clone = name.clone();
        let senders_clone = senders.clone();
        let trigger_copy = trigger;
        let letztes_level_clone = letztes_level.clone();
        let aktualisieren_sender = Mutex::new(aktualisieren_sender);
        let set_async_interrupt_result =
            anschluss.setze_async_interrupt(Trigger::Both, move |level| {
                let mut guard = letztes_level_clone.lock();
                let letztes_level_mut = guard.as_mut();
                let callback_aufrufen = letztes_level_mut
                    .map(|bisher| trigger_copy.callback_aufrufen(level, bisher))
                    .unwrap_or(true);
                *letztes_level_mut = Some(level);
                drop(guard);
                if let Err(fehler) = aktualisieren_sender.lock().send(Aktualisieren) {
                    log::error!(
                        "Kein Empfänger für Aktualisieren-Nachricht bei Level-Änderung des Kontaktes {}: {:?}",
                        name_clone.0,
                        fehler
                    );
                }
                if callback_aufrufen {
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
                }
            });

        match set_async_interrupt_result {
            Ok(()) => Ok(Kontakt {
                name,
                trigger,
                letztes_level,
                anschluss: Arc::new(Mutex::new(Either::Left(anschluss))),
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
        senders.push(SomeLevelSender::from(sender));
        receiver
    }

    /// Registriere einen neuen Channel, der auf das Trigger-Event reagiert.
    /// Das übergebene Funktion wird mit dem neuen [Level] aufgerufen,
    /// das Ergebnis wird mit dem [Sender] geschickt.
    pub fn registriere_trigger_sender<T, F>(&mut self, sender: Sender<T>, f: F)
    where
        T: 'static + Send,
        F: 'static + Fn(Level) -> T + Clone + Send,
    {
        let senders = &mut *self.senders.lock();
        senders.push(SomeLevelSender::from((sender, f)));
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
    type Arg = (Arc<Mutex<Cache>>, SomeAktualisierenSender);

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        (cache, aktualisieren_sender): Self::Arg,
    ) -> Ergebnis<Kontakt> {
        use Ergebnis::*;
        let (mut anschluss, fehler, mut anschlüsse) =
            match self.anschluss.reserviere(lager, anschlüsse, ()) {
                Wert { anschluss, anschlüsse } => (anschluss, None, anschlüsse),
                FehlerMitErsatzwert { anschluss, fehler, anschlüsse } => {
                    (anschluss, Some(fehler), anschlüsse)
                },
                Fehler { fehler, anschlüsse } => return Fehler { fehler, anschlüsse },
            };
        let letztes_level = anschluss.lese().ok();
        match (
            Kontakt::neu(
                self.name,
                anschluss,
                self.trigger,
                Steuerung::neu(letztes_level, cache),
                aktualisieren_sender,
            ),
            fehler,
        ) {
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

/// Trait für Typen mit einem aktuellen [Level].
pub trait MitLevel {
    /// Erhalte das aktuelle [Level].
    fn aktuelles_level(&self) -> Option<Level>;
}

impl MitLevel for () {
    fn aktuelles_level(&self) -> Option<Level> {
        None
    }
}

impl<T: MitLevel> MitLevel for Option<T> {
    fn aktuelles_level(&self) -> Option<Level> {
        self.as_ref().and_then(MitLevel::aktuelles_level)
    }
}

impl MitLevel for Kontakt {
    fn aktuelles_level(&self) -> Option<Level> {
        *self.letztes_level.lock().as_ref()
    }
}
