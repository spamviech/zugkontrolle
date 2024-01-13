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
    gleis::gleise::steuerung::{Aktualisieren, SomeAktualisierenSender, Steuerung},
    typen::MitName,
    util::sender_trait::erstelle_sender_trait_existential,
};

/// Name eines [`Kontaktes`](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

erstelle_sender_trait_existential! {
    LevelSender,
    "Hilfs-Trait um existential types zu ermöglichen (Verstecke T).",
    SomeLevelSender,
    "Ein beliebiger [LevelSender].",
    Level,
}

/// Hilfs-Trait um Trait-Objekte für beide Traits zu erstellen.
trait LetztesLevel: Debug + AsRef<Option<Level>> + AsMut<Option<Level>> + Send {}
impl<T: Debug + AsRef<Option<Level>> + AsMut<Option<Level>> + Send> LetztesLevel for T {}

// TODO Würde API verändert, ersetzte durch "Getter".
#[allow(clippy::partial_pub_fields)]
/// Ein `Kontakt` erlaubt warten auf ein bestimmtes [`Trigger`]-Ereignis.
#[derive(Debug, Clone)]
pub struct Kontakt {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
    /// Die letzte bekannte [Level] eines [`Kontaktes`](Kontakt).
    letztes_level: Arc<Mutex<dyn LetztesLevel>>,
    /// Der Anschluss des Kontaktes.
    anschluss: Arc<Mutex<Either<InputAnschluss, InputSerialisiert>>>,
    /// Wer interessiert sich für das [`Trigger`]-Event.
    senders: Arc<Mutex<Vec<SomeLevelSender>>>,
}

/// Entferne den Anschluss aus dem Either und ersetzte ihn durch seine serialisierbare Repräsentation.
fn entferne_anschluss<T: Serialisiere<S>, S: Clone>(either: &mut Either<T, S>) -> Either<T, S> {
    let serialisiert = serialisiere_anschluss(either);
    // std::mem::replace soll offensichtlich sein.
    #[allow(clippy::absolute_paths)]
    std::mem::replace(either, Either::Right(serialisiert))
}

/// Erhalte eine serialisierbare Repräsentation des Anschlusses.
fn serialisiere_anschluss<T: Serialisiere<S>, S: Clone>(either: &Either<T, S>) -> S {
    match either {
        Either::Left(anschluss) => anschluss.serialisiere(),
        Either::Right(serialisiert) => serialisiert.clone(),
    }
}

/// Setzte den Interrupt-Pin eines Anschlusse zurück.
fn interrupt_zurücksetzen(anschluss: &mut InputAnschluss, kontakt_name: &Name) {
    if let Err(fehler) = anschluss.lösche_async_interrupt() {
        error!(
            "Fehler beim zurücksetzten des interrupts bei Kontakt {}: {fehler:?}",
            kontakt_name.0
        );
    }
}

impl Drop for Kontakt {
    fn drop(&mut self) {
        let mut kontakt_anschluss = self.anschluss.lock();
        if let Either::Left(anschluss) = &mut *kontakt_anschluss {
            interrupt_zurücksetzen(anschluss, &self.name);
        }
    }
}

impl Kontakt {
    /// Erzeuge einen neuen Kontakt.
    ///
    /// ## Errors
    ///
    /// Fehler beim setzen des async Interrupt callbacks.
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
        let senders_clone = Arc::clone(&senders);
        let trigger_copy = trigger;
        let letztes_level_clone = Arc::clone(&letztes_level);
        let aktualisieren_sender = Mutex::new(aktualisieren_sender);
        let set_async_interrupt_result =
            anschluss.setze_async_interrupt(Trigger::Both, move |level| {
                let callback_aufrufen = {
                    let mut guard = letztes_level_clone.lock();
                    let letztes_level_mut = guard.as_mut();
                    let callback_aufrufen = letztes_level_mut
                        .map(|bisher| trigger_copy.callback_aufrufen(level, bisher))
                        .unwrap_or(true);
                    *letztes_level_mut = Some(level);
                    callback_aufrufen
                };
                if let Err(fehler) = aktualisieren_sender.lock().send(Aktualisieren) {
                    log::error!(
                        "Kein Empfänger für Aktualisieren-Nachricht bei Level-Änderung des Kontaktes {}: {:?}",
                        name_clone.0,
                        fehler
                    );
                }
                if callback_aufrufen {
                    let aktuelle_senders = &mut *senders_clone.lock();
                    // Iteriere über alle registrierten Kanäle und schicke über sie das neue Level.
                    let mut next = aktuelle_senders.len().checked_sub(1);
                    while let Some(index) = next {
                        // 0 <= index < senders.len()
                        // range-based for-loop nicht sinnvoll, da disconnected Sender entfernt werden sollen.
                        #[allow(clippy::indexing_slicing)]
                        match aktuelle_senders[index].send(level) {
                            Ok(()) => next = index.checked_sub(1),
                            Err(SendError(_level)) => {
                                // channel was disconnected, so no need to send to it anymore.
                                // swap_remove ist kein Problem, da in zukünftigen Schleifendurchläufen
                                // nur Sender mit kleinerem Index betrachtet werden.
                                let _ = aktuelle_senders.swap_remove(index);
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
    /// Das übergebene Funktion wird mit dem neuen [`Level`] aufgerufen,
    /// das Ergebnis wird mit dem [`Sender`] geschickt.
    pub fn registriere_trigger_sender<T, F>(&mut self, sender: Sender<T>, erzeuge_nachricht: F)
    where
        T: 'static + Send,
        F: 'static + Fn(Level) -> T + Clone + Send,
    {
        let senders = &mut *self.senders.lock();
        senders.push(SomeLevelSender::from((sender, erzeuge_nachricht)));
    }

    /// Blockiere den aktuellen Thread bis das aktuelle Trigger-Event ausgelöst wird.
    ///
    /// ## Errors
    ///
    /// Alle zugehörigen [`Sender`] wurden gedroppt.
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

// Folge der Konvention TypName->TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Serialisierbare Variante eines [`Kontaktes`](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct KontaktSerialisiert {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Der Anschluss des Kontaktes.
    pub anschluss: InputSerialisiert,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
}

impl KontaktSerialisiert {
    /// Erstelle einen neuen [`KontaktSerialisiert`].
    #[must_use]
    pub fn neu(name: Name, anschluss: InputSerialisiert, trigger: Trigger) -> Self {
        KontaktSerialisiert { name, anschluss, trigger }
    }
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
    type MoveArg = SomeAktualisierenSender;
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        anschlüsse: Anschlüsse,
        aktualisieren_sender: Self::MoveArg,
        ref_arg: &Self::RefArg,
        mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Kontakt> {
        use Ergebnis::{Fehler, FehlerMitErsatzwert, Wert};
        // anschlüsse ist die selbe Struktur nach ausführen von `reserviere`.
        #[allow(clippy::shadow_unrelated)]
        let (mut anschluss, fehler, mut anschlüsse) =
            match self.anschluss.reserviere(lager, anschlüsse, (), ref_arg, mut_ref_arg) {
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
                Steuerung::neu(letztes_level, aktualisieren_sender.clone()),
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

// Wird nicht qualifiziert verwendet.
#[allow(clippy::module_name_repetitions)]
/// Trait für Typen mit einem [`Kontakt`].
pub trait MitKontakt {
    /// Erhalte das aktuelle [Level] und den gewählten [`Trigger`].
    fn aktuelles_level_und_trigger(&self) -> Option<(Option<Level>, Trigger)>;
}

impl MitKontakt for () {
    fn aktuelles_level_und_trigger(&self) -> Option<(Option<Level>, Trigger)> {
        None
    }
}

impl<T: MitKontakt> MitKontakt for Option<T> {
    fn aktuelles_level_und_trigger(&self) -> Option<(Option<Level>, Trigger)> {
        self.as_ref().and_then(MitKontakt::aktuelles_level_und_trigger)
    }
}

impl MitKontakt for Kontakt {
    fn aktuelles_level_und_trigger(&self) -> Option<(Option<Level>, Trigger)> {
        Some((*self.letztes_level.lock().as_ref(), self.trigger))
    }
}
