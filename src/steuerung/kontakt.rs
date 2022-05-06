//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::sync::{
    mpsc::{channel, Receiver, RecvError, SendError, Sender},
    Arc,
};

use log::error;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::anschluss::{
    self,
    de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
    level::Level,
    pin::pwm,
    trigger::Trigger,
    Fehler, InputAnschluss, InputSerialisiert, OutputAnschluss,
};

/// Name eines [Kontaktes](Kontakt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

/// Ein `Kontakt` erlaubt warten auf ein bestimmtes [Trigger]-Ereignis.
#[derive(Debug, Clone)]
pub struct Kontakt {
    /// Der Name des Kontaktes.
    pub name: Name,
    /// Wann wird der Kontakt ausgelöst.
    pub trigger: Trigger,
    /// Der Anschluss des Kontaktes.
    anschluss: Arc<Mutex<AnschlussOderSerialisiert<InputAnschluss>>>,
    /// Wer interessiert sich für das [Trigger]-Event.
    senders: Arc<Mutex<Vec<Sender<Level>>>>,
}

#[derive(Debug)]
enum AnschlussOderSerialisiert<T: Serialisiere> {
    Anschluss(T),
    Serialisiert(T::Serialisiert),
}

impl<T> AnschlussOderSerialisiert<T>
where
    T: Serialisiere,
    <T as Serialisiere>::Serialisiert: Clone,
{
    fn entferne_anschluss(&mut self) -> AnschlussOderSerialisiert<T> {
        let serialisiert = self.serialisiere();
        std::mem::replace(self, AnschlussOderSerialisiert::Serialisiert(serialisiert))
    }

    fn serialisiere(&self) -> T::Serialisiert {
        match self {
            AnschlussOderSerialisiert::Anschluss(anschluss) => anschluss.serialisiere(),
            AnschlussOderSerialisiert::Serialisiert(serialisiert) => serialisiert.clone(),
        }
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
        if let AnschlussOderSerialisiert::Anschluss(anschluss) = &mut *kontakt_anschluss {
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
        let senders: Arc<Mutex<Vec<Sender<Level>>>> = Arc::new(Mutex::new(Vec::new()));
        let senders_clone = senders.clone();
        let set_async_interrupt_result = anschluss.setze_async_interrupt(trigger, move |level| {
            let senders = &mut *senders_clone.lock();
            // iterate over all registered channels, sending them the level
            // start at the end to avoid shifting channels as much as possible
            let mut next = senders.len().checked_sub(1);
            while let Some(i) = next {
                match senders[i].send(level) {
                    Ok(()) => next = i.checked_sub(1),
                    Err(SendError(_level)) => {
                        // channel was disconnected, so no need to send to it anymore
                        let _ = senders.remove(i);
                    },
                }
            }
        });
        match set_async_interrupt_result {
            Ok(()) => Ok(Kontakt {
                name,
                anschluss: Arc::new(Mutex::new(AnschlussOderSerialisiert::Anschluss(anschluss))),
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
        senders.push(sender);
        receiver
    }

    /// Blockiere den aktuellen Thread bis das aktuelle Trigger-Event ausgelöst wird.
    pub fn warte_auf_trigger(&mut self) -> Result<Level, RecvError> {
        let receiver = self.registriere_trigger_channel();
        receiver.recv()
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

impl Serialisiere for Kontakt {
    type Serialisiert = KontaktSerialisiert;

    fn serialisiere(&self) -> KontaktSerialisiert {
        KontaktSerialisiert {
            name: self.name.clone(),
            anschluss: self.anschluss.lock().serialisiere(),
            trigger: self.trigger,
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let mut kontakt_anschluss = self.anschluss.lock();
        if let AnschlussOderSerialisiert::Anschluss(mut anschluss) =
            kontakt_anschluss.entferne_anschluss()
        {
            interrupt_zurücksetzen(&mut anschluss, &self.name);
            anschluss.anschlüsse()
        } else {
            (Vec::new(), Vec::new(), Vec::new())
        }
    }
}

impl Reserviere<Kontakt> for KontaktSerialisiert {
    type Arg = ();

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        arg: (),
    ) -> de_serialisieren::Result<Kontakt> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            mut input_nicht_benötigt,
        } = self.anschluss.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, arg)?;
        match Kontakt::neu(self.name, anschluss, self.trigger) {
            Ok(anschluss) => Ok(Reserviert {
                anschluss,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            }),
            Err((fehler, anschluss)) => {
                input_nicht_benötigt.push(anschluss);
                Err(de_serialisieren::Fehler {
                    fehler,
                    pwm_pins: pwm_nicht_benötigt,
                    output_anschlüsse: output_nicht_benötigt,
                    input_anschlüsse: input_nicht_benötigt,
                })
            },
        }
    }
}
