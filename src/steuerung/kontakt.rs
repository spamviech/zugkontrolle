//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::sync::{
    mpsc::{channel, Receiver, RecvError, SendError, Sender},
    Arc, Mutex,
};

use log::error;
use serde::{Deserialize, Serialize};

use crate::anschluss::{
    de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
    pwm, Anschlüsse, Fehler, InputAnschluss, InputSerialisiert, Level, OutputAnschluss, Trigger,
};

/// Name eines Kontaktes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

/// Ein `Kontakt` erlaubt warten auf ein bestimmtes `Trigger`-Ereignis.
#[derive(Debug)]
pub struct Kontakt {
    pub name: Name,
    pub anschluss: InputAnschluss,
    pub trigger: Trigger,
    senders: Arc<Mutex<Vec<Sender<Level>>>>,
}

impl Drop for Kontakt {
    fn drop(&mut self) {
        if let Err(fehler) = self.anschluss.clear_async_interrupt() {
            error!(
                "Fehler beim zurücksetzten des interrupts des Kontaktes {}: {:?}",
                self.name.0, fehler
            )
        }
    }
}

impl Kontakt {
    /// Erzeuge einen neuen Kontakt
    pub fn neu(
        name: Name,
        mut anschluss: InputAnschluss,
        trigger: Trigger,
    ) -> Result<Self, (Fehler, InputAnschluss)> {
        let senders: Arc<Mutex<Vec<Sender<Level>>>> = Arc::new(Mutex::new(Vec::new()));
        let name_clone = name.clone();
        let senders_clone = senders.clone();
        let set_async_interrupt_result = anschluss.set_async_interrupt(trigger, move |level| {
            let senders = &mut *senders_clone.lock().unwrap_or_else(|poison_error| {
                error!("Sender-Mutex für Kontakt {} poisoned!", name_clone.0);
                poison_error.into_inner()
            });
            // iterate over all registered channels, sending them the level
            // start at the end to avoid shifting channels as much as possible
            let mut next = senders.len().checked_sub(1);
            while let Some(i) = next {
                match senders[i].send(level) {
                    Ok(()) => next = i.checked_sub(1),
                    Err(SendError(_level)) => {
                        // channel was disconnected, so no need to send to it anymore
                        senders.remove(i);
                    }
                }
            }
        });
        match set_async_interrupt_result {
            Ok(()) => Ok(Kontakt { name, anschluss, trigger, senders }),
            Err(fehler) => Err((fehler, anschluss)),
        }
    }

    /// Erhalte den aktuellen Wert
    #[inline(always)]
    pub fn lese(&mut self) -> Result<Level, Fehler> {
        self.anschluss.read()
    }

    /// Registriere einen neuen Channel, der auf das Trigger-Event reagiert.
    /// Rückgabewert ist der zugehörige `Receiver`.
    pub fn registriere_trigger_channel(&mut self) -> Receiver<Level> {
        let (sender, receiver) = channel();
        let senders = &mut *self.senders.lock().unwrap_or_else(|poison_error| {
            error!("Sender-Mutex für Kontakt {} poisoned!", self.name.0);
            poison_error.into_inner()
        });
        senders.push(sender);
        receiver
    }

    /// Blockiere den aktuellen Thread bis das aktuelle Trigger-Event ausgelöst wird.
    pub fn warte_auf_trigger(&mut self) -> Result<Level, RecvError> {
        let receiver = self.registriere_trigger_channel();
        receiver.recv()
    }
}

/// Serialisierte Variante eines `Kontakt`es.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KontaktSerialisiert {
    pub name: Name,
    pub anschluss: InputSerialisiert,
    pub trigger: Trigger,
}

impl Serialisiere for Kontakt {
    type Serialisiert = KontaktSerialisiert;

    fn serialisiere(&self) -> KontaktSerialisiert {
        KontaktSerialisiert {
            name: self.name.clone(),
            anschluss: self.anschluss.serialisiere(),
            trigger: self.trigger,
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        // self.anschluss.anschlüsse()
        todo!("Can't split up Kontakt with Drop-Trait implemented")
    }
}

impl Reserviere<Kontakt> for KontaktSerialisiert {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Kontakt> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            mut input_nicht_benötigt,
        } = self.anschluss.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        let anschluss = match Kontakt::neu(self.name, anschluss, self.trigger) {
            Ok(anschluss) => anschluss,
            Err((fehler, anschluss)) => {
                input_nicht_benötigt.push(anschluss);
                return Err(de_serialisieren::Fehler {
                    fehler,
                    pwm_pins: pwm_nicht_benötigt,
                    output_anschlüsse: output_nicht_benötigt,
                    input_anschlüsse: input_nicht_benötigt,
                });
            }
        };
        Ok(Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}
