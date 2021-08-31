//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
    pwm, Anschlüsse, Error, InputAnschluss, InputSerialisiert, Level, OutputAnschluss, Trigger,
};

/// Name eines Kontaktes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Kontakt<Anschluss> {
    pub name: Name,
    pub anschluss: Anschluss,
    pub trigger: Trigger,
}

impl Kontakt<InputAnschluss> {
    pub fn read(&mut self) -> Result<Level, Error> {
        self.anschluss.read()
    }

    pub fn set_async_interrupt(
        &mut self,
        callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Error> {
        self.anschluss.set_async_interrupt(self.trigger, callback)
    }

    pub fn clear_async_interrupt(&mut self) -> Result<(), Error> {
        self.anschluss.clear_async_interrupt()
    }
}

impl Serialisiere for Kontakt<InputAnschluss> {
    type Serialisiert = Kontakt<InputSerialisiert>;

    fn serialisiere(&self) -> Kontakt<InputSerialisiert> {
        Kontakt {
            name: self.name.clone(),
            anschluss: self.anschluss.serialisiere(),
            trigger: self.trigger,
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschluss.anschlüsse()
    }
}

impl Reserviere<Kontakt<InputAnschluss>> for Kontakt<InputSerialisiert> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Kontakt<InputAnschluss>> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.anschluss.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: Kontakt { name: self.name, anschluss, trigger: self.trigger },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}
