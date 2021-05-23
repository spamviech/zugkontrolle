//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use crate::anschluss::{Error, InputAnschluss, Level, Trigger};

pub struct Kontakt {
    // TODO name ist eigentlich nur für die Anzeige relevant
    pub name: String,
    pub anschluss: InputAnschluss,
    pub trigger: Trigger,
}

impl Kontakt {
    pub fn read(&mut self) -> Result<Level, Error> {
        self.anschluss.read()
    }

    pub fn set_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Error> {
        self.anschluss.set_async_interrupt(trigger, callback)
    }

    pub fn clear_async_interrupt(&mut self) -> Result<(), Error> {
        self.anschluss.clear_async_interrupt()
    }
}
