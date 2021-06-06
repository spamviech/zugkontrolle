//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::anschluss::{Anschlüsse, Error, InputAnschluss, InputSave, Level, Trigger};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Kontakt<Anschluss> {
    pub anschluss: Anschluss,
}

impl Kontakt<InputAnschluss> {
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

    pub fn to_save(&self) -> Kontakt<InputSave> {
        Kontakt { anschluss: self.anschluss.to_save() }
    }
}

impl Kontakt<InputSave> {
    pub fn reserviere(
        self, anschlüsse: &mut Anschlüsse
    ) -> Result<Kontakt<InputAnschluss>, Error> {
        Ok(Kontakt { anschluss: self.anschluss.reserviere(anschlüsse)? })
    }
}

/// Name eines Kontaktes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschluss> = BTreeMap<Name, Kontakt<Anschluss>>;
