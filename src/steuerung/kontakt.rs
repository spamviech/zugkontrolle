//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    speichern::{self, Reserviere, Reserviert, ToSave},
    Anschlüsse, Error, InputAnschluss, InputSave, Level, Trigger,
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

impl ToSave<InputAnschluss> for Kontakt<InputAnschluss> {
    type Save = Kontakt<InputSave>;

    fn to_save(&self) -> Kontakt<InputSave> {
        Kontakt {
            name: self.name.clone(),
            anschluss: self.anschluss.to_save(),
            trigger: self.trigger,
        }
    }
}

impl Reserviere<Kontakt<InputAnschluss>, InputAnschluss> for Kontakt<InputSave> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = InputAnschluss>,
    ) -> speichern::Result<Kontakt<InputAnschluss>, InputAnschluss> {
        let Reserviert { anschluss, nicht_benötigt } = self
            .anschluss
            .reserviere(anschlüsse, bisherige_anschlüsse.into_iter())
            .map_err(|speichern::Error { fehler, bisherige_anschlüsse }| speichern::Error {
                fehler,
                bisherige_anschlüsse,
            })?;
        Ok(Reserviert {
            anschluss: Kontakt { name: self.name, anschluss, trigger: self.trigger },
            nicht_benötigt,
        })
    }
}
