//! Kontakt, der über einen Anschluss ausgelesen werden kann.

use std::collections::HashMap;

use ::serde::{Deserialize, Serialize};
use log::error;

use crate::anschluss::{
    serde::{self, Reserviere, Reserviert, ToSave},
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

impl ToSave for Kontakt<InputAnschluss> {
    type Save = Kontakt<InputSave>;

    fn to_save(&self) -> Kontakt<InputSave> {
        Kontakt {
            name: self.name.clone(),
            anschluss: self.anschluss.to_save(),
            trigger: self.trigger,
        }
    }
}

impl Reserviere<Kontakt<InputAnschluss>> for Kontakt<InputSave> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = Kontakt<InputAnschluss>>,
    ) -> serde::Result<Kontakt<InputAnschluss>> {
        let (save, input_anschlüsse) =
            bisherige_anschlüsse.fold((HashMap::new(), Vec::new()), |mut acc, kontakt| {
                acc.0.insert(kontakt.anschluss.to_save(), kontakt.to_save());
                acc.1.push(kontakt.anschluss);
                acc
            });
        // Nicht gefundene Anschlüsse werden über drop-Handler ans Singleton zurückgegeben
        // Es sollten alle Anschlüsse gefunden werden
        let konvertiere_anschlüsse = |vec: Vec<InputAnschluss>| {
            vec.into_iter()
                .filter_map(|anschluss| match save.remove(&anschluss.to_save()) {
                    Some(Kontakt { name, anschluss: _, trigger }) => {
                        Some(Kontakt { name, anschluss, trigger })
                    }
                    None => {
                        error!(
                            "Anschluss {:?} nicht in Map bisheriger Kontakte {:?} gefunden!",
                            anschluss, save
                        );
                        None
                    }
                })
                .collect::<Vec<Kontakt<InputAnschluss>>>()
        };
        let Reserviert { anschluss, nicht_benötigt } = self
            .anschluss
            .reserviere(anschlüsse, input_anschlüsse.into_iter())
            .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                fehler,
                bisherige_anschlüsse: konvertiere_anschlüsse(bisherige_anschlüsse),
            })?;
        Ok(Reserviert {
            anschluss: Kontakt { name: self.name, anschluss, trigger: self.trigger },
            nicht_benötigt: konvertiere_anschlüsse(nicht_benötigt),
        })
    }
}
