//! Schaltbare Gleise.

use std::{collections::HashMap, thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{Anschlüsse, Error, Fließend, OutputAnschluss, Reserviere, ToSave};
use crate::lookup::Lookup;

// inklusive Kreuzung
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Weiche<Anschlüsse> {
    pub anschlüsse: Anschlüsse,
}

impl<Anschlüsse> Weiche<Anschlüsse> {
    pub fn schalten<Richtung>(&mut self, richtung: &Richtung) -> Result<(), Error>
    where
        Anschlüsse: Lookup<Richtung, OutputAnschluss>,
    {
        let anschluss = self.anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        Ok(())
    }
}

impl<T, S> ToSave<Weiche<S>> for Weiche<T>
where
    T: ToSave<S>,
    S: Serialize + for<'de> Deserialize<'de>,
{
    fn to_save(&self) -> Weiche<S> {
        Weiche { anschlüsse: self.anschlüsse.to_save() }
    }
}
impl<T: Reserviere<R>, R> Reserviere<Weiche<R>> for Weiche<T> {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<Weiche<R>, Error> {
        Ok(Weiche { anschlüsse: self.anschlüsse.reserviere(anschlüsse)? })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschlüsse> = HashMap<Name, Weiche<Anschlüsse>>;
