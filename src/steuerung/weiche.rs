//! Schaltbare Gleise.

use std::{thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{Anschlüsse, Error, Fließend, OutputAnschluss, Reserviere, ToSave};
use crate::lookup::Lookup;

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Weiche<Anschlüsse> {
    pub name: Name,
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

impl<T: ToSave> ToSave for Weiche<T> {
    type Save = Weiche<T::Save>;

    fn to_save(&self) -> Weiche<T::Save> {
        Weiche { name: self.name.clone(), anschlüsse: self.anschlüsse.to_save() }
    }
}
impl<T: Reserviere<R>, R> Reserviere<Weiche<R>> for Weiche<T> {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<Weiche<R>, Error> {
        Ok(Weiche { name: self.name, anschlüsse: self.anschlüsse.reserviere(anschlüsse)? })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);
