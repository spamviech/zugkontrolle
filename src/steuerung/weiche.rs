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
pub struct Weiche<Richtung, Anschlüsse> {
    pub name: Name,
    pub aktuelle_richtung: Richtung,
    pub letzte_richtung: Richtung,
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Lookup<Richtung, OutputAnschluss>,
{
    pub fn schalten(&mut self, richtung: &Richtung) -> Result<(), Error> {
        let anschluss = self.anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        self.letzte_richtung = self.aktuelle_richtung.clone();
        self.aktuelle_richtung = richtung.clone();
        Ok(())
    }
}

impl<Richtung, T> ToSave for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: ToSave,
{
    type Save = Weiche<Richtung, T::Save>;

    fn to_save(&self) -> Weiche<Richtung, T::Save> {
        Weiche {
            name: self.name.clone(),
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self.anschlüsse.to_save(),
        }
    }
}
impl<Richtung: Clone, T: Reserviere<R>, R> Reserviere<Weiche<Richtung, R>> for Weiche<Richtung, T> {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<Weiche<Richtung, R>, Error> {
        Ok(Weiche {
            name: self.name,
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self.anschlüsse.reserviere(anschlüsse)?,
        })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);
