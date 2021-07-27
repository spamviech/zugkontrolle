//! Schaltbare Gleise.

use std::{fmt::Debug, hash::Hash, thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    speichern::{self, Reserviere, Reserviert, ToSave},
    Anschlüsse, Error, Fließend, OutputAnschluss,
};
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

impl<Richtung, T, Anschluss> ToSave for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de> + Debug,
    T: ToSave + Debug,
    <T as ToSave>::Save: Hash + Eq + Debug,
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
impl<Richtung, T, R, Anschluss> Reserviere<Weiche<Richtung, R>, Anschluss> for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de> + Debug,
    R: ToSave + Debug,
    T: Reserviere<R, Anschluss> + Hash,
    <R as ToSave>::Save: Hash + Eq + Debug,
{
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = Anschluss>,
    ) -> speichern::Result<Weiche<Richtung, R>, Anschluss> {
        let Reserviert { anschluss: anschlüsse, nicht_benötigt } =
            self.anschlüsse.reserviere(anschlüsse, bisherige_anschlüsse.into_iter())?;
        Ok(Reserviert {
            anschluss: Weiche {
                name: self.name,
                aktuelle_richtung: self.aktuelle_richtung.clone(),
                letzte_richtung: self.letzte_richtung.clone(),
                anschlüsse,
            },
            nicht_benötigt,
        })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(400);
