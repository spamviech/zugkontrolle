//! Schaltbare Gleise.

use std::{collections::HashMap, fmt::Debug, hash::Hash, thread::sleep, time::Duration};

use log::error;
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

impl<Richtung, T, Anschluss> ToSave<Anschluss> for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de> + Debug,
    T: ToSave<Anschluss> + Debug,
    <T as ToSave<Anschluss>>::Save: Hash + Eq + Debug,
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
    R: ToSave<Anschluss> + Debug,
    T: Reserviere<R, Anschluss> + Hash,
    <R as ToSave<Anschluss>>::Save: Hash + Eq + Debug,
{
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = Weiche<Richtung, R>>,
    ) -> speichern::Result<Weiche<Richtung, R>, Anschluss> {
        let (save, rs) =
            bisherige_anschlüsse.fold((HashMap::new(), Vec::new()), |mut acc, weiche| {
                acc.0.insert(weiche.anschlüsse.to_save(), weiche.to_save());
                acc.1.push(weiche.anschlüsse);
                acc
            });
        // Nicht gefundene Anschlüsse werden über drop-Handler ans Singleton zurückgegeben
        // Es sollten alle Anschlüsse gefunden werden
        let konvertiere_anschlüsse = |vec: Vec<R>| {
            vec.into_iter()
                .filter_map(|r| match save.remove(&r.to_save()) {
                    Some(Weiche { name, aktuelle_richtung, letzte_richtung, anschlüsse: _ }) => {
                        Some(Weiche { name, aktuelle_richtung, letzte_richtung, anschlüsse: r })
                    }
                    None => {
                        error!(
                            "Anschluss {:?} nicht in Map bisheriger Weichen {:?} gefunden!",
                            r, save
                        );
                        None
                    }
                })
                .collect::<Vec<_>>()
        };
        let Reserviert { anschluss: anschlüsse, nicht_benötigt } = self
            .anschlüsse
            .reserviere(anschlüsse, rs.into_iter())
            .map_err(|speichern::Error { fehler, bisherige_anschlüsse }| speichern::Error {
                fehler,
                bisherige_anschlüsse: konvertiere_anschlüsse(bisherige_anschlüsse),
            })?;
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
