//! Ein Streckenabschnitt regelt die Stromzufuhr.

use std::collections::HashMap;

use log::error;
use serde::{Deserialize, Serialize};

use crate::anschluss::{
    speichern::{self, Reserviere, Reserviert, ToSave},
    Anschlüsse, Error, Fließend, OutputAnschluss, OutputSave,
};
use crate::farbe::Farbe;

pub type StreckenabschnittSave = Streckenabschnitt<OutputSave>;
/// Steuerung der Stromzufuhr.
#[derive(Debug, Serialize, Deserialize)]
pub struct Streckenabschnitt<Anschluss = OutputAnschluss> {
    pub farbe: Farbe,
    pub anschluss: Anschluss,
}

impl Streckenabschnitt<OutputAnschluss> {
    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Error> {
        self.anschluss.einstellen(fließend)
    }

    pub fn strom_umschalten(&mut self) -> Result<(), Error> {
        self.anschluss.umstellen()
    }
}

impl ToSave<OutputAnschluss> for Streckenabschnitt {
    type Save = StreckenabschnittSave;

    fn to_save(&self) -> Streckenabschnitt<OutputSave> {
        Streckenabschnitt { farbe: self.farbe, anschluss: self.anschluss.to_save() }
    }
}

impl Reserviere<Streckenabschnitt, OutputAnschluss> for StreckenabschnittSave {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = Streckenabschnitt>,
    ) -> speichern::Result<Streckenabschnitt, OutputAnschluss> {
        let (save, output_anschlüsse) =
            bisherige_anschlüsse.fold((HashMap::new(), Vec::new()), |mut acc, kontakt| {
                acc.0.insert(kontakt.anschluss.to_save(), kontakt.to_save());
                acc.1.push(kontakt.anschluss);
                acc
            });
        // Nicht gefundene Anschlüsse werden über drop-Handler ans Singleton zurückgegeben
        // Es sollten alle Anschlüsse gefunden werden
        let konvertiere_anschlüsse = |vec: Vec<OutputAnschluss>| {
            vec.into_iter()
                .filter_map(|anschluss| match save.remove(&anschluss.to_save()) {
                    Some(Streckenabschnitt { farbe, anschluss: _ }) => {
                        Some(Streckenabschnitt { farbe, anschluss })
                    }
                    None => {
                        error!(
                            "Anschluss {:?} nicht in Map bisheriger Kontakte {:?} gefunden!",
                            anschluss, save
                        );
                        None
                    }
                })
                .collect::<Vec<Streckenabschnitt>>()
        };
        let Reserviert { anschluss, nicht_benötigt } = self
            .anschluss
            .reserviere(anschlüsse, output_anschlüsse.into_iter())
            .map_err(|speichern::Error { fehler, bisherige_anschlüsse }| speichern::Error {
                fehler,
                bisherige_anschlüsse: konvertiere_anschlüsse(bisherige_anschlüsse),
            })?;
        Ok(Reserviert {
            anschluss: Streckenabschnitt { farbe: self.farbe, anschluss },
            nicht_benötigt: konvertiere_anschlüsse(nicht_benötigt),
        })
    }
}
/// Name eines Streckenabschnittes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschluss = OutputAnschluss> = HashMap<Name, (Streckenabschnitt<Anschluss>, Fließend)>;
