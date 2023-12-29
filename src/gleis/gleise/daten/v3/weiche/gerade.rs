//! Serialisierbare Darstellung eines [Weiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gleise::daten::v3::weiche::{orientierung::Orientierung, steuerung},
        weiche::gerade as v4,
    },
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Anschlüsse = Option<AnschlüsseSerialisiert>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Radius der Kurve.
    pub radius: Skalar,
    /// Der Winkel der Kurve.
    pub winkel: Winkel,
    /// Die Orientierung der Weiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der Weiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der Weiche.
    pub steuerung: Anschlüsse,
}

/// Eine Variante ohne Anschlüsse.
pub type WeicheUnit = WeicheSerialisiert<()>;

impl<A> From<WeicheSerialisiert<A>> for v4::WeicheUnit {
    fn from(wert: WeicheSerialisiert<A>) -> Self {
        let WeicheSerialisiert { länge, radius, winkel, orientierung, beschreibung, steuerung: _ } =
            wert;
        v4::WeicheUnit {
            länge,
            radius,
            winkel,
            orientierung: orientierung.into(),
            beschreibung,
            steuerung: (),
        }
    }
}

impl From<v4::WeicheUnit> for WeicheUnit {
    fn from(wert: v4::WeicheUnit) -> Self {
        let v4::WeicheUnit { länge, radius, winkel, orientierung, beschreibung, steuerung } = wert;
        WeicheUnit {
            länge,
            radius,
            winkel,
            orientierung: orientierung.into(),
            beschreibung,
            steuerung,
        }
    }
}

/// Mögliche Richtungen zum Schalten.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Richtung {
    #[allow(missing_docs)]
    Gerade,
    #[allow(missing_docs)]
    Kurve,
}

impl From<Richtung> for v4::Richtung {
    fn from(wert: Richtung) -> Self {
        match wert {
            Richtung::Gerade => v4::Richtung::Gerade,
            Richtung::Kurve => v4::Richtung::Kurve,
        }
    }
}

/// Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungAnschlüsseSerialisiert {
    /// [Richtung::Gerade]
    pub gerade: crate::anschluss::OutputSerialisiert,
    /// [Richtung::Kurve]
    pub kurve: crate::anschluss::OutputSerialisiert,
}

impl From<RichtungAnschlüsseSerialisiert> for v4::RichtungAnschlüsseSerialisiert {
    fn from(wert: RichtungAnschlüsseSerialisiert) -> Self {
        let RichtungAnschlüsseSerialisiert { gerade, kurve } = wert;
        v4::RichtungAnschlüsseSerialisiert { gerade, kurve }
    }
}
