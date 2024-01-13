//! Serialisierbare Darstellung eines [`KurvenWeiche`] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gleise::daten::v3::weiche::{orientierung::Orientierung, steuerung},
        weiche::kurve as v4,
    },
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Kurven-Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct KurvenWeicheSerialisiert<Anschlüsse = Option<AnschlüsseSerialisiert>> {
    /// Die Länge der Geraden vor der äußeren Kurve.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Die Orientierung der KurvenWeiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der KurvenWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der KurvenWeiche.
    pub steuerung: Anschlüsse,
}

/// Eine Variante ohne Anschlüsse.
pub type KurvenWeicheUnit = KurvenWeicheSerialisiert<()>;

impl<A> From<KurvenWeicheSerialisiert<A>> for v4::KurvenWeicheUnit {
    fn from(wert: KurvenWeicheSerialisiert<A>) -> Self {
        let KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: _,
        } = wert;
        v4::KurvenWeicheUnit {
            länge,
            radius,
            winkel,
            orientierung: orientierung.into(),
            beschreibung,
            steuerung: (),
        }
    }
}

impl From<v4::KurvenWeicheUnit> for KurvenWeicheUnit {
    fn from(wert: v4::KurvenWeicheUnit) -> Self {
        let v4::KurvenWeicheUnit { länge, radius, winkel, orientierung, beschreibung, steuerung } =
            wert;
        KurvenWeicheUnit {
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
    Innen,
    #[allow(missing_docs)]
    Außen,
}

impl From<Richtung> for v4::Richtung {
    fn from(wert: Richtung) -> Self {
        match wert {
            Richtung::Innen => v4::Richtung::Innen,
            Richtung::Außen => v4::Richtung::Außen,
        }
    }
}

/// Eine Struktur mit von [`Richtung`]-Varianten abgeleiteten Felder.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungAnschlüsseSerialisiert {
    /// [`Richtung::Innen`]
    pub innen: crate::anschluss::OutputSerialisiert,
    /// [`Richtung::Außen`]
    pub außen: crate::anschluss::OutputSerialisiert,
}

impl From<RichtungAnschlüsseSerialisiert> for v4::RichtungAnschlüsseSerialisiert {
    fn from(wert: RichtungAnschlüsseSerialisiert) -> Self {
        let RichtungAnschlüsseSerialisiert { innen, außen } = wert;
        v4::RichtungAnschlüsseSerialisiert { innen, außen }
    }
}
