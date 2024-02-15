//! Serialisierbare Darstellung einer [`DreiwegeWeiche`] in Version 3.

use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::OutputSerialisiert;
use zugkontrolle_gleis::weiche::dreiwege as v4;
use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::gleise::daten::v3::weiche::steuerung;

/// Die aktuelle und letzte [Richtung] einer [`DreiwegeWeiche`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungInformation {
    /// Die aktuelle [Richtung] der [`DreiwegeWeiche`].
    pub aktuelle_richtung: Richtung,
    /// Die [Richtung] vor der aktuellen [`Richtung`].
    pub letzte_richtung: Richtung,
}

impl From<RichtungInformation> for v4::RichtungInformation {
    fn from(wert: RichtungInformation) -> Self {
        let RichtungInformation { aktuelle_richtung, letzte_richtung } = wert;
        v4::RichtungInformation {
            aktuelle_richtung: aktuelle_richtung.into(),
            letzte_richtung: letzte_richtung.into(),
        }
    }
}

/// Serialisierbare Darstellung der Steuerung einer [`DreiwegeWeiche`].
type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<RichtungInformation, RichtungAnschlüsseSerialisiert>;

// Folge Konvention TypName -> TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
/// Definition einer Dreiwege-Weiche.
///
/// Bei extremen Winkeln (`<0°`, `>180°`) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct DreiwegeWeicheSerialisiert<Anschlüsse = Option<AnschlüsseSerialisiert>> {
    /// Die Länge der Gerade.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der DreiwegeWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der DreiwegeWeiche.
    pub steuerung: Anschlüsse,
}

// Folge Konvention TypName -> TypNameUnit
#[allow(clippy::module_name_repetitions)]
/// Eine Variante ohne Anschlüsse.
pub type DreiwegeWeicheUnit = DreiwegeWeicheSerialisiert<()>;

impl<A> From<DreiwegeWeicheSerialisiert<A>> for v4::DreiwegeWeicheUnit {
    fn from(wert: DreiwegeWeicheSerialisiert<A>) -> Self {
        let DreiwegeWeicheSerialisiert { länge, radius, winkel, beschreibung, steuerung: _ } = wert;
        v4::DreiwegeWeicheUnit { länge, radius, winkel, beschreibung, steuerung: () }
    }
}
impl From<v4::DreiwegeWeicheUnit> for DreiwegeWeicheUnit {
    fn from(wert: v4::DreiwegeWeicheUnit) -> Self {
        let v4::DreiwegeWeicheUnit { länge, radius, winkel, beschreibung, steuerung } = wert;
        DreiwegeWeicheUnit { länge, radius, winkel, beschreibung, steuerung }
    }
}

#[doc = r" Mögliche Richtungen zum Schalten."]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Richtung {
    #[allow(missing_docs)]
    Gerade,
    #[allow(missing_docs)]
    Links,
    #[allow(missing_docs)]
    Rechts,
}

impl From<Richtung> for v4::Richtung {
    fn from(wert: Richtung) -> Self {
        match wert {
            Richtung::Gerade => v4::Richtung::Gerade,
            Richtung::Links => v4::Richtung::Links,
            Richtung::Rechts => v4::Richtung::Rechts,
        }
    }
}

#[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungAnschlüsseSerialisiert {
    #[doc = "[Richtung::Gerade]"]
    pub gerade: OutputSerialisiert,
    #[doc = "[Richtung::Links]"]
    pub links: OutputSerialisiert,
    #[doc = "[Richtung::Rechts]"]
    pub rechts: OutputSerialisiert,
}

impl From<RichtungAnschlüsseSerialisiert> for v4::RichtungAnschlüsseSerialisiert {
    fn from(wert: RichtungAnschlüsseSerialisiert) -> Self {
        let RichtungAnschlüsseSerialisiert { gerade, links, rechts } = wert;
        v4::RichtungAnschlüsseSerialisiert { gerade, links, rechts }
    }
}
