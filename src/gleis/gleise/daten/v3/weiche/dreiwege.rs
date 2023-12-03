//! Serialisierbare Darstellung einer [DreiwegeWeiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::gleise::daten::v3::weiche::steuerung,
    typen::{skalar::Skalar, winkel::Winkel},
};

/// Die aktuelle und letzte [Richtung] einer [DreiwegeWeiche].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungInformation {
    /// Die aktuelle [Richtung] der [DreiwegeWeiche].
    pub aktuelle_richtung: Richtung,
    /// Die [Richtung] vor der aktuellen [Richtung].
    pub letzte_richtung: Richtung,
}

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<RichtungInformation, RichtungAnschlüsseSerialisiert>;

/// Definition einer Dreiwege-Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DreiwegeWeicheSerialisiert {
    /// Die Länge der Gerade.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der DreiwegeWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der DreiwegeWeiche.
    pub steuerung: AnschlüsseSerialisiert,
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

#[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungAnschlüsseSerialisiert {
    #[doc = "[Richtung::Gerade]"]
    pub gerade: crate::anschluss::OutputSerialisiert,
    #[doc = "[Richtung::Links]"]
    pub links: crate::anschluss::OutputSerialisiert,
    #[doc = "[Richtung::Rechts]"]
    pub rechts: crate::anschluss::OutputSerialisiert,
}
