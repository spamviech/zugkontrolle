//! Serialisierbare Darstellung eines [Weiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::de_serialisieren::Anschlüsse,
    gleis::gleise::daten::v3::weiche::{orientierung::Orientierung, steuerung},
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Weiche<Anschlüsse = Option<self::Anschlüsse>> {
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

#[doc = r" Eine Variante ohne Anschlüsse."]
pub type WeicheSerialisiert = Weiche<Option<AnschlüsseSerialisiert>>;
#[doc = r" Eine serialisierbare Repräsentation."]
pub type WeicheUnit = Weiche<()>;

#[doc = r" Mögliche Richtungen zum Schalten."]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Richtung {
    #[allow(missing_docs)]
    Gerade,
    #[allow(missing_docs)]
    Kurve,
}
#[doc = "Eine Struktur mit von [Richtung]-Varianten abgeleiteten Felder."]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungAnschlüsseSerialisiert {
    #[doc = "[Richtung::Gerade]"]
    pub gerade: crate::anschluss::OutputSerialisiert,
    #[doc = "[Richtung::Kurve]"]
    pub kurve: crate::anschluss::OutputSerialisiert,
}
