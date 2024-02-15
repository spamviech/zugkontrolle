//! Serialisierbare Darstellung einer Weichen-Steuerung in Version 3.

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::steuerung::weiche as v4;

/// Serialisierbare Repräsentation der Steuerung einer [`Weiche`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [`Weiche`].
    pub richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [`WeicheSerialisiert`].
    pub(crate) fn neu(name: Name, richtung: Richtung, anschlüsse: Anschlüsse) -> Self {
        WeicheSerialisiert { name, richtung, anschlüsse }
    }
}

impl<R3: Into<R4>, A3: Into<A4>, R4, A4> From<WeicheSerialisiert<R3, A3>>
    for v4::WeicheSerialisiert<R4, A4>
{
    fn from(wert: WeicheSerialisiert<R3, A3>) -> Self {
        let WeicheSerialisiert { name, richtung, anschlüsse } = wert;
        v4::WeicheSerialisiert {
            name: name.into(),
            richtung: richtung.into(),
            anschlüsse: anschlüsse.into(),
        }
    }
}

/// Name einer [`Weiche`](WeicheSerialisiert).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

impl From<Name> for v4::Name {
    fn from(wert: Name) -> Self {
        v4::Name(wert.0)
    }
}
