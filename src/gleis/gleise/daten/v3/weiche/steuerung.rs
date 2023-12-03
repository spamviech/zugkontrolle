//! Serialisierbare Darstellung einer Weichen-Steuerung in Version 3.

use serde::{Deserialize, Serialize};

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
    pub richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

/// Name einer [Weiche](WeicheSerialisiert).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
