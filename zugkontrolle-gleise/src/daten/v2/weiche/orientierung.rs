//! Serialisierbare Darstellung der Orientierung einer [`Weiche`], in welche Richtung geht die Kurve. in Version 3.

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::weiche::orientierung as v4;

/// Die Orientierung einer [`Weiche`], in welche Richtung geht die Kurve.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientierung {
    /// Die Kurve geht nach links.
    Links,
    /// Die Kurve geht nach rechts.
    Rechts,
}

impl From<Orientierung> for v4::Orientierung {
    fn from(wert: Orientierung) -> Self {
        match wert {
            Orientierung::Links => v4::Orientierung::Links,
            Orientierung::Rechts => v4::Orientierung::Rechts,
        }
    }
}

impl From<v4::Orientierung> for Orientierung {
    fn from(wert: v4::Orientierung) -> Self {
        match wert {
            v4::Orientierung::Links => Orientierung::Links,
            v4::Orientierung::Rechts => Orientierung::Rechts,
        }
    }
}
