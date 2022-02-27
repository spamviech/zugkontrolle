//! Verbindung zwischen zwei Gleisen

use crate::{
    nachschlagen,
    typen::{vektor::Vektor, winkel::Winkel},
};

/// Ein `Verbindung` repräsentiert Anschlüsse eines Gleises.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Verbindung {
    /// Position des Anschluss
    pub position: Vektor,
    /// Ausgehende Richtung des Anschlusses als Winkel zur x-Achse
    /// (im Uhrzeigersinn, y-Koordinate wächst nach unten)
    pub richtung: Winkel,
}

/// Spezialisierung des Nachschlagen-Traits auf `Verbindung` als Element.
pub trait Nachschlagen<Name>: nachschlagen::Nachschlagen<Name, Verbindung> {}
impl<Name, T: nachschlagen::Nachschlagen<Name, Verbindung>> Nachschlagen<Name> for T {}
