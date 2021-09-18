//! Verbindung zwischen zwei Gleisen

use crate::{application::typen::*, lookup};

pub(crate) mod rstern;

/// Ein /Verbindung/ repräsentiert Anschlüsse eines Gleises.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Verbindung {
    /// Position des Anschluss
    pub position: Vektor,
    /// Ausgehende Richtung des Anschlusses als Winkel zur x-Achse
    /// (im Uhrzeigersinn, y-Koordinate wächst nach unten)
    pub richtung: Winkel,
}

/// Spezialisierung des Lookup-Traits auf `Verbindung` als Element.
pub trait Lookup<Name>: lookup::Lookup<Name, Verbindung> {}
impl<Name, T: lookup::Lookup<Name, Verbindung>> Lookup<Name> for T {}
