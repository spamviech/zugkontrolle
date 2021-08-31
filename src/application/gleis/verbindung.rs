//! Verbindung zwischen zwei Gleisen

use rstar;

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
// copy+paste from example implementation for IntegerPoint
impl rstar::Point for Vektor {
    type Scalar = f32;

    const DIMENSIONS: usize = 2;

    fn generate(generator: impl Fn(usize) -> Self::Scalar) -> Self {
        Vektor { x: Skalar(generator(0)), y: Skalar(generator(1)) }
    }

    fn nth(&self, index: usize) -> Self::Scalar {
        match index {
            0 => self.x.0,
            1 => self.y.0,
            _ => unreachable!(),
        }
    }

    fn nth_mut(&mut self, index: usize) -> &mut Self::Scalar {
        match index {
            0 => &mut self.x.0,
            1 => &mut self.y.0,
            _ => unreachable!(),
        }
    }
}

/// Spezialisierung des Lookup-Traits auf `Verbindung` als Element.
pub trait Lookup<Name>: lookup::Lookup<Name, Verbindung> {}
impl<Name, T: lookup::Lookup<Name, Verbindung>> Lookup<Name> for T {}
