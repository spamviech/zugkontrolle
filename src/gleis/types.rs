//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

pub mod angle;
pub mod canvas;
pub use crate::zugtyp::{Spurweite, Zugtyp};
pub use angle::*;
pub use canvas::*;

use std::ops::Div;

// abgeleitete Größe unter der Umrechnung 1mm
/// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
pub fn abstand<Z: Zugtyp>() -> CanvasAbstand {
    Z::SPURWEITE.to_abstand() / 3.
}
/// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
pub fn beschraenkung<Z: Zugtyp>() -> CanvasAbstand {
    Z::SPURWEITE.to_abstand() + 2. * abstand::<Z>()
}
/// Äußerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_aussen<Z: Zugtyp>(radius: Radius) -> CanvasAbstand {
    radius.to_abstand() + 0.5 * Z::SPURWEITE.to_abstand() + abstand::<Z>()
}
/// Innerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_innen<Z: Zugtyp>(radius: Radius) -> CanvasAbstand {
    radius.to_abstand() - 0.5 * Z::SPURWEITE.to_abstand() - abstand::<Z>()
}

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(f64);
impl Length {
    pub const fn new(length: f64) -> Self {
        Length(length)
    }
}
impl Div<Length> for Length {
    type Output = f64;
    fn div(self, other: Length) -> f64 {
        self.0 / other.0
    }
}
impl Div<Radius> for Length {
    type Output = f64;
    fn div(self, other: Radius) -> f64 {
        self.0 / other.0
    }
}

/// Radius \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(f64);
impl Radius {
    pub const fn new(radius: f64) -> Self {
        Radius(radius)
    }
}
impl Div<Radius> for Radius {
    type Output = f64;
    fn div(self, other: Radius) -> f64 {
        self.0 / other.0
    }
}
impl Div<Length> for Radius {
    type Output = f64;
    fn div(self, other: Length) -> f64 {
        self.0 / other.0
    }
}
