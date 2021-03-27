//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

pub mod angle;
pub mod canvas;
pub use angle::*;
pub use canvas::*;

use std::ops::Div;

use crate::zugtyp::*;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f64);

pub trait Zugtyp {
    const SPURWEITE: Spurweite;
}
// abgeleitete Größe unter der Umrechnung 1mm
/// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
pub fn abstand<Z: Zugtyp>() -> CanvasAbstand {
    CanvasAbstand::from(Z::SPURWEITE) / 3.
}
/// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
pub fn beschraenkung<Z: Zugtyp>() -> CanvasAbstand {
    CanvasAbstand::from(Z::SPURWEITE) + 2. * abstand::<Z>()
}
/// Äußerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_aussen<Z: Zugtyp>(radius: Radius) -> CanvasAbstand {
    CanvasAbstand::from(radius) + 0.5 * CanvasAbstand::from(Z::SPURWEITE) + abstand::<Z>()
}
/// Innerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_innen<Z: Zugtyp>(radius: Radius) -> CanvasAbstand {
    CanvasAbstand::from(radius) - 0.5 * CanvasAbstand::from(Z::SPURWEITE) - abstand::<Z>()
}

impl Zugtyp for Maerklin {
    #[allow(non_upper_case_globals)]
    const SPURWEITE: Spurweite = Spurweite(16.5);
}

impl Zugtyp for Lego {
    #[allow(non_upper_case_globals)]
    const SPURWEITE: Spurweite = Spurweite(38.);
}

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(f64);
impl Length {
    pub fn new(length: f64) -> Self {
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
    pub fn new(radius: f64) -> Self {
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
