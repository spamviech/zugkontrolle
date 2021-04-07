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
pub use canvas::ToAbstand;

use std::ops::Div;

use super::anchor;

// abgeleitete Größe unter der Umrechnung 1mm
/// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
pub fn abstand<Z: Zugtyp>() -> canvas::Abstand {
    Z::SPURWEITE.to_abstand() / 3.
}
/// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
pub fn beschraenkung<Z: Zugtyp>() -> canvas::Abstand {
    Z::SPURWEITE.to_abstand() + 2. * abstand::<Z>()
}
/// Äußerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_aussen<Z: Zugtyp>(radius: Radius) -> canvas::Abstand {
    radius.to_abstand() + 0.5 * Z::SPURWEITE.to_abstand() + abstand::<Z>()
}
/// Innerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_innen<Z: Zugtyp>(radius: Radius) -> canvas::Abstand {
    radius.to_abstand() - 0.5 * Z::SPURWEITE.to_abstand() - abstand::<Z>()
}

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(f32);
impl Length {
    pub const fn new(length: f32) -> Self {
        Length(length)
    }
}
impl Div<Length> for Length {
    type Output = f32;
    fn div(self, other: Length) -> f32 {
        self.0 / other.0
    }
}
impl Div<Radius> for Length {
    type Output = f32;
    fn div(self, other: Radius) -> f32 {
        self.0 / other.0
    }
}

/// Radius \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(f32);
impl Radius {
    pub const fn new(radius: f32) -> Self {
        Radius(radius)
    }
}
impl Div<Radius> for Radius {
    type Output = f32;
    fn div(self, other: Radius) -> f32 {
        self.0 / other.0
    }
}
impl Div<Length> for Radius {
    type Output = f32;
    fn div(self, other: Length) -> f32 {
        self.0 / other.0
    }
}

pub trait Zeichnen
where
    Self::AnchorPoints: anchor::Lookup<Self::AnchorName>,
{
    /// Maximale x,y-Werte
    fn size(&self) -> canvas::Size;

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    fn fuelle(&self) -> Vec<canvas::Path>;

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self) -> Vec<canvas::Path>;

    /// Identifier for AnchorPoints.
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type AnchorName;
    /// Speicher-Typ für /anchor::Point/. Muss /anchor::Lookup<Self::AnchorName>/ implementieren.
    type AnchorPoints;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> Self::AnchorPoints;
}
