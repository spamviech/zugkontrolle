//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

pub mod angle;
pub mod canvas;
pub mod mm;

// re-exports
pub use angle::*;
pub use mm::*;

use super::anchor;
pub use crate::zugtyp::{Anschluss, Zugtyp};

// abgeleitete Größe unter der Umrechnung 1mm
/// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
pub fn abstand<Z: Zugtyp>() -> canvas::Abstand<canvas::Y> {
    Z::SPURWEITE.to_abstand() / 3.
}
/// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
pub fn beschränkung<Z: Zugtyp>() -> canvas::Abstand<canvas::Y> {
    Z::SPURWEITE.to_abstand() + 2. * abstand::<Z>()
}
/// Äußerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_außen<Z: Zugtyp>(
    radius: canvas::Abstand<canvas::Radius>,
) -> canvas::Abstand<canvas::Radius> {
    radius + 0.5 * Z::SPURWEITE.to_abstand().as_radius() + abstand::<Z>().as_radius()
}
/// Innerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_innen<Z: Zugtyp>(
    radius: canvas::Abstand<canvas::Radius>,
) -> canvas::Abstand<canvas::Radius> {
    radius - 0.5 * Z::SPURWEITE.to_abstand().as_radius() - abstand::<Z>().as_radius()
}

pub trait Zeichnen
where
    Self::AnchorPoints: anchor::Lookup<Self::AnchorName>,
{
    /// Maximale x,y-Werte
    fn size(&self) -> canvas::Size;

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    /// Alle Pfade werden mit /canvas::FillRule::EvenOdd/ gefüllt.
    fn fülle(&self) -> Vec<canvas::Path>;

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self) -> Vec<canvas::Path>;

    /// Beschreibung und Position (falls verfügbar)
    fn beschreibung(&self) -> Option<(canvas::Position, &String)>;

    /// Zeigt der /canvas::Vector/ auf das Gleis?
    fn innerhalb(&self, relative_position: canvas::Vector) -> bool;

    /// Identifier for AnchorPoints.
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type AnchorName;
    /// Speicher-Typ für /anchor::Anchor/. Muss /anchor::Lookup<Self::AnchorName>/ implementieren.
    type AnchorPoints;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> Self::AnchorPoints;
}
