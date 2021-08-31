//! newtypes auf f32, um zwischen mm-basierten und Pixel-basierten Größen zu unterscheiden

pub mod canvas;
pub mod mm;
pub mod skalar;
pub mod winkel;

// re-exports
pub use canvas::{pfad, Bogen, Cache, Frame, Pfad, Position, Transformation};
pub use mm::*;
pub use skalar::Skalar;
pub use winkel::*;
pub mod vektor;
pub use vektor::Vektor;

pub use crate::zugtyp::Zugtyp;
use crate::{
    application::gleis::verbindung,
    steuerung::{kontakt::Kontakt, weiche::Weiche},
};

// abgeleitete Größe unter der Umrechnung von /mm/ auf /Pixel/
/// Abstand beider Schienen
pub fn spurweite<Z: Zugtyp>() -> Skalar {
    Z::SPURWEITE.als_skalar()
}
/// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
pub fn abstand<Z: Zugtyp>() -> Skalar {
    spurweite::<Z>() / Skalar(3.)
}
/// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
pub fn beschränkung<Z: Zugtyp>() -> Skalar {
    spurweite::<Z>() + abstand::<Z>().doppelt()
}
/// Innerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_innen<Z: Zugtyp>(radius: Skalar) -> Skalar {
    radius - Skalar(0.5) * spurweite::<Z>() - abstand::<Z>()
}
/// Äußerster Radius (inklusive Beschränkung) einer Kurve
pub fn radius_begrenzung_außen<Z: Zugtyp>(radius: Skalar) -> Skalar {
    radius + Skalar(0.5) * spurweite::<Z>() + abstand::<Z>()
}

/// Wird ein Pfad mit voller oder reduzierter Transparenz gefüllt
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transparenz {
    Voll,
    Reduziert,
    Minimal,
}

impl Transparenz {
    pub fn true_reduziert(input: bool) -> Transparenz {
        if input {
            Transparenz::Reduziert
        } else {
            Transparenz::Voll
        }
    }

    pub fn kombiniere(self, other: Transparenz) -> Transparenz {
        use Transparenz::*;
        match (self, other) {
            (Minimal, _) | (_, Minimal) => Minimal,
            (Reduziert, Reduziert) => Minimal,
            (Voll, Reduziert) | (Reduziert, Voll) => Reduziert,
            (Transparenz::Voll, Transparenz::Voll) => Voll,
        }
    }

    pub fn alpha(self) -> f32 {
        match self {
            Transparenz::Minimal => 0.3,
            Transparenz::Reduziert => 0.6,
            Transparenz::Voll => 1.,
        }
    }
}

pub trait Zeichnen
where
    Self::AnchorPoints: verbindung::Lookup<Self::AnchorName>,
{
    /// Maximale x,y-Werte
    fn size(&self) -> Vektor;

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    /// Alle Pfade werden mit /canvas::FillRule::EvenOdd/ gefüllt.
    fn fülle(&self) -> Vec<(Pfad, Transparenz)>;

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self) -> Vec<Pfad>;

    /// Position, Beschreibung und Name (falls verfügbar)
    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>);

    /// Zeigt der /Vektor/ auf das Gleis?
    fn innerhalb(&self, relative_position: Vektor) -> bool;

    /// Identifier for AnchorPoints.
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type AnchorName;
    /// Speicher-Typ für /anchor::Verbindung/. Muss /anchor::Lookup<Self::AnchorName>/ implementieren.
    type AnchorPoints;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> Self::AnchorPoints;
}

pub trait MitName {
    fn name(&self) -> Option<&String>;
}
impl MitName for () {
    fn name(&self) -> Option<&String> {
        None
    }
}
impl<R, A> MitName for Option<Weiche<R, A>> {
    fn name(&self) -> Option<&String> {
        self.as_ref().map(|weiche| &weiche.name.0)
    }
}
impl<A> MitName for Option<Kontakt<A>> {
    fn name(&self) -> Option<&String> {
        self.as_ref().map(|kontakt| &kontakt.name.0)
    }
}

pub trait MitRichtung<Richtung> {
    fn aktuelle_richtung(&self) -> Option<&Richtung>;
}
impl<R> MitRichtung<R> for () {
    fn aktuelle_richtung(&self) -> Option<&R> {
        None
    }
}
impl<R, A> MitRichtung<R> for Option<Weiche<R, A>> {
    fn aktuelle_richtung(&self) -> Option<&R> {
        self.as_ref().map(|Weiche { aktuelle_richtung, .. }| aktuelle_richtung)
    }
}
