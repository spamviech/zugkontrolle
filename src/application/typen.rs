//! newtypes auf f32, um zwischen mm-basierten und Pixel-basierten Größen zu unterscheiden

use crate::{
    application::gleis::verbindung,
    lookup::Lookup,
    steuerung::{
        kontakt::{Kontakt, KontaktSerialisiert},
        weiche::Weiche,
    },
};

// re-exports
pub use self::{
    canvas::{pfad, Bogen, Cache, Frame, Pfad, Position, Transformation},
    mm::*,
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    winkel::{Trigonometrie, Winkel, WinkelGradmaß},
};
pub use crate::zugtyp::Zugtyp;

pub mod canvas;
pub mod mm;
pub mod rechteck;
pub mod skalar;
pub mod vektor;
pub mod winkel;

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
    Self::Verbindungen: verbindung::Lookup<Self::VerbindungName>,
{
    /// Einschließendes Rechteck bei Position `(0,0)`.
    fn rechteck(&self) -> Rechteck;

    /// Einschließendes Rechteck, wenn sich das Gleis an der `Position` befindet.
    fn rechteck_an_position(&self, position: &Position) -> Rechteck {
        let mut rechteck = self.rechteck();
        rechteck.respektiere_rotation(&position.winkel);
        rechteck.verschiebe(&position.punkt);
        rechteck
    }

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    /// Alle Pfade werden mit `canvas::FillRule::EvenOdd` gefüllt.
    fn fülle(&self) -> Vec<(Pfad, Transparenz)>;

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self) -> Vec<Pfad>;

    /// Position für, sowie Beschreibung und Name (falls verfügbar).
    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>);

    /// Zeigt der `Vektor` auf das Gleis, die angegebene Klick-`ungenauigkeit` berücksichtigend?
    fn innerhalb(&self, relative_position: Vektor, ungenauigkeit: Skalar) -> bool;

    /// Identifier for `Verbindungen`.
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type VerbindungName;
    /// Speicher-Typ für `verbindung::Verbindung`.
    /// Muss `verbindung::Lookup<Self::VerbindungName>` implementieren.
    type Verbindungen;
    /// Verbindungen (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei `(0,0)`, Richtung nach außen zeigend.
    /// Es wird erwartet, dass sich die Verbindungen innerhalb von `rechteck` befinden.
    fn verbindungen(&self) -> Self::Verbindungen;

    /// Absolute Position der Verbindungen, wenn sich das Gleis an der `Position` befindet.
    fn verbindungen_an_position(&self, position: Position) -> Self::Verbindungen {
        self.verbindungen().map(
            |&verbindung::Verbindung { position: verbindung_position, richtung }| {
                verbindung::Verbindung {
                    position: position.transformation(verbindung_position),
                    richtung: position.winkel + richtung,
                }
            },
        )
    }
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
impl MitName for Option<Kontakt> {
    fn name(&self) -> Option<&String> {
        self.as_ref().map(|kontakt| &kontakt.name.0)
    }
}
impl MitName for Option<KontaktSerialisiert> {
    fn name(&self) -> Option<&String> {
        self.as_ref().map(|kontakt| &kontakt.name.0)
    }
}

pub trait MitRichtung<Richtung> {
    fn aktuelle_richtung(&self) -> Option<Richtung>;
}
impl<R> MitRichtung<R> for () {
    fn aktuelle_richtung(&self) -> Option<R> {
        None
    }
}
impl<R, T: MitRichtung<R>> MitRichtung<R> for Option<T> {
    fn aktuelle_richtung(&self) -> Option<R> {
        self.as_ref().and_then(|t| t.aktuelle_richtung())
    }
}
impl<R: Clone, A> MitRichtung<R> for Weiche<R, A> {
    fn aktuelle_richtung(&self) -> Option<R> {
        Some(self.aktuelle_richtung.clone())
    }
}
