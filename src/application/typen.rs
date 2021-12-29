//! newtypes auf f32, um zwischen mm-basierten und Pixel-basierten Größen zu unterscheiden

use crate::{
    application::gleis::verbindung::{self, Verbindung},
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

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);

// abgeleitete Größe unter der Umrechnung von /mm/ auf /Pixel/
impl Spurweite {
    /// Abstand beider Schienen
    pub fn spurweite(&self) -> Skalar {
        self.spurweite.als_skalar()
    }
    /// Abstand seitlich der Schienen zum Anzeigen des Gleisendes
    pub fn abstand(&self) -> Skalar {
        self.spurweite() / Skalar(3.)
    }
    /// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten)
    pub fn beschränkung(&self) -> Skalar {
        self.spurweite() + self.abstand().doppelt()
    }
    /// Innerster Radius (inklusive Beschränkung) einer Kurve
    pub fn radius_begrenzung_innen(&self, radius: Skalar) -> Skalar {
        radius - self.spurweite().halbiert() - self.abstand()
    }
    /// Äußerster Radius (inklusive Beschränkung) einer Kurve
    pub fn radius_begrenzung_außen(&self, radius: Skalar) -> Skalar {
        radius + self.spurweite().halbiert() + self.abstand()
    }
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
            (Voll, Voll) => Voll,
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

pub trait Zeichnen {
    /// Einschließendes Rechteck bei Position `(0,0)`.
    fn rechteck(&self, spurweite: Spurweite) -> Rechteck;

    /// Einschließendes Rechteck, wenn sich das Gleis an der `Position` befindet.
    fn rechteck_an_position(&self, spurweite: Spurweite, position: &Position) -> Rechteck {
        self.rechteck(spurweite)
            .respektiere_rotation_chain(&position.winkel)
            .verschiebe_chain(&position.punkt)
    }

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    /// Alle Pfade werden mit `canvas::FillRule::EvenOdd` gefüllt.
    fn fülle(&self, spurweite: Spurweite) -> Vec<(Pfad, Transparenz)>;

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self, spurweite: Spurweite) -> Vec<Pfad>;

    /// Position für, sowie Beschreibung und Name (falls verfügbar).
    fn beschreibung_und_name(
        &self,
        spurweite: Spurweite,
    ) -> (Position, Option<&String>, Option<&String>);

    /// Zeigt der `Vektor` auf das Gleis, die angegebene Klick-`ungenauigkeit` berücksichtigend?
    fn innerhalb(
        &self,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> bool;

    /// Identifier for `Verbindungen`.
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type VerbindungName;
    /// Speicher-Typ für `Verbindung`.
    /// Muss `verbindung::Lookup<Self::VerbindungName>` implementieren.
    type Verbindungen: verbindung::Lookup<Self::VerbindungName>;
    /// Verbindungen (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei `(0,0)`, Richtung nach außen zeigend.
    /// Es wird erwartet, dass sich die Verbindungen innerhalb von `rechteck` befinden.
    fn verbindungen(&self, spurweite: Spurweite) -> Self::Verbindungen;

    /// Absolute Position der Verbindungen, wenn sich das Gleis an der `Position` befindet.
    fn verbindungen_an_position(
        &self,
        spurweite: Spurweite,
        position: Position,
    ) -> Self::Verbindungen {
        self.verbindungen(spurweite).map(
            |&Verbindung { position: verbindung_position, richtung }| Verbindung {
                position: position.transformation(verbindung_position),
                richtung: position.winkel + richtung,
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
