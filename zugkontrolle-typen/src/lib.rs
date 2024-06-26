//! Abstrakte Beschreibungen für z.B. Koordinaten und andere Anzeige-relevanten Parameter.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use crate::{
    canvas::{pfad::Pfad, Position},
    farbe::Farbe,
    mm::Spurweite,
    nachschlagen::Nachschlagen,
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    verbindung::Verbindung,
};

pub mod canvas;
pub mod farbe;
pub mod klick_quelle;
pub mod mm;
pub mod nachschlagen;
pub mod rechteck;
pub mod skalar;
pub mod vektor;
pub mod verbindung;
pub mod winkel;

/// Wird ein Pfad mit voller oder reduzierter Transparenz gefüllt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Transparenz {
    /// Der Pfad wird mit voller Stärke gezeichnet.
    Voll,
    /// Der Pfad wird leicht transparent gezeichnet.
    Reduziert,
    /// Der Pfad mit stark transparent gezeichnet.
    Minimal,
}

impl Transparenz {
    /// [Reduzierte](Transparenz::Reduziert), wenn das Argument [`true`] ist,
    /// ansonsten [`Volle`](Transparenz::Voll) Transparenz.
    #[must_use]
    pub fn true_reduziert(input: bool) -> Transparenz {
        if input {
            Transparenz::Reduziert
        } else {
            Transparenz::Voll
        }
    }

    /// Kombiniere zwei Transparenz-Werte.
    #[must_use]
    pub fn kombiniere(self, other: Transparenz) -> Transparenz {
        use Transparenz::{Minimal, Reduziert, Voll};
        match (self, other) {
            (Minimal, _) | (_, Minimal) | (Reduziert, Reduziert) => Minimal,
            (Voll, Reduziert) | (Reduziert, Voll) => Reduziert,
            (Voll, Voll) => Voll,
        }
    }

    /// Erhalte den assoziierten Wert für den Alpha-Kanal.
    #[must_use]
    pub fn alpha(self) -> f32 {
        match self {
            Transparenz::Minimal => 0.3,
            Transparenz::Reduziert => 0.6,
            Transparenz::Voll => 1.,
        }
    }
}

/// Rückgabewert von [`Zeichnen::innerhalb`].
#[derive(Debug, Clone, Copy)]
pub enum Innerhalb {
    /// Der [`Vektor`] ist innerhalb des Gleises.
    Innerhalb,
    /// Der [`Vektor`] ist unter Berücksichtigung der Ungenauigkeit innerhalb des Gleises.
    Toleranz,
    /// Der [`Vektor`] ist außerhalb des Gleises.
    Außerhalb,
}

impl Innerhalb {
    /// Kombiniere zwei [`Innerhalb`], wenn beide Bedingungen erfüllt sein müssen.
    #[must_use]
    pub fn und(self, anderes: Innerhalb) -> Innerhalb {
        match (self, anderes) {
            (Innerhalb::Innerhalb, mit_innerhalb) | (mit_innerhalb, Innerhalb::Innerhalb) => {
                mit_innerhalb
            },
            (Innerhalb::Außerhalb, _) | (_, Innerhalb::Außerhalb) => Innerhalb::Außerhalb,
            (Innerhalb::Toleranz, Innerhalb::Toleranz) => Innerhalb::Toleranz,
        }
    }

    /// Kombiniere zwei [`Innerhalb`], wenn nur eine beliebige der Bedingungen erfüllt sein muss.
    #[must_use]
    pub fn oder(self, anderes: Innerhalb) -> Innerhalb {
        match (self, anderes) {
            (Innerhalb::Außerhalb, mit_außerhalb) | (mit_außerhalb, Innerhalb::Außerhalb) => {
                mit_außerhalb
            },
            (Innerhalb::Innerhalb, _) | (_, Innerhalb::Innerhalb) => Innerhalb::Innerhalb,
            (Innerhalb::Toleranz, Innerhalb::Toleranz) => Innerhalb::Toleranz,
        }
    }
}

/// Trait für Typen, die auf einem [`Frame`](crate::typen::canvas::Frame) gezeichnet werden können.
///
/// Die Darstellungs-Reihenfolge ist [fülle](Zeichnen::fülle), [`zeichne`](Zeichnen::zeichne),
/// [`beschreibung_und_name`](Zeichnen::beschreibung_und_name).
pub trait Zeichnen<T> {
    /// Einschließendes Rechteck bei Position `(0,0)`.
    fn rechteck(&self, t: &T, spurweite: Spurweite) -> Rechteck;

    // t: T
    #[allow(clippy::min_ident_chars)]
    /// Einschließendes Rechteck, wenn sich das Gleis an der [`Position`] befindet.
    fn rechteck_an_position(&self, t: &T, spurweite: Spurweite, position: &Position) -> Rechteck {
        self.rechteck(t, spurweite)
            .respektiere_rotation_chain(&position.winkel)
            .verschiebe_chain(&position.punkt)
    }

    /// Erzeuge die Pfade für Darstellung der Linien.
    fn zeichne(&self, t: &T, spurweite: Spurweite) -> Vec<Pfad>;

    /// Erzeuge die Pfade für Färben des Hintergrunds.
    ///
    /// Alle Pfade werden mit [`fill::Rule::EvenOdd`](iced::widget::canvas::fill::Rule::EvenOdd) gefüllt.
    ///
    /// Wenn ein Pfad ohne Farbe zurückgegeben wird, wird die Farbe des
    /// [`Streckenabschnitts`](crate::steuerung::streckenabschnitt::Streckenabschnitt) verwendet.
    fn fülle(&self, t: &T, spurweite: Spurweite) -> Vec<(Pfad, Option<Farbe>, Transparenz)>;

    /// [`Position`] und Text für Beschreibung und Name (falls verfügbar).
    fn beschreibung_und_name<'s, 't>(
        &'s self,
        t: &'t T,
        spurweite: Spurweite,
    ) -> (Position, Option<&'s str>, Option<&'t str>);

    /// Zeigt der [`Vektor`] auf das Gleis, die angegebene Klick-`ungenauigkeit` berücksichtigend?
    fn innerhalb(
        &self,
        t: &T,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> Innerhalb;

    /// Identifier for [`Self::Verbindungen`](Zeichnen::Verbindungen).
    /// Ein enum wird empfohlen, aber andere Typen funktionieren ebenfalls.
    type VerbindungName;

    /// Speicher-Typ für [`Verbindung`].
    /// Muss [`verbindung::Nachschlagen<Self::VerbindungName>`] implementieren.
    type Verbindungen: verbindung::Nachschlagen<Self::VerbindungName>;

    /// Verbindungen (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei `(0,0)`, Richtung nach außen zeigend.
    /// Es wird erwartet, dass sich die Verbindungen innerhalb von `rechteck` befinden.
    fn verbindungen(&self, t: &T, spurweite: Spurweite) -> Self::Verbindungen;

    // t: T
    #[allow(clippy::min_ident_chars)]
    /// Absolute Position der Verbindungen, wenn sich das Gleis an der [`Position`] befindet.
    fn verbindungen_an_position(
        &self,
        t: &T,
        spurweite: Spurweite,
        position: Position,
    ) -> Self::Verbindungen {
        self.verbindungen(t, spurweite).zuordnen(
            |&Verbindung { position: verbindung_position, richtung }| {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                let richtung = position.winkel + richtung;
                Verbindung { position: position.transformation(verbindung_position), richtung }
            },
        )
    }
}

/// Trait für (potentiell) benannte Typen.
pub trait MitName {
    /// Der Name des Wertes.
    fn name(&self) -> Option<&str>;
}

impl MitName for () {
    fn name(&self) -> Option<&str> {
        None
    }
}

impl<T: MitName> MitName for Option<T> {
    fn name(&self) -> Option<&str> {
        self.as_ref().and_then(MitName::name)
    }
}
