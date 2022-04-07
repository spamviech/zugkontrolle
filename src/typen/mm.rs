//! Typen mit `mm` Größen.

use std::ops::Div;

use serde::{Deserialize, Serialize};

use crate::typen::skalar::Skalar;

macro_rules! erstelle_neu_und_als_skalar {
    ($doc_neu: expr, $doc_als_skalar: expr $(,)?) => {
        #[doc = $doc_neu]
        pub const fn neu(radius: f32) -> Self {
            Self(radius)
        }

        #[doc = $doc_als_skalar]
        pub const fn als_skalar(self) -> Skalar {
            Skalar(self.0)
        }
    };
}

/// Spurweite \[mm\].
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Spurweite(f32);

// Abgeleitete Größe unter der Umrechnung von `mm` auf `Pixel`.
impl Spurweite {
    erstelle_neu_und_als_skalar! {
        "Erstelle einen neue [Spurweite].",
        "Abstand beider Schienen.",
    }

    /// Abstand seitlich der Schienen zum Anzeigen des Gleisendes.
    pub fn abstand(self) -> Skalar {
        self.als_skalar() / Skalar(3.)
    }

    /// Länge der Beschränkung (Spurweite + Abstand auf beiden Seiten).
    pub fn beschränkung(self) -> Skalar {
        self.als_skalar() + self.abstand().doppelt()
    }

    /// Innerster Radius (inklusive Beschränkung) einer Kurve.
    pub fn radius_begrenzung_innen(self, radius: Skalar) -> Skalar {
        radius - self.als_skalar().halbiert() - self.abstand()
    }

    /// Äußerster Radius (inklusive Beschränkung) einer Kurve.
    pub fn radius_begrenzung_außen(self, radius: Skalar) -> Skalar {
        radius + self.als_skalar().halbiert() + self.abstand()
    }
}

/// Längenmaß \[mm\].
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Länge(f32);

impl Länge {
    erstelle_neu_und_als_skalar! {
        "Erstelle eine neue [Länge].",
        "Länge auf einem Canvas.",
    }
}

impl Div<Länge> for Länge {
    type Output = f32;

    fn div(self, other: Länge) -> f32 {
        self.0 / other.0
    }
}

impl Div<Radius> for Länge {
    type Output = f32;

    fn div(self, other: Radius) -> f32 {
        self.0 / other.0
    }
}

/// Radius \[mm\].
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(f32);

impl Radius {
    erstelle_neu_und_als_skalar! {
        "Erstelle einen neuen [Radius].",
        "Radius auf einem Canvas.",
    }
}

impl Div<Radius> for Radius {
    type Output = f32;

    fn div(self, other: Radius) -> f32 {
        self.0 / other.0
    }
}

impl Div<Länge> for Radius {
    type Output = f32;

    fn div(self, other: Länge) -> f32 {
        self.0 / other.0
    }
}
