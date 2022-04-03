//! Typen mit `mm` Größen.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::ops::Div;

use serde::{Deserialize, Serialize};

use crate::typen::skalar::Skalar;

/// Spurweite \[mm\].
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Spurweite(pub f32);

// Abgeleitete Größe unter der Umrechnung von /mm/ auf /Pixel/.
impl Spurweite {
    /// Abstand beider Schienen
    pub fn spurweite(&self) -> Skalar {
        Skalar(self.0)
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

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Länge(pub(crate) f32);
impl Länge {
    pub const fn neu(länge: f32) -> Self {
        Länge(länge)
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

/// Radius \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(pub(crate) f32);
impl Radius {
    pub const fn neu(radius: f32) -> Self {
        Radius(radius)
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
