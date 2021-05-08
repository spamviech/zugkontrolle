//! Typen mit /mm/ Größen

use std::ops::Div;

// re-export
pub use crate::zugtyp::Spurweite;

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
