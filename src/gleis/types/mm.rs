//! Typen mit /mm/ Größen

use std::ops::Div;

// re-export
pub use crate::zugtyp::Spurweite;

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(pub(crate) f32);
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
pub struct Radius(pub(crate) f32);
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
