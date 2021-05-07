//! Winkel in Bogen- und Gradmaß

use std::cmp::Ordering;
use std::convert::From;
use std::ops::{Add, AddAssign, Mul, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

/// Trigonometrische Funktionen (+ abs) für Winkel.
pub trait Trigonometrie {
    fn abs(&self) -> Self;
    fn cos(&self) -> f32;
    fn sin(&self) -> f32;
    fn tan(&self) -> f32;
    fn acos(input: f32) -> Self;
    fn asin(input: f32) -> Self;
    fn atan(input: f32) -> Self;
}

/// Winkel \[Bogenmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Serialize, Deserialize)]
pub struct Winkel(pub(crate) f32);

impl Winkel {
    pub const fn new(winkel: f32) -> Self {
        Winkel(winkel)
    }
}

// automatically implements Trait Into
impl From<AngleDegrees> for Winkel {
    fn from(AngleDegrees(f): AngleDegrees) -> Winkel {
        Winkel(f.to_radians())
    }
}
impl PartialEq<AngleDegrees> for Winkel {
    fn eq(&self, other: &AngleDegrees) -> bool {
        self.eq(&Winkel::from(*other))
    }
}
impl PartialOrd<AngleDegrees> for Winkel {
    fn partial_cmp(&self, other: &AngleDegrees) -> Option<Ordering> {
        self.partial_cmp(&Winkel::from(*other))
    }
}
impl AddAssign<Winkel> for Winkel {
    fn add_assign(&mut self, Winkel(other): Winkel) {
        self.0 += other
    }
}
impl Add<Winkel> for Winkel {
    type Output = Self;

    fn add(mut self, other: Winkel) -> Winkel {
        self += other;
        self
    }
}
impl AddAssign<AngleDegrees> for Winkel {
    fn add_assign(&mut self, AngleDegrees(other): AngleDegrees) {
        self.0 += other.to_radians()
    }
}
impl Add<AngleDegrees> for Winkel {
    type Output = Winkel;

    fn add(mut self, other: AngleDegrees) -> Winkel {
        self += other;
        self
    }
}
impl SubAssign<Winkel> for Winkel {
    fn sub_assign(&mut self, Winkel(other): Winkel) {
        self.0 -= other
    }
}
impl Sub<Winkel> for Winkel {
    type Output = Self;

    fn sub(mut self, other: Winkel) -> Winkel {
        self -= other;
        self
    }
}
impl SubAssign<AngleDegrees> for Winkel {
    fn sub_assign(&mut self, AngleDegrees(other): AngleDegrees) {
        self.0 -= other.to_radians()
    }
}
impl Sub<AngleDegrees> for Winkel {
    type Output = Winkel;

    fn sub(mut self, other: AngleDegrees) -> Winkel {
        self -= other;
        self
    }
}
impl Neg for Winkel {
    type Output = Self;

    fn neg(self) -> Self {
        Winkel(-self.0)
    }
}
impl Mul<f32> for Winkel {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        Winkel(other * self.0)
    }
}
impl Mul<Winkel> for f32 {
    type Output = Winkel;

    fn mul(self, Winkel(other): Winkel) -> Winkel {
        Winkel(self * other)
    }
}
impl Trigonometrie for Winkel {
    fn abs(&self) -> Winkel {
        Winkel(self.0.abs())
    }

    fn cos(&self) -> f32 {
        self.0.cos()
    }

    fn sin(&self) -> f32 {
        self.0.sin()
    }

    fn tan(&self) -> f32 {
        self.0.tan()
    }

    fn acos(input: f32) -> Self {
        Winkel(input.acos())
    }

    fn asin(input: f32) -> Self {
        Winkel(input.asin())
    }

    fn atan(input: f32) -> Self {
        Winkel(input.atan())
    }
}

/// Winkel \[Gradmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct AngleDegrees(f32);

impl AngleDegrees {
    pub const fn new(winkel: f32) -> Self {
        AngleDegrees(winkel)
    }
}

impl From<Winkel> for AngleDegrees {
    fn from(Winkel(f): Winkel) -> AngleDegrees {
        AngleDegrees(f.to_degrees())
    }
}
impl PartialEq<Winkel> for AngleDegrees {
    fn eq(&self, other: &Winkel) -> bool {
        Winkel::from(*self).eq(other)
    }
}
impl PartialOrd<Winkel> for AngleDegrees {
    fn partial_cmp(&self, other: &Winkel) -> Option<Ordering> {
        Winkel::from(*self).partial_cmp(other)
    }
}
impl Add<AngleDegrees> for AngleDegrees {
    type Output = Self;

    fn add(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self.0 + other)
    }
}
impl Add<Winkel> for AngleDegrees {
    type Output = Winkel;

    fn add(self, other: Winkel) -> Winkel {
        other + self
    }
}
impl Sub<AngleDegrees> for AngleDegrees {
    type Output = Self;

    fn sub(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self.0 - other)
    }
}
impl Sub<Winkel> for AngleDegrees {
    type Output = Winkel;

    fn sub(self, other: Winkel) -> Winkel {
        other - self
    }
}
impl Neg for AngleDegrees {
    type Output = Self;

    fn neg(self) -> Self {
        AngleDegrees(-self.0)
    }
}
impl Mul<f32> for AngleDegrees {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        AngleDegrees(other * self.0)
    }
}
impl Mul<AngleDegrees> for f32 {
    type Output = AngleDegrees;

    fn mul(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self * other)
    }
}
impl Trigonometrie for AngleDegrees {
    fn abs(&self) -> AngleDegrees {
        AngleDegrees(self.0.abs())
    }

    fn cos(&self) -> f32 {
        self.0.to_radians().cos()
    }

    fn sin(&self) -> f32 {
        self.0.to_radians().sin()
    }

    fn tan(&self) -> f32 {
        self.0.to_radians().tan()
    }

    fn acos(input: f32) -> Self {
        AngleDegrees(input.acos().to_degrees())
    }

    fn asin(input: f32) -> Self {
        AngleDegrees(input.asin().to_degrees())
    }

    fn atan(input: f32) -> Self {
        AngleDegrees(input.atan().to_degrees())
    }
}
