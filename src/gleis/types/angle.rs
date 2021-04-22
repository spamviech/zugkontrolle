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
pub struct Angle(pub(crate) f32);

impl Angle {
    pub const fn new(angle: f32) -> Self {
        Angle(angle)
    }
}

// automatically implements Trait Into
impl From<AngleDegrees> for Angle {
    fn from(AngleDegrees(f): AngleDegrees) -> Angle {
        Angle(f.to_radians())
    }
}
impl PartialEq<AngleDegrees> for Angle {
    fn eq(&self, other: &AngleDegrees) -> bool {
        self.eq(&Angle::from(*other))
    }
}
impl PartialOrd<AngleDegrees> for Angle {
    fn partial_cmp(&self, other: &AngleDegrees) -> Option<Ordering> {
        self.partial_cmp(&Angle::from(*other))
    }
}
impl AddAssign<Angle> for Angle {
    fn add_assign(&mut self, Angle(other): Angle) {
        self.0 += other
    }
}
impl Add<Angle> for Angle {
    type Output = Self;
    fn add(mut self, other: Angle) -> Angle {
        self += other;
        self
    }
}
impl AddAssign<AngleDegrees> for Angle {
    fn add_assign(&mut self, AngleDegrees(other): AngleDegrees) {
        self.0 += other.to_radians()
    }
}
impl Add<AngleDegrees> for Angle {
    type Output = Angle;
    fn add(mut self, other: AngleDegrees) -> Angle {
        self += other;
        self
    }
}
impl SubAssign<Angle> for Angle {
    fn sub_assign(&mut self, Angle(other): Angle) {
        self.0 -= other
    }
}
impl Sub<Angle> for Angle {
    type Output = Self;
    fn sub(mut self, other: Angle) -> Angle {
        self -= other;
        self
    }
}
impl SubAssign<AngleDegrees> for Angle {
    fn sub_assign(&mut self, AngleDegrees(other): AngleDegrees) {
        self.0 -= other.to_radians()
    }
}
impl Sub<AngleDegrees> for Angle {
    type Output = Angle;
    fn sub(mut self, other: AngleDegrees) -> Angle {
        self -= other;
        self
    }
}
impl Neg for Angle {
    type Output = Self;
    fn neg(self) -> Self {
        Angle(-self.0)
    }
}
impl Mul<f32> for Angle {
    type Output = Self;
    fn mul(self, other: f32) -> Self {
        Angle(other * self.0)
    }
}
impl Mul<Angle> for f32 {
    type Output = Angle;
    fn mul(self, Angle(other): Angle) -> Angle {
        Angle(self * other)
    }
}
impl Trigonometrie for Angle {
    fn abs(&self) -> Angle {
        Angle(self.0.abs())
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
        Angle(input.acos())
    }
    fn asin(input: f32) -> Self {
        Angle(input.asin())
    }
    fn atan(input: f32) -> Self {
        Angle(input.atan())
    }
}

/// Winkel \[Gradmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct AngleDegrees(f32);

impl AngleDegrees {
    pub const fn new(angle: f32) -> Self {
        AngleDegrees(angle)
    }
}

impl From<Angle> for AngleDegrees {
    fn from(Angle(f): Angle) -> AngleDegrees {
        AngleDegrees(f.to_degrees())
    }
}
impl PartialEq<Angle> for AngleDegrees {
    fn eq(&self, other: &Angle) -> bool {
        Angle::from(*self).eq(other)
    }
}
impl PartialOrd<Angle> for AngleDegrees {
    fn partial_cmp(&self, other: &Angle) -> Option<Ordering> {
        Angle::from(*self).partial_cmp(other)
    }
}
impl Add<AngleDegrees> for AngleDegrees {
    type Output = Self;
    fn add(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self.0 + other)
    }
}
impl Add<Angle> for AngleDegrees {
    type Output = Angle;
    fn add(self, other: Angle) -> Angle {
        other + self
    }
}
impl Sub<AngleDegrees> for AngleDegrees {
    type Output = Self;
    fn sub(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self.0 - other)
    }
}
impl Sub<Angle> for AngleDegrees {
    type Output = Angle;
    fn sub(self, other: Angle) -> Angle {
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
