use std::cmp::Ordering;
use std::convert::From;
use std::ops::{Add, Mul, Neg, Sub};

/// Trigonometrische Funktionen (+ abs) für Winkel.
pub trait Trigonometrie {
    fn abs(&self) -> Self;
    fn cos(&self) -> f64;
    fn sin(&self) -> f64;
    fn tan(&self) -> f64;
    fn acos(input: f64) -> Self;
    fn asin(input: f64) -> Self;
    fn atan(input: f64) -> Self;
}

/// Winkel \[Bogenmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct Angle(pub(crate) f64);

impl Angle {
    pub const fn new(angle: f64) -> Self {
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
impl Add<Angle> for Angle {
    type Output = Self;
    fn add(self, Angle(other): Angle) -> Angle {
        Angle(self.0 + other)
    }
}
impl Add<AngleDegrees> for Angle {
    type Output = Angle;
    fn add(self, AngleDegrees(other): AngleDegrees) -> Angle {
        Angle(self.0 + other.to_radians())
    }
}
impl Sub<Angle> for Angle {
    type Output = Self;
    fn sub(self, Angle(other): Angle) -> Angle {
        Angle(self.0 - other)
    }
}
impl Sub<AngleDegrees> for Angle {
    type Output = Angle;
    fn sub(self, AngleDegrees(other): AngleDegrees) -> Angle {
        Angle(self.0 - other.to_radians())
    }
}
impl Neg for Angle {
    type Output = Self;
    fn neg(self) -> Self {
        Angle(-self.0)
    }
}
impl Mul<f64> for Angle {
    type Output = Self;
    fn mul(self, other: f64) -> Self {
        Angle(other * self.0)
    }
}
impl Mul<Angle> for f64 {
    type Output = Angle;
    fn mul(self, Angle(other): Angle) -> Angle {
        Angle(self * other)
    }
}
impl Trigonometrie for Angle {
    fn abs(&self) -> Angle {
        Angle(self.0.abs())
    }
    fn cos(&self) -> f64 {
        self.0.cos()
    }
    fn sin(&self) -> f64 {
        self.0.sin()
    }
    fn tan(&self) -> f64 {
        self.0.tan()
    }
    fn acos(input: f64) -> Self {
        Angle(input.acos())
    }
    fn asin(input: f64) -> Self {
        Angle(input.asin())
    }
    fn atan(input: f64) -> Self {
        Angle(input.atan())
    }
}

/// Winkel \[Gradmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct AngleDegrees(f64);

impl AngleDegrees {
    pub const fn new(angle: f64) -> Self {
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
    fn add(self, Angle(other): Angle) -> Angle {
        Angle(self.0.to_radians() + other)
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
    fn sub(self, Angle(other): Angle) -> Angle {
        Angle(self.0.to_radians() - other)
    }
}
impl Neg for AngleDegrees {
    type Output = Self;
    fn neg(self) -> Self {
        AngleDegrees(-self.0)
    }
}
impl Mul<f64> for AngleDegrees {
    type Output = Self;
    fn mul(self, other: f64) -> Self {
        AngleDegrees(other * self.0)
    }
}
impl Mul<AngleDegrees> for f64 {
    type Output = AngleDegrees;
    fn mul(self, AngleDegrees(other): AngleDegrees) -> AngleDegrees {
        AngleDegrees(self * other)
    }
}
impl Trigonometrie for AngleDegrees {
    fn abs(&self) -> AngleDegrees {
        AngleDegrees(self.0.abs())
    }
    fn cos(&self) -> f64 {
        self.0.to_radians().cos()
    }
    fn sin(&self) -> f64 {
        self.0.to_radians().sin()
    }
    fn tan(&self) -> f64 {
        self.0.to_radians().tan()
    }
    fn acos(input: f64) -> Self {
        AngleDegrees(input.acos().to_degrees())
    }
    fn asin(input: f64) -> Self {
        AngleDegrees(input.asin().to_degrees())
    }
    fn atan(input: f64) -> Self {
        AngleDegrees(input.atan().to_degrees())
    }
}
