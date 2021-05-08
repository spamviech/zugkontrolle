//! Winkel in Bogen- und Gradmaß

use std::cmp::Ordering;
use std::convert::From;
use std::f32::consts;
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

/// τ = 2. * π, i.e. a full circle
pub const TAU: Winkel = Winkel(consts::TAU);
/// π, i.e. a half circle
pub const PI: Winkel = Winkel(consts::PI);
/// π / 2., i.e. a quarter circle
pub const FRAC_PI_2: Winkel = Winkel(consts::FRAC_PI_2);
/// 0
pub const ZERO: Winkel = Winkel(0.);

/// Winkel \[Bogenmaß\]
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Serialize, Deserialize)]
pub struct Winkel(pub(crate) f32);

impl Winkel {
    pub const fn new(winkel: f32) -> Self {
        Winkel(winkel)
    }
}

// automatically implements Trait Into
impl From<WinkelGradmaß> for Winkel {
    fn from(WinkelGradmaß(f): WinkelGradmaß) -> Winkel {
        Winkel(f.to_radians())
    }
}
impl PartialEq<WinkelGradmaß> for Winkel {
    fn eq(&self, other: &WinkelGradmaß) -> bool {
        self.eq(&Winkel::from(*other))
    }
}
impl PartialOrd<WinkelGradmaß> for Winkel {
    fn partial_cmp(&self, other: &WinkelGradmaß) -> Option<Ordering> {
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
impl AddAssign<WinkelGradmaß> for Winkel {
    fn add_assign(&mut self, WinkelGradmaß(other): WinkelGradmaß) {
        self.0 += other.to_radians()
    }
}
impl Add<WinkelGradmaß> for Winkel {
    type Output = Winkel;

    fn add(mut self, other: WinkelGradmaß) -> Winkel {
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
impl SubAssign<WinkelGradmaß> for Winkel {
    fn sub_assign(&mut self, WinkelGradmaß(other): WinkelGradmaß) {
        self.0 -= other.to_radians()
    }
}
impl Sub<WinkelGradmaß> for Winkel {
    type Output = Winkel;

    fn sub(mut self, other: WinkelGradmaß) -> Winkel {
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
pub struct WinkelGradmaß(f32);

impl WinkelGradmaß {
    pub const fn new(winkel: f32) -> Self {
        WinkelGradmaß(winkel)
    }
}

impl From<Winkel> for WinkelGradmaß {
    fn from(Winkel(f): Winkel) -> WinkelGradmaß {
        WinkelGradmaß(f.to_degrees())
    }
}
impl PartialEq<Winkel> for WinkelGradmaß {
    fn eq(&self, other: &Winkel) -> bool {
        Winkel::from(*self).eq(other)
    }
}
impl PartialOrd<Winkel> for WinkelGradmaß {
    fn partial_cmp(&self, other: &Winkel) -> Option<Ordering> {
        Winkel::from(*self).partial_cmp(other)
    }
}
impl Add<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn add(self, WinkelGradmaß(other): WinkelGradmaß) -> WinkelGradmaß {
        WinkelGradmaß(self.0 + other)
    }
}
impl Add<Winkel> for WinkelGradmaß {
    type Output = Winkel;

    fn add(self, other: Winkel) -> Winkel {
        other + self
    }
}
impl Sub<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn sub(self, WinkelGradmaß(other): WinkelGradmaß) -> WinkelGradmaß {
        WinkelGradmaß(self.0 - other)
    }
}
impl Sub<Winkel> for WinkelGradmaß {
    type Output = Winkel;

    fn sub(self, other: Winkel) -> Winkel {
        other - self
    }
}
impl Neg for WinkelGradmaß {
    type Output = Self;

    fn neg(self) -> Self {
        WinkelGradmaß(-self.0)
    }
}
impl Mul<f32> for WinkelGradmaß {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        WinkelGradmaß(other * self.0)
    }
}
impl Mul<WinkelGradmaß> for f32 {
    type Output = WinkelGradmaß;

    fn mul(self, WinkelGradmaß(other): WinkelGradmaß) -> WinkelGradmaß {
        WinkelGradmaß(self * other)
    }
}
impl Trigonometrie for WinkelGradmaß {
    fn abs(&self) -> WinkelGradmaß {
        WinkelGradmaß(self.0.abs())
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
        WinkelGradmaß(input.acos().to_degrees())
    }

    fn asin(input: f32) -> Self {
        WinkelGradmaß(input.asin().to_degrees())
    }

    fn atan(input: f32) -> Self {
        WinkelGradmaß(input.atan().to_degrees())
    }
}