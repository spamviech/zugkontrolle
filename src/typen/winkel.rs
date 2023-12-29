//! Winkel in Bogen- und Gradmaß.

use std::cmp::Ordering;
use std::convert::From;
use std::f32::consts;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use crate::typen::skalar::Skalar;

/// Trigonometrische Funktionen (+ abs) für Winkel.
pub trait Trigonometrie {
    /// Absoluter Wert.
    fn abs(&self) -> Self;
    /// Normalisiert in den äquivalenten Bereich zu [-π,π).
    fn normalisiert(self) -> Self;
    /// Kosinus
    fn cos(&self) -> Skalar;
    /// Sinus
    fn sin(&self) -> Skalar;
    /// Tangens
    fn tan(&self) -> Skalar;
    /// Inverser Kosinus
    fn acos(input: Skalar) -> Self;
    /// Inverser Sinus
    fn asin(input: Skalar) -> Self;
    /// Inverser Tangens
    fn atan(input: Skalar) -> Self;
}

/// τ = 2. * π, eine ganze Umdrehung.
pub const TAU: Winkel = Winkel(consts::TAU);

/// π, eine halbe Umdrehung.
pub const PI: Winkel = Winkel(consts::PI);

/// π / 2., eine viertel Umdrehung.
pub const FRAC_PI_2: Winkel = Winkel(consts::FRAC_PI_2);

/// 0
pub const ZERO: Winkel = Winkel(0.);

/// Winkel \[Bogenmaß\]
///
/// Die [PartialEq]- und [PartialOrd]-Instanzen sind abgeleitet und normalisieren die Winkel NICHT,
/// bevor sie verglichen werden.
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Serialize, Deserialize)]
pub struct Winkel(pub f32);

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

impl AddAssign<&Winkel> for Winkel {
    fn add_assign(&mut self, Winkel(other): &Winkel) {
        self.0 += other
    }
}

impl AddAssign<&WinkelGradmaß> for Winkel {
    fn add_assign(&mut self, WinkelGradmaß(other): &WinkelGradmaß) {
        self.0 += other.to_radians()
    }
}

impl AddAssign<Winkel> for Winkel {
    fn add_assign(&mut self, rhs: Winkel) {
        *self += &rhs
    }
}

impl AddAssign<WinkelGradmaß> for Winkel {
    fn add_assign(&mut self, rhs: WinkelGradmaß) {
        *self += &rhs
    }
}

#[allow(single_use_lifetimes)]
impl<T> AddAssign<&mut T> for Winkel
where
    Winkel: for<'s> AddAssign<&'s T>,
{
    fn add_assign(&mut self, rhs: &mut T) {
        *self += &*rhs
    }
}

impl<T> Add<T> for Winkel
where
    Winkel: AddAssign<T>,
{
    type Output = Self;

    fn add(mut self, other: T) -> Winkel {
        self += other;
        self
    }
}

impl SubAssign<&Winkel> for Winkel {
    fn sub_assign(&mut self, Winkel(other): &Winkel) {
        self.0 -= other
    }
}

impl SubAssign<&WinkelGradmaß> for Winkel {
    fn sub_assign(&mut self, WinkelGradmaß(other): &WinkelGradmaß) {
        self.0 -= other.to_radians()
    }
}

impl SubAssign<Winkel> for Winkel {
    fn sub_assign(&mut self, rhs: Winkel) {
        *self -= &rhs
    }
}

impl SubAssign<WinkelGradmaß> for Winkel {
    fn sub_assign(&mut self, rhs: WinkelGradmaß) {
        *self -= &rhs
    }
}
#[allow(single_use_lifetimes)]

impl<T> SubAssign<&mut T> for Winkel
where
    Winkel: for<'s> SubAssign<&'s T>,
{
    fn sub_assign(&mut self, rhs: &mut T) {
        *self -= &*rhs
    }
}

impl<T> Sub<T> for Winkel
where
    Winkel: SubAssign<T>,
{
    type Output = Self;

    fn sub(mut self, other: T) -> Self::Output {
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

impl MulAssign<f32> for Winkel {
    fn mul_assign(&mut self, rhs: f32) {
        self.0 *= rhs
    }
}

impl Mul<f32> for Winkel {
    type Output = Self;

    fn mul(mut self, other: f32) -> Self {
        self *= other;
        self
    }
}

impl Mul<Winkel> for f32 {
    type Output = Winkel;

    fn mul(self, other: Winkel) -> Winkel {
        other * self
    }
}

impl DivAssign<f32> for Winkel {
    fn div_assign(&mut self, rhs: f32) {
        self.0 /= rhs;
    }
}

impl Div<f32> for Winkel {
    type Output = Self;

    fn div(mut self, other: f32) -> Self {
        self /= other;
        self
    }
}

impl Div<Winkel> for f32 {
    type Output = Winkel;

    fn div(self, other: Winkel) -> Winkel {
        other / self
    }
}

impl Trigonometrie for Winkel {
    fn abs(&self) -> Winkel {
        Winkel(self.0.abs())
    }

    fn normalisiert(mut self) -> Self {
        while self < -PI {
            self += TAU
        }
        while self >= PI {
            self -= TAU
        }
        self
    }

    fn cos(&self) -> Skalar {
        Skalar(self.0.cos())
    }

    fn sin(&self) -> Skalar {
        Skalar(self.0.sin())
    }

    fn tan(&self) -> Skalar {
        Skalar(self.0.tan())
    }

    fn acos(input: Skalar) -> Self {
        Winkel(input.0.acos())
    }

    fn asin(input: Skalar) -> Self {
        Winkel(input.0.asin())
    }

    fn atan(input: Skalar) -> Self {
        Winkel(input.0.atan())
    }
}

/// Winkel \[Gradmaß\].
///
/// Die [PartialEq]- und [PartialOrd]-Instanzen sind abgeleitet und normalisieren die Winkel NICHT,
/// bevor sie verglichen werden.
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct WinkelGradmaß(f32);

impl WinkelGradmaß {
    /// Konstruktor
    pub const fn neu(grad: f32) -> Self {
        WinkelGradmaß(grad)
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

impl AddAssign<WinkelGradmaß> for WinkelGradmaß {
    fn add_assign(&mut self, rhs: WinkelGradmaß) {
        self.0 += rhs.0
    }
}

impl Add<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn add(mut self, other: WinkelGradmaß) -> WinkelGradmaß {
        self += other;
        self
    }
}

impl Add<Winkel> for WinkelGradmaß {
    type Output = Winkel;

    fn add(self, other: Winkel) -> Winkel {
        other + self
    }
}

impl SubAssign<WinkelGradmaß> for WinkelGradmaß {
    fn sub_assign(&mut self, rhs: WinkelGradmaß) {
        self.0 -= rhs.0
    }
}

impl Sub<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn sub(mut self, other: WinkelGradmaß) -> WinkelGradmaß {
        self -= other;
        self
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

    fn normalisiert(mut self) -> Self {
        while self < -WinkelGradmaß(180.) {
            self += WinkelGradmaß(360.)
        }
        while self >= WinkelGradmaß(180.) {
            self -= WinkelGradmaß(360.)
        }
        self
    }

    fn cos(&self) -> Skalar {
        Skalar(self.0.to_radians().cos())
    }

    fn sin(&self) -> Skalar {
        Skalar(self.0.to_radians().sin())
    }

    fn tan(&self) -> Skalar {
        Skalar(self.0.to_radians().tan())
    }

    fn acos(input: Skalar) -> Self {
        WinkelGradmaß(input.0.acos().to_degrees())
    }

    fn asin(input: Skalar) -> Self {
        WinkelGradmaß(input.0.asin().to_degrees())
    }

    fn atan(input: Skalar) -> Self {
        WinkelGradmaß(input.0.atan().to_degrees())
    }
}
