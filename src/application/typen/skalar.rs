//! Skalar-Werte für Größen auf dem Canvas

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use crate::application::typen::mm;

/// Skalar-Werte für Größen auf dem Canvas
///
/// Methoden bilden den bekannten arithmetischen Körper.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Skalar(pub f32);

impl Skalar {
    /// Doppelter Wert
    pub fn doppelt(&self) -> Self {
        Skalar(2.) * self
    }

    /// Halber Wert
    pub fn halbiert(&self) -> Self {
        Skalar(0.5) * self
    }

    /// Absoluter Wert
    pub fn abs(&self) -> Self {
        Skalar(self.0.abs())
    }

    /// Kopie des größeren Elements
    pub fn max(&self, other: &Self) -> Self {
        if self > other {
            *self
        } else {
            *other
        }
    }

    /// Kopie des kleineren Elements
    pub fn min(&self, other: &Self) -> Self {
        if self < other {
            *self
        } else {
            *other
        }
    }
}

macro_rules! impl_from_mm {
    (mm:: $type:ident) => {
        impl mm::$type {
            pub const fn als_skalar(self) -> Skalar {
                Skalar(self.0)
            }
        }
    };
}
impl_from_mm! {mm::Länge}
impl_from_mm! {mm::Radius}
impl_from_mm! {mm::Spurweite}

// Ein Körper ist eine (additive) abelsche Gruppe
// Halbgruppe
impl AddAssign<&Self> for Skalar {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs.0;
    }
}
impl AddAssign<&mut Self> for Skalar {
    fn add_assign(&mut self, rhs: &mut Self) {
        *self += &*rhs;
    }
}
impl AddAssign<Self> for Skalar {
    fn add_assign(&mut self, rhs: Self) {
        *self += &rhs;
    }
}
impl<T> Add<T> for Skalar
where
    Skalar: AddAssign<T>,
{
    type Output = Self;

    fn add(mut self, rhs: T) -> Self::Output {
        self += rhs;
        self
    }
}
// Monoid
impl Skalar {
    /// Additiv neutrales Element (0.)
    pub const fn additiv_neutral() -> Self {
        Skalar(0.)
    }
}
// inverses Element
impl Neg for Skalar {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.0 = -self.0;
        self
    }
}
impl SubAssign<Self> for Skalar {
    fn sub_assign(&mut self, rhs: Self) {
        *self += rhs.neg();
    }
}
impl SubAssign<&Self> for Skalar {
    fn sub_assign(&mut self, rhs: &Self) {
        *self -= rhs.clone();
    }
}
impl SubAssign<&mut Self> for Skalar {
    fn sub_assign(&mut self, rhs: &mut Self) {
        *self -= &*rhs;
    }
}
impl<T> Sub<T> for Skalar
where
    Skalar: SubAssign<T>,
{
    type Output = Self;

    fn sub(mut self, rhs: T) -> Self::Output {
        self -= rhs;
        self
    }
}

// Körper ohne additiv-neutrales Element (0) ist eine (multiplikative) abelsche Gruppe
// Halbgruppe
impl MulAssign<&Self> for Skalar {
    fn mul_assign(&mut self, rhs: &Self) {
        self.0 *= rhs.0;
    }
}
impl MulAssign<&mut Self> for Skalar {
    fn mul_assign(&mut self, rhs: &mut Self) {
        *self *= &*rhs;
    }
}
impl MulAssign<Self> for Skalar {
    fn mul_assign(&mut self, rhs: Self) {
        *self *= &rhs;
    }
}
impl<T> Mul<T> for Skalar
where
    Skalar: MulAssign<T>,
{
    type Output = Self;

    fn mul(mut self, rhs: T) -> Self::Output {
        self *= rhs;
        self
    }
}
// Monoid
impl Skalar {
    /// Multiplikativ neutrales Element (1.)
    pub const fn multiplikativ_neutral() -> Self {
        Skalar(1.)
    }
}
// Inverses Element (via division)
impl DivAssign<&Self> for Skalar {
    fn div_assign(&mut self, rhs: &Self) {
        *self *= Skalar(1. / rhs.0);
    }
}
impl DivAssign<&mut Self> for Skalar {
    fn div_assign(&mut self, rhs: &mut Self) {
        *self /= &*rhs;
    }
}
impl DivAssign<Self> for Skalar {
    fn div_assign(&mut self, rhs: Self) {
        *self /= &rhs;
    }
}
impl<T> Div<T> for Skalar
where
    Skalar: DivAssign<T>,
{
    type Output = Self;

    fn div(mut self, rhs: T) -> Self::Output {
        self /= rhs;
        self
    }
}
