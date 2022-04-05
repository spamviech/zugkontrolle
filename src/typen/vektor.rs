//! Vektoren über [f32] mit allen Funktionen für einen 2-dimensionen Vektorraum.

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use crate::typen::{
    skalar::Skalar,
    winkel::{Trigonometrie, Winkel},
};

/// Vektoren über [Skalar] ([f32]) mit allen Funktionen für einen 2-dimensionen Vektorraum.
///
/// Addition zwischen Vektoren formen einen abelsche Gruppe
/// mit dem [null_vektor](Vektor::null_vektor) als neutrales Element.
///
/// Multiplikation mit einem [Skalar] befolgt Distributivgesetzte mit der Addition von Vektoren.
/// Multiplikation ist assoziativ mit Multiplikation zwischen zwei [Skalar].
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Vektor {
    /// x-Koordinate des Vektors.
    pub x: Skalar,
    /// y-Koordinate des Vektors.
    pub y: Skalar,
}

impl Vektor {
    /// Nullvektor.
    ///
    /// - additiv neutrales Element.
    /// - Resultat einer Multiplikation mit `Skalar(0)`.
    pub fn null_vektor() -> Self {
        Vektor { x: Skalar(0.), y: Skalar(0.) }
    }

    /// Einheitsvektor in x-Richtung.
    pub const EX: Vektor = Vektor { x: Skalar(1.), y: Skalar(0.) };

    /// Einheitsvektor in y-Richtung.
    pub const EY: Vektor = Vektor { x: Skalar(0.), y: Skalar(1.) };

    /// Erzeuge einen Vektor aus seinen Polarkoordinaten.
    ///
    /// Winkel wachsen im Uhrzeigersinn.
    /// y-Koordinaten wachsen nach unten.
    pub fn polar_koordinaten(radius: Skalar, winkel: Winkel) -> Self {
        Vektor { x: radius * winkel.cos(), y: radius * winkel.sin() }
    }

    /// Normalisiere den Vektor auf länge `Skalar(1.)`.
    pub fn normalisiere(&mut self) {
        *self /= self.länge();
    }

    /// Einheitsvektor mit identischer Richtung.
    pub fn einheitsvektor(mut self) -> Self {
        self.normalisiere();
        self
    }

    /// Skalarprodukt zweier Vektoren.
    ///
    /// Es gilt `self.skalarprodukt(other) == self.länge() * other.länge() * self.winkel(other).cos()`.
    /// Insbesondere gilt ´self.länge() == self.skalarprodukt(self).sqrt()`
    pub fn skalarprodukt(&self, other: &Self) -> Skalar {
        self.x * other.x + self.y * other.y
    }

    /// Länge eines Vektors (euklidische Metrik).
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn länge(&self) -> Skalar {
        Skalar(self.skalarprodukt(self).0.sqrt())
    }

    /// Winkel zwischen zwei Vektoren (im Uhrzeigersinn).
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn winkel(&self, other: &Self) -> Winkel {
        Winkel::acos(self.skalarprodukt(other) / (self.länge() * other.länge()))
    }

    /// Rotiere einen Vektor um `winkel` im Uhrzeigersinn.
    pub fn rotiere<T: Trigonometrie>(&mut self, winkel: T) {
        let Vektor { x, y } = *self;
        let cos = winkel.cos();
        let sin = winkel.sin();
        self.x = cos * x - sin * y;
        self.y = sin * x + cos * y;
    }

    /// Erzeuge einen Vektor, der um `winkel` im Uhrzeigersinn rotiert ist.
    pub fn rotiert<T: Trigonometrie>(mut self, winkel: T) -> Self {
        self.rotiere(winkel);
        self
    }
}

// Ein Vektor-Raum ist eine (additive) abelsche Gruppe
// Halbgruppe
impl AddAssign<&Self> for Vektor {
    fn add_assign(&mut self, rhs: &Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl AddAssign<&mut Self> for Vektor {
    fn add_assign(&mut self, rhs: &mut Self) {
        *self += &*rhs;
    }
}

impl AddAssign<Self> for Vektor {
    fn add_assign(&mut self, rhs: Self) {
        *self += &rhs;
    }
}

impl<T> Add<T> for Vektor
where
    Vektor: AddAssign<T>,
{
    type Output = Self;

    fn add(mut self, rhs: T) -> Self::Output {
        self += rhs;
        self
    }
}

impl<T> Add<T> for &Vektor
where
    Vektor: AddAssign<T>,
{
    type Output = Vektor;

    fn add(self, rhs: T) -> Self::Output {
        *self + rhs
    }
}

impl<T> Add<T> for &mut Vektor
where
    Vektor: AddAssign<T>,
{
    type Output = Vektor;

    fn add(self, rhs: T) -> Self::Output {
        &*self + rhs
    }
}

// Monoid
impl Default for Vektor {
    fn default() -> Self {
        Self::null_vektor()
    }
}

// inverses Element
impl Neg for Vektor {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.x = -self.x;
        self.y = -self.y;
        self
    }
}

impl SubAssign<Self> for Vektor {
    fn sub_assign(&mut self, rhs: Self) {
        *self += rhs.neg();
    }
}

impl SubAssign<&Self> for Vektor {
    fn sub_assign(&mut self, rhs: &Self) {
        *self -= rhs.clone();
    }
}

impl SubAssign<&mut Self> for Vektor {
    fn sub_assign(&mut self, rhs: &mut Self) {
        *self -= &*rhs;
    }
}

impl<T> Sub<T> for Vektor
where
    Vektor: SubAssign<T>,
{
    type Output = Self;

    fn sub(mut self, rhs: T) -> Self::Output {
        self -= rhs;
        self
    }
}

impl<T> Sub<T> for &Vektor
where
    Vektor: SubAssign<T>,
{
    type Output = Vektor;

    fn sub(self, rhs: T) -> Self::Output {
        *self - rhs
    }
}

impl<T> Sub<T> for &mut Vektor
where
    Vektor: SubAssign<T>,
{
    type Output = Vektor;

    fn sub(self, rhs: T) -> Self::Output {
        &*self - rhs
    }
}

// Multiplikation/Division mit Skalar
impl MulAssign<&Skalar> for Vektor {
    fn mul_assign(&mut self, rhs: &Skalar) {
        self.x *= rhs;
        self.y *= rhs;
    }
}

impl MulAssign<Skalar> for Vektor {
    fn mul_assign(&mut self, rhs: Skalar) {
        *self *= &rhs;
    }
}

impl MulAssign<&mut Skalar> for Vektor {
    fn mul_assign(&mut self, rhs: &mut Skalar) {
        *self *= &*rhs;
    }
}

impl<T> Mul<T> for Vektor
where
    Vektor: MulAssign<T>,
{
    type Output = Self;

    fn mul(mut self, rhs: T) -> Self::Output {
        self *= rhs;
        self
    }
}

impl Mul<Vektor> for &Skalar {
    type Output = Vektor;

    fn mul(self, rhs: Vektor) -> Self::Output {
        rhs * self
    }
}

impl Mul<Vektor> for Skalar {
    type Output = Vektor;

    fn mul(self, rhs: Vektor) -> Self::Output {
        rhs * self
    }
}

impl Mul<Vektor> for &mut Skalar {
    type Output = Vektor;

    fn mul(self, rhs: Vektor) -> Self::Output {
        rhs * self
    }
}

impl DivAssign<&Skalar> for Vektor {
    fn div_assign(&mut self, rhs: &Skalar) {
        self.x /= rhs;
        self.y /= rhs;
    }
}

impl DivAssign<Skalar> for Vektor {
    fn div_assign(&mut self, rhs: Skalar) {
        *self /= &rhs;
    }
}

impl DivAssign<&mut Skalar> for Vektor {
    fn div_assign(&mut self, rhs: &mut Skalar) {
        *self /= &*rhs;
    }
}

impl<T> Div<T> for Vektor
where
    Vektor: DivAssign<T>,
{
    type Output = Self;

    fn div(mut self, rhs: T) -> Self::Output {
        self /= rhs;
        self
    }
}

// copy+paste der Beispiel-Implementierung für IntegerPoint
impl rstar::Point for Vektor {
    type Scalar = f32;

    const DIMENSIONS: usize = 2;

    fn generate(mut generator: impl FnMut(usize) -> Self::Scalar) -> Self {
        Vektor { x: Skalar(generator(0)), y: Skalar(generator(1)) }
    }

    fn nth(&self, index: usize) -> Self::Scalar {
        match index {
            0 => self.x.0,
            1 => self.y.0,
            _ => unreachable!(),
        }
    }

    fn nth_mut(&mut self, index: usize) -> &mut Self::Scalar {
        match index {
            0 => &mut self.x.0,
            1 => &mut self.y.0,
            _ => unreachable!(),
        }
    }
}
