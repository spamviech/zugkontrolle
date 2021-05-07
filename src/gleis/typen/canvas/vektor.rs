//! Vektoren über /f32/ mit allen Funktionen für einen 2-dimensionen Vektorraum

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use crate::gleis::typen::winkel::{Trigonometrie, Winkel};

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
/// Vektoren über `f32` mit allen Funktionen für einen 2-dimensionen Vektorraum
///
/// Addition zwischen Vektoren formen einen abelsche Gruppe
/// mit dem `Vektor::null_vektor` als neutrales Element.
///
/// Multiplikation mit einem `f32` befolgt Distributivgesetzte mit der Addition von Vektoren.
/// Multiplikation ist assoziativ mit Multiplikation zwischen zwei `f32`.
pub struct Vektor {
    pub x: f32,
    pub y: f32,
}

impl Vektor {
    /// Nullvektor
    ///
    /// - additiv neutrales Element
    /// - Resultat einer Multiplikation mit 0.
    pub fn null_vektor() -> Self {
        Vektor { x: 0., y: 0. }
    }

    /// Einheitsvektor mit identischer Richtung
    pub fn einheitsvektor(&self) -> Self {
        self.clone() / self.länge()
    }

    /// Skalarprodukt zweier Vektoren
    ///
    /// Es gilt `self.skalarprodukt(other) == self.länge() * other.länge() *
    /// self.winkel(other).cos()`. Insbesondere gilt ´self.länge() ==
    /// self.skalarprodukt(self).sqrt()`
    pub fn skalarprodukt(&self, other: &Self) -> f32 {
        self.x * other.x + self.y * other.y
    }

    /// Länge eines Vektors (euklidische Metrik)
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn länge(&self) -> f32 {
        self.skalarprodukt(self).sqrt()
    }

    /// Winkel zwischen zwei Vektoren (im Uhrzeigersinn)
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn winkel(&self, other: &Self) -> Winkel {
        Winkel::acos(-self.skalarprodukt(other) / (self.länge() * other.länge()))
    }

    /// Erzeuge einen Vektor, der um /winkel/ im Uhrzeigersinn rotiert ist
    pub fn rotiere<T: Trigonometrie>(&self, winkel: T) -> Self {
        // https://de.wikipedia.org/wiki/Drehmatrix#Drehmatrix_der_Ebene_%E2%84%9D%C2%B2
        // geht von Drehung gegen den Uhrzeigersinn und nach oben steigender y-Achse aus
        Vektor {
            x: winkel.cos() * self.x - winkel.sin() * self.y,
            y: winkel.sin() * self.x + winkel.cos() * self.y,
        }
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

// Multiplikation/Division mit Skalar
impl MulAssign<f32> for Vektor {
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
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
impl Mul<Vektor> for f32 {
    type Output = Vektor;

    fn mul(self, rhs: Vektor) -> Self::Output {
        rhs * self
    }
}
impl DivAssign<f32> for Vektor {
    fn div_assign(&mut self, rhs: f32) {
        *self *= 1. / rhs;
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
