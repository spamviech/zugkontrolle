//! Vektoren über /f32/ mit allen Funktionen für einen 2-dimensionen Vektorraum

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use super::skalar::Skalar;
use super::Position;
use crate::gleis::typen::winkel::{Trigonometrie, Winkel};

/// Vektoren über `f32` mit allen Funktionen für einen 2-dimensionen Vektorraum
///
/// Addition zwischen Vektoren formen einen abelsche Gruppe
/// mit dem `Vektor::null_vektor` als neutrales Element.
///
/// Multiplikation mit einem `f32` befolgt Distributivgesetzte mit der Addition von Vektoren.
/// Multiplikation ist assoziativ mit Multiplikation zwischen zwei `f32`.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Vektor {
    pub x: Skalar,
    pub y: Skalar,
}

impl Vektor {
    /// Nullvektor
    ///
    /// - additiv neutrales Element
    /// - Resultat einer Multiplikation mit 0.
    pub fn null_vektor() -> Self {
        Vektor { x: Skalar(0.), y: Skalar(0.) }
    }

    /// Erzeuge einen Vektor aus seinen Polarkoordinaten
    ///
    /// Winkel wachsen im Uhrzeigersinn.
    /// y-Koordinaten wachsen nach unten.
    pub fn polar_koordinaten(radius: Skalar, winkel: Winkel) -> Self {
        Vektor { x: radius * Skalar(winkel.cos()), y: radius * Skalar(winkel.sin()) }
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
    pub fn skalarprodukt(&self, other: &Self) -> Skalar {
        self.x * other.x + self.y * other.y
    }

    /// Länge eines Vektors (euklidische Metrik)
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn länge(&self) -> Skalar {
        Skalar(self.skalarprodukt(self).0.sqrt())
    }

    /// Winkel zwischen zwei Vektoren (im Uhrzeigersinn)
    ///
    /// Definiert über `Vektor::skalarprodukt`.
    pub fn winkel(&self, other: &Self) -> Winkel {
        Winkel::acos((-self.skalarprodukt(other) / (self.länge() * other.länge())).0)
    }

    /// Erzeuge einen Vektor, der um /winkel/ im Uhrzeigersinn rotiert ist
    pub fn rotiere<T: Trigonometrie>(&self, winkel: T) -> Self {
        // https://de.wikipedia.org/wiki/Drehmatrix#Drehmatrix_der_Ebene_%E2%84%9D%C2%B2
        // geht von Drehung gegen den Uhrzeigersinn und nach oben steigender y-Achse aus
        Vektor {
            x: Skalar(winkel.cos()) * self.x - Skalar(winkel.sin()) * self.y,
            y: Skalar(winkel.sin()) * self.x + Skalar(winkel.cos()) * self.y,
        }
    }

    /// Konvertiere zu einem /iced::Vector/, relativ zu einem Pivot-Punkt und nachträglich gedreht.
    /// und skaliert.
    pub fn zu_iced(self, pivot: Position, faktor: Skalar) -> iced::Vector {
        let Vektor { x, y } = (self - pivot.punkt).rotiere(pivot.winkel);
        iced::Vector { x: x.0, y: y.0 }
    }

    /// Spezialfall von /zu_iced/, ohne verschieben, rotieren und skalieren.
    pub fn zu_iced_unskaliert(self) -> iced::Vector {
        self.zu_iced(
            Position { punkt: Vektor::null_vektor(), winkel: Winkel(0.) },
            Skalar::multiplikativ_neutral(),
        )
    }

    /// Konvertiere einen /iced::Vector/, invers zu /zu_iced/.
    ///
    /// iced-Koordinaten sind skaliert, gedreht und um einen pivot.punkt verschoben.
    pub fn von_iced(iced::Vector { x, y }: iced::Vector, pivot: Position, faktor: Skalar) -> Self {
        (Vektor { x: Skalar(x), y: Skalar(y) } / faktor).rotiere(-pivot.winkel) + pivot.punkt
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

    fn add(mut self, rhs: T) -> Self::Output {
        *self + rhs
    }
}
impl<T> Add<T> for &mut Vektor
where
    Vektor: AddAssign<T>,
{
    type Output = Vektor;

    fn add(mut self, rhs: T) -> Self::Output {
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

    fn sub(mut self, rhs: T) -> Self::Output {
        *self - rhs
    }
}
impl<T> Sub<T> for &mut Vektor
where
    Vektor: SubAssign<T>,
{
    type Output = Vektor;

    fn sub(mut self, rhs: T) -> Self::Output {
        &*self - rhs
    }
}

// Multiplikation/Division mit Skalar
impl MulAssign<Skalar> for Vektor {
    fn mul_assign(&mut self, rhs: Skalar) {
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
impl Mul<Vektor> for Skalar {
    type Output = Vektor;

    fn mul(self, rhs: Vektor) -> Self::Output {
        rhs * self
    }
}
impl DivAssign<Skalar> for Vektor {
    fn div_assign(&mut self, rhs: Skalar) {
        *self *= Skalar::multiplikativ_neutral() / rhs;
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
