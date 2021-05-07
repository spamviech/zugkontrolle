//! Koordinaten auf einem iced::canvas::Frame

use std::convert::From;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use serde::{Deserialize, Serialize};

use crate::gleis::typen::{mm, winkel::Trigonometrie, winkel::Winkel};

/// Horizontale Koordinate auf einem Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct X(pub f32);
macro_rules! impl_with_abstand {
    ($type:ident) => {
        impl $type {
            pub fn to_abstand(self) -> Abstand<$type> {
                Abstand(self)
            }
        }
        impl Add<$type> for Abstand<$type> {
            type Output = $type;

            fn add(self, $type(rhs): $type) -> Self::Output {
                $type(self.0 .0 + rhs)
            }
        }
        impl Add<Abstand<$type>> for $type {
            type Output = Self;

            fn add(self, Abstand($type(rhs)): Abstand<$type>) -> Self {
                $type(self.0 + rhs)
            }
        }
        impl AddAssign<Abstand<$type>> for $type {
            fn add_assign(&mut self, Abstand($type(rhs)): Abstand<$type>) {
                self.0 += rhs
            }
        }
        impl Sub<Abstand<$type>> for $type {
            type Output = Self;

            fn sub(self, Abstand($type(rhs)): Abstand<$type>) -> Self {
                $type(self.0 - rhs)
            }
        }
        impl SubAssign<Abstand<$type>> for $type {
            fn sub_assign(&mut self, Abstand($type(rhs)): Abstand<$type>) {
                self.0 -= rhs
            }
        }
    };
}
impl_with_abstand! {X}
impl Neg for X {
    type Output = Self;

    fn neg(self) -> Self {
        X(-self.0)
    }
}
/// Vertikale Koordinate auf einem Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct Y(pub f32);
impl_with_abstand! {Y}
impl Neg for Y {
    type Output = Y;

    fn neg(self) -> Self {
        Y(-self.0)
    }
}
/// Radius auf einem Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct Radius(pub f32);
impl_with_abstand! {Radius}

/// Abstand/Länge auf einem Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy, Serialize, Deserialize)]
pub struct Abstand<T>(T);
macro_rules! impl_abstand {
    ($type:ident) => {
        impl Abstand<$type> {
            const fn new_from_mm(abstand_mm: f32) -> Self {
                Abstand($type(abstand_mm))
            }

            pub fn min(&self, other: &Self) -> Self {
                Abstand($type(self.0 .0.min(other.0 .0)))
            }

            pub fn max(&self, other: &Self) -> Self {
                Abstand($type(self.0 .0.max(other.0 .0)))
            }

            pub fn as_x(self) -> Abstand<X> {
                Abstand(X(self.0 .0))
            }

            pub fn as_y(self) -> Abstand<Y> {
                Abstand(Y(self.0 .0))
            }

            pub fn as_radius(self) -> Abstand<Radius> {
                Abstand(Radius(self.0 .0))
            }
        }
        // with Self
        impl AddAssign<Self> for Abstand<$type> {
            fn add_assign(&mut self, Abstand($type(rhs)): Self) {
                self.0 .0 += rhs
            }
        }
        impl Add<Self> for Abstand<$type> {
            type Output = Self;

            fn add(mut self, other: Self) -> Self {
                self += other;
                self
            }
        }
        impl SubAssign<Self> for Abstand<$type> {
            fn sub_assign(&mut self, Abstand($type(rhs)): Self) {
                self.0 .0 -= rhs
            }
        }
        impl Sub<Self> for Abstand<$type> {
            type Output = Self;

            fn sub(mut self, other: Self) -> Self {
                self -= other;
                self
            }
        }
        // get ratio
        impl Div<Abstand<X>> for Abstand<$type> {
            type Output = f32;

            fn div(self, Abstand(X(rhs)): Abstand<X>) -> Self::Output {
                self.0 .0 / rhs
            }
        }
        impl Div<Abstand<Y>> for Abstand<$type> {
            type Output = f32;

            fn div(self, Abstand(Y(rhs)): Abstand<Y>) -> Self::Output {
                self.0 .0 / rhs
            }
        }
        impl Div<Abstand<Radius>> for Abstand<$type> {
            type Output = f32;

            fn div(self, Abstand(Radius(rhs)): Abstand<Radius>) -> Self::Output {
                self.0 .0 / rhs
            }
        }
        // scale with f32
        impl MulAssign<f32> for Abstand<$type> {
            fn mul_assign(&mut self, rhs: f32) {
                self.0 .0 *= rhs
            }
        }
        impl Mul<f32> for Abstand<$type> {
            type Output = Self;

            fn mul(mut self, rhs: f32) -> Self::Output {
                self *= rhs;
                self
            }
        }
        impl Mul<Abstand<$type>> for f32 {
            type Output = Abstand<$type>;

            fn mul(self, rhs: Abstand<$type>) -> Self::Output {
                rhs * self
            }
        }
        impl DivAssign<f32> for Abstand<$type> {
            fn div_assign(&mut self, rhs: f32) {
                self.0 .0 /= rhs
            }
        }
        impl Div<f32> for Abstand<$type> {
            type Output = Self;

            fn div(mut self, rhs: f32) -> Self::Output {
                self /= rhs;
                self
            }
        }
    };
}
impl_abstand! {X}
impl_abstand! {Y}
impl_abstand! {Radius}
/// Umrechnung von mm-Größen auf Canvas-Koordinaten
/// Verwenden dieser Funktion um evtl. in der Zukunft einen Faktor zu erlauben
impl mm::Spurweite {
    pub const fn to_abstand(self) -> Abstand<Y> {
        Abstand::<Y>::new_from_mm(self.0)
    }
}
impl mm::Länge {
    pub const fn to_abstand(self) -> Abstand<X> {
        Abstand::<X>::new_from_mm(self.0)
    }
}
impl mm::Radius {
    pub const fn to_abstand(self) -> Abstand<Radius> {
        Abstand::<Radius>::new_from_mm(self.0)
    }
}

/// Coordinate type safe variant of /iced::Point/
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Point {
    pub x: X,
    pub y: Y,
}
impl Point {
    pub fn new(x: X, y: Y) -> Self {
        Point { x, y }
    }
}
impl From<Point> for iced::Point {
    fn from(Point { x, y }: Point) -> Self {
        iced::Point { x: x.0, y: y.0 }
    }
}

/// Coordinate type safe variant of /iced::Size/
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Size {
    pub width: Abstand<X>,
    pub height: Abstand<Y>,
}
impl Size {
    pub fn new(width: Abstand<X>, height: Abstand<Y>) -> Self {
        Size { width, height }
    }
}
impl From<Size> for iced::Size<f32> {
    fn from(Size { width, height }: Size) -> Self {
        iced::Size { width: width.0 .0, height: height.0 .0 }
    }
}

/// Coordinate type safe variant of /iced::Vector/
#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Vector {
    pub dx: Abstand<X>,
    pub dy: Abstand<Y>,
}
impl Vector {
    /// Erzeuge einen neuen Vektor.
    pub fn new(x: X, y: Y) -> Self {
        Vector { dx: x.to_abstand(), dy: y.to_abstand() }
    }

    /// Berechne die Länge des Vektors.
    pub fn length_x(&self) -> Abstand<X> {
        Abstand(X((self.dx.0 .0 * self.dx.0 .0 + self.dy.0 .0 * self.dy.0 .0).sqrt()))
    }

    /// Berechne die Länge des Vektors.
    pub fn length_y(&self) -> Abstand<Y> {
        Abstand(Y((self.dx.0 .0 * self.dx.0 .0 + self.dy.0 .0 * self.dy.0 .0).sqrt()))
    }

    /// Berechne die Länge des Vektors.
    pub fn length_radius(&self) -> Abstand<Radius> {
        Abstand(Radius((self.dx.0 .0 * self.dx.0 .0 + self.dy.0 .0 * self.dy.0 .0).sqrt()))
    }

    // Winkel zwischen Richtungs-Vektor und x-Achse
    pub(crate) fn winkel_mit_x_achse(&self) -> Winkel {
        let len = (self.dx.0 .0 * self.dx.0 .0 + self.dy.0 .0 * self.dy.0 .0).sqrt();
        let acos_winkel = Winkel::acos(self.dx.0 .0 / len);
        if self.dy.0 .0 < 0. {
            acos_winkel
        } else {
            -acos_winkel
        }
    }

    /// Erzeuge einen Vektor, der um /winkel/ im Uhrzeigersinn rotiert ist.
    pub fn rotate<T: Trigonometrie>(&self, winkel: T) -> Self {
        // https://de.wikipedia.org/wiki/Drehmatrix#Drehmatrix_der_Ebene_%E2%84%9D%C2%B2
        // geht von Drehung gegen den Uhrzeigersinn und nach oben steigender y-Achse aus
        Vector {
            dx: winkel.cos() * self.dx - winkel.sin() * self.dy.as_x(),
            dy: winkel.sin() * self.dx.as_y() + winkel.cos() * self.dy,
        }
    }

    /// Berechne das Skalarprodukt zweier Vektoren.
    ///
    /// Es gilt `self.scalar_product(other) == self.length() * other.length() *
    /// winkel_zwischen_self_und_other.cos()`.
    pub fn scalar_product(&self, other: &Vector) -> f32 {
        self.dx.0 .0 * other.dx.0 .0 + self.dy.0 .0 * other.dy.0 .0
    }

    /// Berechne das Skalarprodukt zweier Vektoren, normiert auf Einheitsvektoren.
    ///
    /// Es gilt `self.scalar_product_normalized(other) == winkel_zwischen_self_und_other.cos()`.
    pub fn scalar_product_normalized(&self, other: &Vector) -> f32 {
        self.scalar_product(other) / (self.length_x().0 .0 * other.length_x().0 .0)
    }
}
impl From<Vector> for iced::Vector {
    fn from(Vector { dx, dy }: Vector) -> Self {
        iced::Vector { x: dx.0 .0, y: dy.0 .0 }
    }
}
impl Neg for Vector {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.dx.0 .0 *= -1.;
        self.dy.0 .0 *= -1.;
        self
    }
}
// Convert to Vector
impl From<Point> for Vector {
    fn from(Point { x, y }: Point) -> Self {
        Vector { dx: x.to_abstand(), dy: y.to_abstand() }
    }
}
impl From<Size> for Vector {
    fn from(Size { width, height }: Size) -> Self {
        Vector { dx: width, dy: height }
    }
}
// add two Vector
impl AddAssign<&Vector> for Vector {
    fn add_assign(&mut self, other: &Vector) {
        self.dx += other.dx;
        self.dy += other.dy;
    }
}
impl AddAssign<Vector> for Vector {
    fn add_assign(&mut self, other: Vector) {
        *self += &other
    }
}
impl Add<Vector> for Vector {
    type Output = Self;

    fn add(mut self, other: Vector) -> Self::Output {
        self += other;
        self
    }
}
impl Add<&Vector> for Vector {
    type Output = Self;

    fn add(mut self, other: &Vector) -> Self::Output {
        self += other;
        self
    }
}
// subtract one vector from another
impl SubAssign<&Vector> for Vector {
    fn sub_assign(&mut self, other: &Vector) {
        self.dx -= other.dx;
        self.dy -= other.dy;
    }
}
impl SubAssign<Vector> for Vector {
    fn sub_assign(&mut self, other: Vector) {
        *self -= &other
    }
}
impl Sub<&Vector> for Vector {
    type Output = Self;

    fn sub(mut self, other: &Vector) -> Self::Output {
        self -= other;
        self
    }
}
impl Sub<Vector> for Vector {
    type Output = Self;

    fn sub(mut self, other: Vector) -> Self::Output {
        self -= other;
        self
    }
}
// add Vector and Point
impl AddAssign<&Vector> for Point {
    fn add_assign(&mut self, other: &Vector) {
        self.x += other.dx;
        self.y += other.dy;
    }
}
impl AddAssign<Vector> for Point {
    fn add_assign(&mut self, other: Vector) {
        *self += &other
    }
}
impl Add<Vector> for Point {
    type Output = Point;

    fn add(mut self, other: Vector) -> Self::Output {
        self += other;
        self
    }
}
impl Add<&Vector> for Point {
    type Output = Point;

    fn add(mut self, other: &Vector) -> Self::Output {
        self += other;
        self
    }
}
impl Add<Point> for &Vector {
    type Output = Point;

    fn add(self, other: Point) -> Self::Output {
        other + self
    }
}
impl Add<Point> for Vector {
    type Output = Point;

    fn add(self, other: Point) -> Self::Output {
        other + self
    }
}
// substract Vector from Point
impl SubAssign<&Vector> for Point {
    fn sub_assign(&mut self, other: &Vector) {
        self.x -= other.dx;
        self.y -= other.dy;
    }
}
impl SubAssign<Vector> for Point {
    fn sub_assign(&mut self, other: Vector) {
        *self -= &other
    }
}
impl Sub<&Vector> for Point {
    type Output = Point;

    fn sub(mut self, other: &Vector) -> Self::Output {
        self -= other;
        self
    }
}
impl Sub<Vector> for Point {
    type Output = Point;

    fn sub(mut self, other: Vector) -> Self::Output {
        self -= other;
        self
    }
}
// scale with f32
impl MulAssign<f32> for Vector {
    fn mul_assign(&mut self, other: f32) {
        self.dx *= other;
        self.dy *= other;
    }
}
impl Mul<f32> for Vector {
    type Output = Vector;

    fn mul(mut self, other: f32) -> Self::Output {
        self *= other;
        self
    }
}
impl Mul<Vector> for f32 {
    type Output = Vector;

    fn mul(self, other: Vector) -> Self::Output {
        other * self
    }
}
impl Mul<&Vector> for f32 {
    type Output = Vector;

    fn mul(self, other: &Vector) -> Self::Output {
        *other * self
    }
}
impl DivAssign<f32> for Vector {
    fn div_assign(&mut self, other: f32) {
        self.dx /= other;
        self.dy /= other;
    }
}
impl Div<f32> for Vector {
    type Output = Vector;

    fn div(mut self, other: f32) -> Self::Output {
        self /= other;
        self
    }
}

/// Coordinate type safe variant of /iced::canvas::path::Arc/
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Arc {
    pub center: Point,
    pub radius: Radius,
    pub start: Winkel,
    pub end: Winkel,
}
impl Arc {
    pub fn new(center: Point, radius: Radius, start: Winkel, end: Winkel) -> Self {
        Arc { center, radius, start, end }
    }
}
impl From<Arc> for iced::canvas::path::Arc {
    fn from(Arc { center, radius, start, end }: Arc) -> Self {
        iced::canvas::path::Arc {
            center: center.into(),
            radius: radius.0,
            start_angle: start.0,
            end_angle: end.0,
        }
    }
}

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub point: Point,
    pub winkel: Winkel,
}
impl Position {
    /// Point nachdem das Objekt an die Position bewegt und um den Winkel gedreht wird.
    pub fn transformation(&self, anchor: Point) -> Point {
        let x = X(self.point.x.0 + anchor.x.0 * self.winkel.cos() - anchor.y.0 * self.winkel.sin());
        let y = Y(self.point.y.0 + anchor.x.0 * self.winkel.sin() + anchor.y.0 * self.winkel.cos());
        Point { x, y }
    }

    /// Vector nachdem das Objekt um den Winkel gedreht wird.
    pub fn rotation(&self, direction: Vector) -> Vector {
        let dx = direction.dx * self.winkel.cos() - direction.dy.as_x() * self.winkel.sin();
        let dy = direction.dx.as_y() * self.winkel.sin() + direction.dy * self.winkel.cos();
        Vector { dx, dy }
    }
}
