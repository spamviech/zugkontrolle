//! Koordinaten auf einem iced::canvas::Frame

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::{convert::From, marker::PhantomData};

use iced;

use crate::gleis::types::{angle::Angle, angle::Trigonometrie, mm};

/// Horizontale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct X(pub f32);
impl X {
    pub fn to_abstand(self) -> Abstand<X> {
        Abstand(self.0, PhantomData)
    }
}
impl Add<X> for Abstand<X> {
    type Output = X;

    fn add(self, X(rhs): X) -> Self::Output {
        X(self.0 + rhs)
    }
}
impl Add<Abstand<X>> for X {
    type Output = Self;

    fn add(self, Abstand(rhs, _phantom_data): Abstand<X>) -> Self {
        X(self.0 + rhs)
    }
}
impl AddAssign<Abstand<X>> for X {
    fn add_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<X>) {
        self.0 += rhs
    }
}
impl Sub<Abstand<X>> for X {
    type Output = Self;

    fn sub(self, Abstand(rhs, _phantom_data): Abstand<X>) -> Self {
        X(self.0 - rhs)
    }
}
impl SubAssign<Abstand<X>> for X {
    fn sub_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<X>) {
        self.0 -= rhs
    }
}
impl Neg for X {
    type Output = Self;

    fn neg(self) -> Self {
        X(-self.0)
    }
}
/// Vertikale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct Y(pub f32);
impl Y {
    pub fn to_abstand(self) -> Abstand<Y> {
        Abstand(self.0, PhantomData)
    }
}
impl Add<Y> for Abstand<Y> {
    type Output = Y;

    fn add(self, Y(rhs): Y) -> Self::Output {
        Y(self.0 + rhs)
    }
}
impl Add<Abstand<Y>> for Y {
    type Output = Self;

    fn add(self, Abstand(rhs, _phantom_data): Abstand<Y>) -> Self {
        Y(self.0 + rhs)
    }
}
impl AddAssign<Abstand<Y>> for Y {
    fn add_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<Y>) {
        self.0 += rhs
    }
}
impl Sub<Abstand<Y>> for Y {
    type Output = Self;

    fn sub(self, Abstand(rhs, _phantom_data): Abstand<Y>) -> Self {
        Y(self.0 - rhs)
    }
}
impl SubAssign<Abstand<Y>> for Y {
    fn sub_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<Y>) {
        self.0 -= rhs
    }
}
impl Neg for Y {
    type Output = Y;

    fn neg(self) -> Self {
        Y(-self.0)
    }
}
/// Radius auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(pub f32);
impl Radius {
    pub fn to_abstand(self) -> Abstand<Radius> {
        Abstand(self.0, PhantomData)
    }
}
impl Add<Radius> for Abstand<Radius> {
    type Output = Radius;

    fn add(self, Radius(rhs): Radius) -> Self::Output {
        Radius(self.0 + rhs)
    }
}
impl Add<Abstand<Radius>> for Radius {
    type Output = Self;

    fn add(self, Abstand(rhs, _phantom_data): Abstand<Radius>) -> Self {
        Radius(self.0 + rhs)
    }
}
impl AddAssign<Abstand<Radius>> for Radius {
    fn add_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<Radius>) {
        self.0 += rhs
    }
}
impl Sub<Abstand<Radius>> for Radius {
    type Output = Self;

    fn sub(self, Abstand(rhs, _phantom_data): Abstand<Radius>) -> Self {
        Radius(self.0 - rhs)
    }
}
impl SubAssign<Abstand<Radius>> for Radius {
    fn sub_assign(&mut self, Abstand(rhs, _phantom_data): Abstand<Radius>) {
        self.0 -= rhs
    }
}
/// Abstand/Länge auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct Abstand<T>(f32, PhantomData<*const T>);
impl<T> Abstand<T> {
    const fn new_from_mm(abstand_mm: f32) -> Self {
        Abstand(abstand_mm, PhantomData)
    }

    pub fn max(&self, other: &Self) -> Self {
        Abstand(self.0.max(other.0), self.1)
    }

    pub fn convert<S>(self) -> Abstand<S> {
        Abstand(self.0, PhantomData)
    }
}
// with Self
impl<T> Add<Self> for Abstand<T> {
    type Output = Self;

    fn add(self, Abstand(rhs, phantom_data): Self) -> Self {
        Abstand(self.0 + rhs, phantom_data)
    }
}
impl<T> AddAssign<Self> for Abstand<T> {
    fn add_assign(&mut self, Abstand(rhs, _phantom_data): Self) {
        self.0 += rhs
    }
}
impl<T> Sub<Self> for Abstand<T> {
    type Output = Self;

    fn sub(self, Abstand(rhs, phantom_data): Self) -> Self {
        Abstand(self.0 - rhs, phantom_data)
    }
}
impl<T> SubAssign<Self> for Abstand<T> {
    fn sub_assign(&mut self, Abstand(rhs, _phantom_data): Self) {
        self.0 -= rhs
    }
}
// get ratio
impl<A, B> Div<Abstand<B>> for Abstand<A> {
    type Output = f32;
    fn div(self, rhs: Abstand<B>) -> Self::Output {
        self.0 / rhs.0
    }
}
// scale with f32
impl<T> Mul<f32> for Abstand<T> {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self::Output {
        Abstand(self.0 * rhs, self.1)
    }
}
impl<T> Mul<Abstand<T>> for f32 {
    type Output = Abstand<T>;

    fn mul(self, Abstand(rhs, phantom_data): Abstand<T>) -> Self::Output {
        Abstand(self * rhs, phantom_data)
    }
}
impl<T> MulAssign<f32> for Abstand<T> {
    fn mul_assign(&mut self, rhs: f32) {
        self.0 *= rhs
    }
}
impl<T> Div<f32> for Abstand<T> {
    type Output = Self;

    fn div(self, rhs: f32) -> Self::Output {
        Abstand(self.0 / rhs, self.1)
    }
}
impl<T> DivAssign<f32> for Abstand<T> {
    fn div_assign(&mut self, rhs: f32) {
        self.0 /= rhs
    }
}
/// Umrechnung von mm-Größen auf Canvas-Koordinaten
/// Verwenden dieser Funktion um evtl. in der Zukunft einen Faktor zu erlauben
impl mm::Spurweite {
    pub const fn to_abstand(self) -> Abstand<Y> {
        Abstand::new_from_mm(self.0)
    }
}
impl mm::Length {
    pub const fn to_abstand(self) -> Abstand<X> {
        Abstand::new_from_mm(self.0)
    }
}
impl mm::Radius {
    pub const fn to_abstand(self) -> Abstand<Radius> {
        Abstand::new_from_mm(self.0)
    }
}

/// Coordinate type safe variant of /iced::Point/
#[derive(Debug, PartialEq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Size {
    pub width: X,
    pub height: Y,
}
impl Size {
    pub fn new(width: X, height: Y) -> Self {
        Size { width, height }
    }
}
impl From<Size> for iced::Size<f32> {
    fn from(Size { width, height }: Size) -> Self {
        iced::Size { width: width.0, height: height.0 }
    }
}

/// Coordinate type safe variant of /iced::Vector/
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Vector {
    pub dx: X,
    pub dy: Y,
}
impl Vector {
    /// Erzeuge einen neuen Vektor.
    pub fn new(dx: X, dy: Y) -> Self {
        Vector { dx, dy }
    }
    /// Berechne die Länge des Vektors.
    pub fn length<T>(&self) -> Abstand<T> {
        Abstand(self.dx.0 * self.dx.0 + self.dy.0 * self.dy.0, PhantomData)
    }
    // Winkel zwischen Richtungs-Vektor und x-Achse
    pub(crate) fn winkel_mit_x_achse(&self) -> Angle {
        let len = (self.dx.0 * self.dx.0 + self.dy.0 * self.dy.0).sqrt();
        let acos_winkel = Angle::acos(self.dx.0 / len);
        if self.dy < Y(0.) {
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
            dx: X(0.) + winkel.cos() * self.dx.to_abstand()
                - winkel.sin() * self.dy.to_abstand().convert(),
            dy: Y(0.)
                + winkel.sin() * self.dx.to_abstand().convert()
                + winkel.cos() * self.dy.to_abstand(),
        }
    }
}
impl From<Vector> for iced::Vector {
    fn from(Vector { dx, dy }: Vector) -> Self {
        iced::Vector { x: dx.0, y: dy.0 }
    }
}
impl Neg for Vector {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Vector { dx: -self.dx, dy: -self.dy }
    }
}
// Convert to Vector
impl From<Point> for Vector {
    fn from(Point { x, y }: Point) -> Self {
        Vector { dx: x, dy: y }
    }
}
impl From<Size> for Vector {
    fn from(Size { width, height }: Size) -> Self {
        Vector { dx: width, dy: height }
    }
}
// add Vector and Point
impl AddAssign<Vector> for Point {
    fn add_assign(&mut self, other: Vector) {
        self.x += other.dx.to_abstand();
        self.y += other.dy.to_abstand();
    }
}
impl Add<Vector> for Point {
    type Output = Point;
    fn add(self, other: Vector) -> Self::Output {
        Point { x: self.x + other.dx.to_abstand(), y: self.y + other.dy.to_abstand() }
    }
}
impl Add<Point> for Vector {
    type Output = Point;
    fn add(self, other: Point) -> Self::Output {
        Point { x: self.dx + other.x.to_abstand(), y: self.dy + other.y.to_abstand() }
    }
}
// substract Vector from Point
impl SubAssign<Vector> for Point {
    fn sub_assign(&mut self, other: Vector) {
        self.x -= other.dx.to_abstand();
        self.y -= other.dy.to_abstand();
    }
}
impl Sub<Vector> for Point {
    type Output = Point;
    fn sub(self, other: Vector) -> Self::Output {
        Point { x: self.x - other.dx.to_abstand(), y: self.y - other.dy.to_abstand() }
    }
}
// scale with f32
impl Mul<Vector> for f32 {
    type Output = Vector;
    fn mul(self, other: Vector) -> Self::Output {
        Vector {
            dx: X(0.) + self * other.dx.to_abstand(),
            dy: Y(0.) + self * other.dy.to_abstand(),
        }
    }
}
impl Mul<f32> for Vector {
    type Output = Vector;
    fn mul(self, other: f32) -> Self::Output {
        Vector {
            dx: X(0.) + self.dx.to_abstand() * other,
            dy: Y(0.) + self.dy.to_abstand() * other,
        }
    }
}
impl Div<f32> for Vector {
    type Output = Vector;
    fn div(self, other: f32) -> Self::Output {
        Vector {
            dx: X(0.) + self.dx.to_abstand() / other,
            dy: Y(0.) + self.dy.to_abstand() / other,
        }
    }
}

/// Coordinate type safe variant of /iced::canvas::path::Arc/
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Arc {
    pub center: Point,
    pub radius: Radius,
    pub start: Angle,
    pub end: Angle,
}
impl Arc {
    pub fn new(center: Point, radius: Radius, start: Angle, end: Angle) -> Self {
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
