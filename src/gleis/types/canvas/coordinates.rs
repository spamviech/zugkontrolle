//! Koordinaten auf einem iced::canvas::Frame

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::{convert::From, marker::PhantomData};

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
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
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

    pub fn min(&self, other: &Self) -> Self {
        Abstand(self.0.min(other.0), self.1)
    }

    pub fn max(&self, other: &Self) -> Self {
        Abstand(self.0.max(other.0), self.1)
    }

    pub fn as_x(self) -> Abstand<X> {
        Abstand(self.0, PhantomData)
    }

    pub fn as_y(self) -> Abstand<Y> {
        Abstand(self.0, PhantomData)
    }

    pub fn as_radius(self) -> Abstand<Radius> {
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
        iced::Size { width: width.0, height: height.0 }
    }
}

/// Coordinate type safe variant of /iced::Vector/
#[derive(Debug, PartialEq, Clone, Copy)]
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
    pub fn length<T>(&self) -> Abstand<T> {
        Abstand((self.dx.0 * self.dx.0 + self.dy.0 * self.dy.0).sqrt(), PhantomData)
    }
    // Winkel zwischen Richtungs-Vektor und x-Achse
    pub(crate) fn winkel_mit_x_achse(&self) -> Angle {
        let len = (self.dx.0 * self.dx.0 + self.dy.0 * self.dy.0).sqrt();
        let acos_winkel = Angle::acos(self.dx.0 / len);
        if self.dy.0 < 0. {
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
    /// Es gilt `self.scalar_product(other) == self.length() * other.length() * winkel_zwischen_self_und_other.cos()`.
    pub fn scalar_product(&self, other: &Vector) -> f32 {
        self.dx.0 * other.dx.0 + self.dy.0 * other.dy.0
    }
    /// Berechne das Skalarprodukt zweier Vektoren, normiert auf Einheitsvektoren.
    ///
    /// Es gilt `self.scalar_product_normalized(other) == winkel_zwischen_self_und_other.cos()`.
    pub fn scalar_product_normalized(&self, other: &Vector) -> f32 {
        self.scalar_product(other) / (self.length::<X>().0 * other.length::<X>().0)
    }
}
impl From<Vector> for iced::Vector {
    fn from(Vector { dx, dy }: Vector) -> Self {
        iced::Vector { x: dx.0, y: dy.0 }
    }
}
impl Neg for Vector {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        self.dx.0 *= -1.;
        self.dy.0 *= -1.;
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
// add Vector and Point
impl AddAssign<Vector> for Point {
    fn add_assign(&mut self, other: Vector) {
        self.x += other.dx;
        self.y += other.dy;
    }
}
impl AddAssign<&Vector> for Point {
    fn add_assign(&mut self, other: &Vector) {
        self.x += other.dx;
        self.y += other.dy;
    }
}
impl Add<Vector> for Point {
    type Output = Point;
    fn add(self, other: Vector) -> Self::Output {
        Point { x: self.x + other.dx, y: self.y + other.dy }
    }
}
impl Add<&Vector> for Point {
    type Output = Point;
    fn add(self, other: &Vector) -> Self::Output {
        Point { x: self.x + other.dx, y: self.y + other.dy }
    }
}
impl Add<Point> for Vector {
    type Output = Point;
    fn add(self, other: Point) -> Self::Output {
        Point { x: self.dx + other.x, y: self.dy + other.y }
    }
}
impl Add<Point> for &Vector {
    type Output = Point;
    fn add(self, other: Point) -> Self::Output {
        Point { x: self.dx + other.x, y: self.dy + other.y }
    }
}
// substract Vector from Point
impl SubAssign<Vector> for Point {
    fn sub_assign(&mut self, other: Vector) {
        self.x -= other.dx;
        self.y -= other.dy;
    }
}
impl SubAssign<&Vector> for Point {
    fn sub_assign(&mut self, other: &Vector) {
        self.x -= other.dx;
        self.y -= other.dy;
    }
}
impl Sub<Vector> for Point {
    type Output = Point;
    fn sub(self, other: Vector) -> Self::Output {
        Point { x: self.x - other.dx, y: self.y - other.dy }
    }
}
impl Sub<&Vector> for Point {
    type Output = Point;
    fn sub(self, other: &Vector) -> Self::Output {
        Point { x: self.x - other.dx, y: self.y - other.dy }
    }
}
// scale with f32
impl Mul<Vector> for f32 {
    type Output = Vector;
    fn mul(self, other: Vector) -> Self::Output {
        Vector { dx: self * other.dx, dy: self * other.dy }
    }
}
impl Mul<&Vector> for f32 {
    type Output = Vector;
    fn mul(self, other: &Vector) -> Self::Output {
        Vector { dx: self * other.dx, dy: self * other.dy }
    }
}
impl Mul<f32> for Vector {
    type Output = Vector;
    fn mul(self, other: f32) -> Self::Output {
        Vector { dx: self.dx * other, dy: self.dy * other }
    }
}
impl Div<f32> for Vector {
    type Output = Vector;
    fn div(self, other: f32) -> Self::Output {
        Vector { dx: self.dx / other, dy: self.dy / other }
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

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone)]
pub struct Position {
    pub point: Point,
    pub winkel: Angle,
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
