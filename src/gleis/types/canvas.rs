//! newtypes für einen cairo::Context

use std::f32::consts::PI;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::{convert::From, marker::PhantomData};

use iced;

use super::anchor;
use super::angle::Angle;

// re-exports
pub use iced::{
    canvas::{Cache, Fill, FillRule, Stroke, Text},
    Color,
};

/// Horizontale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct X(pub f32);
impl From<X> for Abstand<X> {
    fn from(X(input): X) -> Self {
        Abstand(input, PhantomData)
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
impl From<Y> for Abstand<Y> {
    fn from(Y(input): Y) -> Self {
        Abstand(input, PhantomData)
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
impl From<Radius> for Abstand<Radius> {
    fn from(Radius(input): Radius) -> Self {
        Abstand(input, PhantomData)
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
impl<T> From<super::Spurweite> for Abstand<T> {
    fn from(super::Spurweite(spurweite): super::Spurweite) -> Self {
        Abstand::new_from_mm(spurweite)
    }
}
impl<T> From<super::Length> for Abstand<T> {
    fn from(super::Length(length): super::Length) -> Self {
        Abstand::new_from_mm(length)
    }
}
impl From<super::Radius> for Abstand<Radius> {
    fn from(super::Radius(radius): super::Radius) -> Self {
        Abstand::new_from_mm(radius)
    }
}
pub trait ToAbstand<T>: Into<Abstand<T>> {
    fn to_abstand(self) -> Abstand<T> {
        self.into()
    }
}
impl<T: Into<Abstand<S>>, S> ToAbstand<S> for T {}

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
impl From<anchor::Position> for Point {
    fn from(anchor::Position { x, y }: anchor::Position) -> Self {
        Point { x, y }
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
    pub fn new(dx: X, dy: Y) -> Self {
        Vector { dx, dy }
    }
}
impl From<Vector> for iced::Vector {
    fn from(Vector { dx, dy }: Vector) -> Self {
        iced::Vector { x: dx.0, y: dy.0 }
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
impl From<anchor::Direction> for Vector {
    fn from(anchor::Direction { dx, dy }: anchor::Direction) -> Self {
        Vector { dx, dy }
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

/// Pfad auf dem Canvas
///
/// Transformationen werden ausgeführt, bevor der Pfad gezeichnet/gefüllt wird!
pub struct Path {
    path: iced::canvas::Path,
    transformations: Vec<Transformation>,
}

/// Unterstützte Transformationen
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Transformation {
    /// Verschiebe alle Koordinaten um den übergebenen Vector.
    Translate(Vector),
    /// Rotiere alle Koordinaten um den Ursprung (im Uhrzeigersinn)
    Rotate(Angle),
    /// Skaliere alle Koordinaten (x',y') = (x*scale, y*scale)
    Scale(f32),
}

/// Helper struct, so I don't make stupid mistakes (e.g. inverting twice)
pub struct Inverted<T, Axis>(T, PhantomData<*const Axis>);
pub trait ToPoint {
    fn to_point(self) -> Point;
}
impl ToPoint for Point {
    fn to_point(self) -> Point {
        self
    }
}
impl<P: ToPoint, Axis> ToPoint for Inverted<P, Axis> {
    fn to_point(self) -> Point {
        self.0.to_point()
    }
}
pub trait ToArc {
    fn to_arc(self) -> Arc;
}
impl ToArc for Arc {
    fn to_arc(self) -> Arc {
        self
    }
}
impl<A: ToArc, Axis> ToArc for Inverted<A, Axis> {
    fn to_arc(self) -> Arc {
        self.0.to_arc()
    }
}
impl<T: Into<Inverted<T, B>>, A, B> From<Inverted<T, A>> for Inverted<Inverted<T, A>, B> {
    fn from(inverted: Inverted<T, A>) -> Self {
        Inverted(Inverted(inverted.0.into().0, inverted.1), PhantomData)
    }
}
impl From<Point> for Inverted<Point, X> {
    fn from(point: Point) -> Self {
        Inverted(Point { x: -point.x, ..point }, PhantomData)
    }
}
impl From<Point> for Inverted<Point, Y> {
    fn from(point: Point) -> Self {
        Inverted(Point { y: -point.y, ..point }, PhantomData)
    }
}
impl From<Angle> for Inverted<Angle, X> {
    fn from(angle: Angle) -> Self {
        Inverted(Angle::new(PI) - angle, PhantomData)
    }
}
impl From<Angle> for Inverted<Angle, Y> {
    fn from(angle: Angle) -> Self {
        Inverted(-angle, PhantomData)
    }
}
impl From<Arc> for Inverted<Arc, X> {
    fn from(arc: Arc) -> Self {
        Inverted(
            Arc {
                center: Inverted::<Point, X>::from(arc.center).0,
                start: Inverted::<Angle, X>::from(arc.start).0,
                end: Inverted::<Angle, X>::from(arc.end).0,
                ..arc
            },
            PhantomData,
        )
    }
}
impl From<Arc> for Inverted<Arc, Y> {
    fn from(arc: Arc) -> Self {
        Inverted(
            Arc {
                center: Inverted::<Point, Y>::from(arc.center).0,
                start: Inverted::<Angle, Y>::from(arc.start).0,
                end: Inverted::<Angle, Y>::from(arc.end).0,
                ..arc
            },
            PhantomData,
        )
    }
}
/// newtype auf einem /iced::canvas::path::Builder/
///
/// Implementiert nur Methoden, die ich auch benötige.
/// Evtl. werden später weitere hinzugefügt.
/// Alle Methoden verwenden die hier definierten Typen.
pub struct PathBuilder<P, A> {
    builder: iced::canvas::path::Builder,
    phantom_data: PhantomData<*const (P, A)>,
}

impl PathBuilder<Point, Arc> {
    /// create a new PathBuilder
    pub fn new() -> Self {
        PathBuilder { builder: iced::canvas::path::Builder::new(), phantom_data: PhantomData }
    }

    /// Finalize the Path, building the immutable result
    pub fn build(self) -> Path {
        self.build_under_transformations(Vec::new())
    }

    /// Finalize the Path, building the immutable result after applying all given transformations
    pub fn build_under_transformations(self, transformations: Vec<Transformation>) -> Path {
        Path { path: self.builder.build(), transformations }
    }
}

impl<P: ToPoint, A: ToArc> PathBuilder<P, A> {
    // start a new subpath at /point/
    pub fn move_to(&mut self, point: P) {
        self.builder.move_to(point.to_point().into())
    }

    /// strike a direct line from the current point to /point/
    pub fn line_to(&mut self, point: P) {
        self.builder.line_to(point.to_point().into())
    }

    /// Strike an arc around (xc,xy) with given radius from angle1 to angle2 (clockwise).
    ///
    /// Start a new subgraph.
    pub fn arc(&mut self, arc: A /*, new_sub_path: bool*/) {
        self.builder.arc(arc.to_arc().into())
    }

    /*
    // TODO Funktioniert nicht mit with_invert_x,y :(
    // iced-github-Issue öffnen, die verwendete Bibliothek scheint eine Flag zu unterstützen
    /// Strike an arc from /a/ to /b/ with given radius (clockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph before the arc.
    /// Otherwise, strike a direct line from the current point to the start of the arc.
    pub fn arc_to(&mut self, a: Point, b: Point, radius: Radius, new_sub_path: bool) {
        if new_sub_path {
            self.move_to(a.clone())
        }
        self.builder.arc_to(
            self.invert_point_axis(a).into(),
            self.invert_point_axis(b).into(),
            radius.0,
        )
    }
    */

    /// strike a direct line from the current point to the start of the last subpath
    pub fn close(&mut self) {
        self.builder.close()
    }

    /// Alle Methoden der closure verwenden unveränderte Achsen (x',y') = (x,y)
    ///
    /// Convenience-Funktion um nicht permanent no-op closures erstellen zu müssen.
    pub fn with_normal_axis(&mut self, action: impl for<'s> FnOnce(&'s mut PathBuilder<P, A>)) {
        action(self)
    }

    /// Alle Methoden der closure verwenden eine gespiegelte x-Achse (x',y') = (-x,y)
    ///
    /// **ACHTUNG:** /arc_to/ hat den Bogen vmtl. in der falschen Richtung.
    /// Aktionen werden in umgekehrter Reihenfolge ausgeführt,
    /// vermutlich sollte davor/danach ein neuer (sub) path gestartet werden.
    pub fn with_invert_x(
        &mut self,
        action: impl for<'s> FnOnce(&'s mut PathBuilder<Inverted<P, X>, Inverted<A, X>>),
    ) {
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: PathBuilder<Inverted<P, X>, Inverted<A, X>> =
                PathBuilder { builder, phantom_data: PhantomData };
            action(&mut inverted_builder);
            inverted_builder.builder
        })
    }

    /// Alle Methoden der closure verwenden eine gespiegelte y-Achse (x',y') = (x,-y)
    ///
    /// **ACHTUNG:** /arc_to/ hat den Bogen vmtl. in der falschen Richtung.
    /// Aktionen werden in umgekehrter Reihenfolge ausgeführt,
    /// vermutlich sollte davor/danach ein neuer (sub) path gestartet werden.
    pub fn with_invert_y(
        &mut self,
        action: impl for<'s> FnOnce(&'s mut PathBuilder<Inverted<P, Y>, Inverted<A, Y>>),
    ) {
        take_mut::take(&mut self.builder, |builder| {
            let mut inverted_builder: PathBuilder<Inverted<P, Y>, Inverted<A, Y>> =
                PathBuilder { builder, phantom_data: PhantomData };
            action(&mut inverted_builder);
            inverted_builder.builder
        })
    }
}

pub struct Frame<'t>(&'t mut iced::canvas::Frame);
impl<'t> Frame<'t> {
    pub fn new(frame: &'t mut iced::canvas::Frame) -> Self {
        Frame(frame)
    }

    /// Draws the stroke of the given Path on the Frame with the provided style.
    pub fn stroke(&mut self, Path { path, transformations }: &Path, stroke: impl Into<Stroke>) {
        self.with_save(|frame| {
            for transformation in transformations {
                frame.transformation(transformation)
            }
            frame.0.stroke(path, stroke)
        })
    }

    /// Draws the given Path on the Frame by filling it with the provided style.
    pub fn fill(&mut self, Path { path, transformations }: &Path, fill: impl Into<Fill>) {
        self.with_save(|frame| {
            for transformation in transformations {
                frame.transformation(transformation)
            }
            frame.0.fill(path, fill)
        })
    }

    /// Draws the characters of the given Text on the Frame, filling them with the given color.
    ///
    /// **Warning:** problems regarding transformation/rotation/scaling from /iced::canvas::Frame/ apply here as well!
    pub fn fill_text(&mut self, text: impl Into<Text>) {
        self.0.fill_text(text)
    }

    /// Stores the current transform of the Frame and executes the given drawing operations, restoring the transform afterwards.
    ///
    /// This method is useful to compose transforms and perform drawing operations in different coordinate systems.
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        self.0.with_save(|frame| action(&mut Frame(frame)))
    }

    /// Wende die übergebene Transformation auf den Frame an.
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translate(vector) => self.0.translate((*vector).into()),
            Transformation::Rotate(angle) => self.0.rotate(angle.0),
            Transformation::Scale(scale) => self.0.scale(*scale),
        }
    }
}
