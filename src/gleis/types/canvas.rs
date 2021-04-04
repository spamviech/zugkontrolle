//! newtypes für einen cairo::Context

use std::convert::From;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use iced::{self, canvas};
// use cairo::Context;
// pub use cairo::Matrix;

use super::anchor;
use super::angle::Angle;

// re-exports
pub use iced::{
    canvas::{Cache, Fill, FillRule, Path, Stroke, Text},
    Color,
};

/// Horizontale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct X(pub f32);
impl Add<Abstand> for X {
    type Output = Self;

    fn add(self, Abstand(rhs): Abstand) -> Self {
        X(self.0 + rhs)
    }
}
impl AddAssign<Abstand> for X {
    fn add_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 += rhs
    }
}
impl Sub<Abstand> for X {
    type Output = Self;

    fn sub(self, Abstand(rhs): Abstand) -> Self {
        X(self.0 - rhs)
    }
}
impl SubAssign<Abstand> for X {
    fn sub_assign(&mut self, Abstand(rhs): Abstand) {
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
impl Add<Abstand> for Y {
    type Output = Self;

    fn add(self, Abstand(rhs): Abstand) -> Self {
        Y(self.0 + rhs)
    }
}
impl AddAssign<Abstand> for Y {
    fn add_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 += rhs
    }
}
impl Sub<Abstand> for Y {
    type Output = Self;

    fn sub(self, Abstand(rhs): Abstand) -> Self {
        Y(self.0 - rhs)
    }
}
impl SubAssign<Abstand> for Y {
    fn sub_assign(&mut self, Abstand(rhs): Abstand) {
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
impl Add<Abstand> for Radius {
    type Output = Self;

    fn add(self, Abstand(rhs): Abstand) -> Self {
        Radius(self.0 + rhs)
    }
}
impl AddAssign<Abstand> for Radius {
    fn add_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 += rhs
    }
}
impl Sub<Abstand> for Radius {
    type Output = Self;

    fn sub(self, Abstand(rhs): Abstand) -> Self {
        Radius(self.0 - rhs)
    }
}
impl SubAssign<Abstand> for Radius {
    fn sub_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 -= rhs
    }
}
/// Abstand/Länge auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct Abstand(f32);
impl Abstand {
    const fn new_from_mm(abstand_mm: f32) -> Abstand {
        Abstand(abstand_mm)
    }

    pub fn max(&self, other: &Abstand) -> Abstand {
        Abstand(self.0.max(other.0))
    }

    /// Anzahl benötigter Pixel um den CanvasAbstand darstellen zu können
    pub fn pixel(&self) -> u64 {
        self.0.ceil() as u64
    }
}
// with Self
impl Add<Self> for Abstand {
    type Output = Self;

    fn add(self, Abstand(rhs): Abstand) -> Self {
        Abstand(self.0 + rhs)
    }
}
impl AddAssign<Self> for Abstand {
    fn add_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 += rhs
    }
}
impl Sub<Self> for Abstand {
    type Output = Self;

    fn sub(self, Abstand(rhs): Abstand) -> Self {
        Abstand(self.0 - rhs)
    }
}
impl SubAssign<Self> for Abstand {
    fn sub_assign(&mut self, Abstand(rhs): Abstand) {
        self.0 -= rhs
    }
}
// get ratio
impl Div<Self> for Abstand {
    type Output = f32;
    fn div(self, rhs: Self) -> Self::Output {
        self.0 / rhs.0
    }
}
// with X
impl From<X> for Abstand {
    fn from(X(input): X) -> Self {
        Abstand(input)
    }
}
impl Add<X> for Abstand {
    type Output = X;

    fn add(self, X(rhs): X) -> Self::Output {
        X(self.0 + rhs)
    }
}
// with Y
impl From<Y> for Abstand {
    fn from(Y(input): Y) -> Self {
        Abstand(input)
    }
}
impl Add<Y> for Abstand {
    type Output = Y;

    fn add(self, Y(rhs): Y) -> Self::Output {
        Y(self.0 + rhs)
    }
}
// with CanvasRadius
impl From<Radius> for Abstand {
    fn from(Radius(input): Radius) -> Self {
        Abstand(input)
    }
}
impl Add<Radius> for Abstand {
    type Output = Radius;

    fn add(self, Radius(rhs): Radius) -> Self::Output {
        Radius(self.0 + rhs)
    }
}
// scale with f32
impl Mul<f32> for Abstand {
    type Output = Self;

    fn mul(self, rhs: f32) -> Abstand {
        Abstand(self.0 * rhs)
    }
}
impl Mul<Abstand> for f32 {
    type Output = Abstand;

    fn mul(self, Abstand(rhs): Abstand) -> Abstand {
        Abstand(self * rhs)
    }
}
impl MulAssign<f32> for Abstand {
    fn mul_assign(&mut self, rhs: f32) {
        self.0 *= rhs
    }
}
impl Div<f32> for Abstand {
    type Output = Self;

    fn div(self, rhs: f32) -> Abstand {
        Abstand(self.0 / rhs)
    }
}
impl DivAssign<f32> for Abstand {
    fn div_assign(&mut self, rhs: f32) {
        self.0 /= rhs
    }
}
/// Umrechnung von mm-Größen auf Canvas-Koordinaten
/// Verwenden dieser Funktion um evtl. in der Zukunft einen Faktor zu erlauben
impl From<super::Spurweite> for Abstand {
    fn from(super::Spurweite(spurweite): super::Spurweite) -> Self {
        Abstand::new_from_mm(spurweite)
    }
}
impl From<super::Length> for Abstand {
    fn from(super::Length(length): super::Length) -> Self {
        Abstand::new_from_mm(length)
    }
}
impl From<super::Radius> for Abstand {
    fn from(super::Radius(radius): super::Radius) -> Self {
        Abstand::new_from_mm(radius)
    }
}
pub trait ToAbstand: Into<Abstand> {
    fn to_abstand(self) -> Abstand {
        self.into()
    }
}
impl<T: Into<Abstand>> ToAbstand for T {}

/// Coordinate type safe variant of /iced::Point/
#[derive(Debug, PartialEq)]
pub struct Point {
    x: X,
    y: Y,
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
#[derive(Debug, PartialEq)]
pub struct Size {
    width: X,
    height: Y,
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
#[derive(Debug, PartialEq)]
pub struct Vector {
    dx: X,
    dy: Y,
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

/// Coordinate type safe variant of /iced::widget::canvas::path::Arc/
#[derive(Debug, PartialEq)]
pub struct Arc {
    center: Point,
    radius: Radius,
    start: Angle,
    end: Angle,
}
impl Arc {
    pub fn new(center: Point, radius: Radius, start: Angle, end: Angle) -> Self {
        Arc { center, radius, start, end }
    }
}
impl From<Arc> for canvas::path::Arc {
    fn from(Arc { center, radius, start, end }: Arc) -> Self {
        canvas::path::Arc {
            center: center.into(),
            radius: radius.0,
            start_angle: start.0,
            end_angle: end.0,
        }
    }
}

/// newtype auf einem /iced::widget::canvas::path::Builder/
///
/// Implementiert nur Methoden, die ich auch benötige.
/// Evtl. werden später weitere hinzugefügt.
/// Alle Methoden verwenden die hier definierten Typen.
pub struct PathBuilder(canvas::path::Builder);
impl PathBuilder {
    /// create a new PathBuilder
    pub fn new() -> Self {
        PathBuilder(canvas::path::Builder::new())
    }

    /// Finalize the Path, building the immutable result
    pub fn build(self) -> Path {
        self.0.build()
    }

    // start a new subpath at /point/
    pub fn move_to(&mut self, point: Point) {
        self.0.move_to(point.into())
    }

    /// strike a direct line from the current point to /point/
    pub fn line_to(&mut self, point: Point) {
        self.0.line_to(point.into())
    }

    /// Strike an arc around (xc,xy) with given radius from angle1 to angle2 (clockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph, this way the method
    /// doesn't strike a direct line from the current point to the start of the arc.
    pub fn arc(&mut self, arc: Arc /*, new_sub_path: bool*/) {
        // if new_sub_path {
        // TODO still required?
        // self.new_sub_path()
        // }
        self.0.arc(arc.into())
    }

    /// Strike an arc from /a/ to /b/ with given radius (clockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph before the arc.
    /// Otherwise, strike a direct line from the current point to the start of the arc.
    pub fn arc_to(&mut self, a: Point, b: Point, radius: Radius, new_sub_path: bool) {
        if new_sub_path {
            self.move_to(a)
        }
        self.0.arc_to(a.into(), b.into(), radius.0)
    }

    /// strike a direct line from the current point to the start of the last subpath
    pub fn close(&mut self) {
        self.0.close()
    }
}

pub struct Frame<'t>(&'t mut canvas::Frame);
impl<'t> Frame<'t> {
    pub fn new(frame: &'t mut canvas::Frame) -> Self {
        Frame(frame)
    }

    /// Draws the stroke of the given Path on the Frame with the provided style.
    pub fn stroke(&mut self, path: &Path, stroke: impl Into<Stroke>) {
        self.0.stroke(path, stroke)
    }

    /// Draws the given Path on the Frame by filling it with the provided style.
    pub fn fill(&mut self, path: &Path, fill: impl Into<Fill>) {
        self.0.fill(path, fill)
    }

    /// Draws the characters of the given Text on the Frame, filling them with the given color.
    ///
    /// **Warning:** problems regarding transformation/rotation/scaling from /iced::widget::canvas::Frame/ apply here as well!
    pub fn fill_text(&mut self, text: impl Into<Text>) {
        self.0.fill_text(text)
    }

    /// Stores the current transform of the Frame and executes the given drawing operations, restoring the transform afterwards.
    ///
    /// This method is useful to compose transforms and perform drawing operations in different coordinate systems.
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        self.0.with_save(|frame| action(&mut Frame(frame)))
    }

    /// Applies a translation to the current transform of the Frame.
    pub fn translate(&mut self, vector: Vector) {
        self.0.translate(vector.into())
    }

    /// Applies a rotation to the current transform of the Frame.
    pub fn rotate(&mut self, angle: Angle) {
        self.0.rotate(angle.0)
    }

    /// Applies a scaling to the current transform of the Frame.
    pub fn scale(&mut self, scale: f32) {
        self.0.scale(scale)
    }
}

/*
///////////////////////////////////////////////////////////////////////////
// old gtk/cairo based implementation

/// newtype auf einen cairo-Context
///
/// Only implements the methods I need, might add others later.
/// All methods only work with corresponding Canvas..-Types
#[derive(Debug)]
pub struct Cairo<'t>(&'t Context);

impl<'t> Cairo<'t> {
    pub fn new(c: &'t Context) -> Cairo<'t> {
        Cairo(c)
    }

    pub fn move_to(&mut self, x: CanvasX, y: CanvasY) {
        self.0.move_to(x.0, y.0)
    }
    pub fn rel_move_to(&mut self, dx: CanvasX, dy: CanvasY) {
        self.0.rel_move_to(dx.0, dy.0)
    }

    pub fn line_to(&mut self, x: CanvasX, y: CanvasY) {
        self.0.line_to(x.0, y.0)
    }
    pub fn rel_line_to(&mut self, dx: CanvasX, dy: CanvasY) {
        self.0.rel_line_to(dx.0, dy.0)
    }

    /// Strike an arc around (xc,xy) with given radius from angle1 to angle2 (clockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph, this way the method
    /// doesn't strike a direct line from the current point to the start of the arc.
    pub fn arc(
        &mut self,
        xc: CanvasX,
        yc: CanvasY,
        radius: CanvasRadius,
        angle1: Angle,
        angle2: Angle,
        new_sub_path: bool,
    ) {
        if new_sub_path {
            self.new_sub_path()
        }
        self.0.arc(xc.0, yc.0, radius.0, angle1.0 as f64, angle2.0 as f64)
    }

    /// Strike an arc around (xc,xy) with given radius from angle1 to angle2 (counterclockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph, this way the method
    /// doesn't strike a direct line from the current point to the start of the arc!
    pub fn arc_negative(
        &mut self,
        xc: CanvasX,
        yc: CanvasY,
        radius: CanvasRadius,
        angle1: Angle,
        angle2: Angle,
        new_sub_path: bool,
    ) {
        if new_sub_path {
            self.new_sub_path()
        }
        self.0.arc_negative(xc.0, yc.0, radius.0, angle1.0 as f64, angle2.0 as f64)
    }

    pub fn new_path(&mut self) {
        self.0.new_path()
    }

    pub fn new_sub_path(&mut self) {
        self.0.new_sub_path()
    }

    pub fn close_path(&mut self) {
        self.0.close_path()
    }

    pub fn stroke(&mut self) {
        self.0.stroke()
    }

    pub fn stroke_preserve(&mut self) {
        self.0.stroke_preserve()
    }

    pub fn fill(&mut self) {
        self.0.fill()
    }

    pub fn fill_preserve(&mut self) {
        self.0.fill_preserve()
    }

    /// perform a /save/ before and a /restore/ after action
    pub fn with_save_restore<F: FnOnce(&mut Self)>(&mut self, action: F) {
        self.0.save().expect("Error in cairo::Context::save");
        action(self);
        self.0.restore().expect("Error in cairo::Context::restore");
    }

    pub fn translate(&mut self, tx: CanvasX, ty: CanvasY) {
        self.0.translate(tx.0, ty.0)
    }

    pub fn rotate(&mut self, angle: Angle) {
        self.0.rotate(angle.0 as f64)
    }

    pub fn transform(&mut self, matrix: Matrix) {
        self.0.transform(matrix)
    }

    pub fn set_source_rgb(&mut self, red: f64, green: f64, blue: f64) {
        self.0.set_source_rgb(red, green, blue)
    }
    pub fn set_source_rgba(&mut self, red: f64, green: f64, blue: f64, alpha: f64) {
        self.0.set_source_rgba(red, green, blue, alpha)
    }
}

pub trait ZeichnenCairo
where
    Self::AnchorPoints: anchor::Lookup<Self::AnchorName>,
{
    /// Maximale Breite
    fn width(&self) -> u64;

    /// Maximale Höhe
    fn height(&self) -> u64;

    /// Erzeuge den Pfad für Darstellung der Linien.
    ///
    /// Der Kontext wurde bereits für eine Darstellung in korrekter Position transformiert.
    /// /cairo.stroke()/ wird nachfolgend aufgerufen.
    fn zeichne(&self, cairo: &mut Cairo);

    /// Erzeuge einen Pfad zum Einfärben für Darstellung des Streckenabschnittes.
    ///
    /// Der Kontext wurde bereits für eine Darstellung in korrekter Position transformiert.
    /// /cairo.fill()/ wird nachfolgend aufgerufen.
    fn fuelle(&self, cairo: &mut Cairo);

    /// Identifier for AnchorPoints.
    /// An enum is advised, but others work as well.
    ///
    /// Since they are used as keys in an HashMap, Hash+Eq must be implemented (derived).
    type AnchorName;
    /// Storage Type for AnchorPoints, should implement /AnchorLookup<Self::AnchorName>/.
    type AnchorPoints;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> Self::AnchorPoints;
}

/// Horizontale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct CanvasX(pub f64);
impl Add<CanvasAbstand> for CanvasX {
    type Output = CanvasX;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> CanvasX {
        CanvasX(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasX {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasX {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasX(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasX {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
impl Neg for CanvasX {
    type Output = CanvasX;

    fn neg(self) -> Self {
        CanvasX(-self.0)
    }
}
/// Vertikale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct CanvasY(pub f64);
impl Add<CanvasAbstand> for CanvasY {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasY(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasY {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasY {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasY(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasY {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
impl Neg for CanvasY {
    type Output = CanvasY;

    fn neg(self) -> Self {
        CanvasY(-self.0)
    }
}
/// Radius auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CanvasRadius(pub f64);
impl Add<CanvasAbstand> for CanvasRadius {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasRadius(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasRadius {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasRadius {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasRadius(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasRadius {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
/// Abstand/Länge auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct CanvasAbstand(f64);
impl CanvasAbstand {
    const fn new_from_mm(abstand_mm: f64) -> CanvasAbstand {
        CanvasAbstand(abstand_mm)
    }

    pub fn max(&self, other: &CanvasAbstand) -> CanvasAbstand {
        CanvasAbstand(self.0.max(other.0))
    }

    /// Anzahl benötigter Pixel um den CanvasAbstand darstellen zu können
    pub fn pixel(&self) -> u64 {
        self.0.ceil() as u64
    }
}
// with Self
impl Add<CanvasAbstand> for CanvasAbstand {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasAbstand(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasAbstand {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasAbstand {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasAbstand(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasAbstand {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
// get ratio
impl Div<CanvasAbstand> for CanvasAbstand {
    type Output = f64;
    fn div(self, rhs: Self) -> Self::Output {
        self.0 / rhs.0
    }
}
// with CanvasX
impl From<CanvasX> for CanvasAbstand {
    fn from(CanvasX(input): CanvasX) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<CanvasX> for CanvasAbstand {
    type Output = CanvasX;

    fn add(self, CanvasX(rhs): CanvasX) -> CanvasX {
        CanvasX(self.0 + rhs)
    }
}
// with CanvasY
impl From<CanvasY> for CanvasAbstand {
    fn from(CanvasY(input): CanvasY) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<CanvasY> for CanvasAbstand {
    type Output = CanvasY;

    fn add(self, CanvasY(rhs): CanvasY) -> CanvasY {
        CanvasY(self.0 + rhs)
    }
}
// with CanvasRadius
impl From<CanvasRadius> for CanvasAbstand {
    fn from(CanvasRadius(input): CanvasRadius) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<CanvasRadius> for CanvasAbstand {
    type Output = CanvasRadius;

    fn add(self, CanvasRadius(rhs): CanvasRadius) -> CanvasRadius {
        CanvasRadius(self.0 + rhs)
    }
}
// scale with f64
impl Mul<f64> for CanvasAbstand {
    type Output = CanvasAbstand;

    fn mul(self, rhs: f64) -> CanvasAbstand {
        CanvasAbstand(self.0 * rhs)
    }
}
impl Mul<CanvasAbstand> for f64 {
    type Output = CanvasAbstand;

    fn mul(self, CanvasAbstand(rhs): CanvasAbstand) -> CanvasAbstand {
        CanvasAbstand(self * rhs)
    }
}
impl MulAssign<f64> for CanvasAbstand {
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs
    }
}
impl Div<f64> for CanvasAbstand {
    type Output = CanvasAbstand;

    fn div(self, rhs: f64) -> CanvasAbstand {
        CanvasAbstand(self.0 / rhs)
    }
}
impl DivAssign<f64> for CanvasAbstand {
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs
    }
}
/// Umrechnung von mm-Größen auf Canvas-Koordinaten
/// Verwenden dieser Funktion um evtl. in der Zukunft einen Faktor zu erlauben
impl From<super::Spurweite> for CanvasAbstand {
    fn from(super::Spurweite(spurweite): super::Spurweite) -> CanvasAbstand {
        CanvasAbstand::new_from_mm(spurweite as f64)
    }
}
impl From<super::Length> for CanvasAbstand {
    fn from(super::Length(length): super::Length) -> CanvasAbstand {
        CanvasAbstand::new_from_mm(length as f64)
    }
}
impl From<super::Radius> for CanvasAbstand {
    fn from(super::Radius(radius): super::Radius) -> CanvasAbstand {
        CanvasAbstand::new_from_mm(radius as f64)
    }
}
*/
