//! newtypes für einen cairo::Context

use std::convert::From;
use std::f32::consts::PI;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use iced::{self, canvas};
// use cairo::Context;
// pub use cairo::Matrix;

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
// with canvas::Radius
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

/// Coordinate type safe variant of /iced::widget::canvas::path::Arc/
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

/// Pfad auf dem Canvas
///
/// Transformationen werden ausgeführt, bevor der Pfad gezeichnet/gefüllt wird!
pub struct Path {
    path: canvas::Path,
    transformations: Vec<Transformation>,
}

/// Unterstützte Transformationen
pub enum Transformation {
    /// Verschiebe alle Koordinaten um den übergebenen Vector.
    Translate(Vector),
    /// Rotiere alle Koordinaten um den Ursprung (im Uhrzeigersinn)
    Rotate(Angle),
    /// Skaliere alle Koordinaten (x',y') = (x*scale, y*scale)
    Scale(f32),
}
/// newtype auf einem /iced::widget::canvas::path::Builder/
///
/// Implementiert nur Methoden, die ich auch benötige.
/// Evtl. werden später weitere hinzugefügt.
/// Alle Methoden verwenden die hier definierten Typen.
pub struct PathBuilder {
    builder: canvas::path::Builder,
    transformations: Vec<Transformation>,
    invert_x: bool,
    invert_y: bool,
}
/// Helper struct, so I don't make stupid mistakes (e.g. inverting twice)
struct Inverted<T>(T);
impl<T> Inverted<T> {
    fn into<S: From<T>>(self) -> S {
        self.0.into()
    }
}
impl PathBuilder {
    /// create a new PathBuilder
    pub fn new() -> Self {
        PathBuilder::new_with_transformations(Vec::new())
    }
    /// create a new PathBuilder for a path after applying all given transformations
    pub fn new_with_transformations(transformations: Vec<Transformation>) -> Self {
        PathBuilder {
            builder: canvas::path::Builder::new(),
            transformations,
            invert_x: false,
            invert_y: false,
        }
    }

    /// Finalize the Path, building the immutable result
    pub fn build(self) -> Path {
        Path { path: self.builder.build(), transformations: self.transformations }
    }

    fn invert_point_axis(&self, Point { x, y }: Point) -> Inverted<Point> {
        Inverted(Point {
            x: if self.invert_x { -x } else { x },
            y: if self.invert_y { -y } else { y },
        })
    }
    fn invert_angle_axis(&self, angle: Angle) -> Inverted<Angle> {
        let inverted_x = if self.invert_x { Angle::new(PI) - angle } else { angle };
        Inverted(if self.invert_y { -inverted_x } else { inverted_x })
    }
    fn invert_arc_axis(&self, Arc { center, radius, start, end }: Arc) -> Inverted<Arc> {
        Inverted(Arc {
            center: self.invert_point_axis(center).0,
            radius,
            start: self.invert_angle_axis(start).0,
            end: self.invert_angle_axis(end).0,
        })
    }

    // start a new subpath at /point/
    pub fn move_to(&mut self, point: Point) {
        self.builder.move_to(self.invert_point_axis(point).into())
    }

    /// strike a direct line from the current point to /point/
    pub fn line_to(&mut self, point: Point) {
        self.builder.line_to(self.invert_point_axis(point).into())
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
        self.builder.arc(self.invert_arc_axis(arc).into())
    }

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
            -radius.0,
        )
    }

    /// strike a direct line from the current point to the start of the last subpath
    pub fn close(&mut self) {
        self.builder.close()
    }

    /// Alle Methoden der closure verwenden eine gespiegelte x-Achse (x',y') = (-x,y)
    ///
    /// **ACHTUNG:** /arc_to/ hat den Bogen vmtl. in der falschen Richtung.
    pub fn with_invert_x(&mut self, action: impl for<'s> FnOnce(&'s mut Self)) {
        self.invert_x = !self.invert_x;
        action(self);
        self.invert_x = !self.invert_x;
    }

    /// Alle Methoden der closure verwenden eine gespiegelte y-Achse (x',y') = (x,-y)
    ///
    /// **ACHTUNG:** /arc_to/ hat den Bogen vmtl. in der falschen Richtung.
    pub fn with_invert_y(&mut self, action: impl for<'s> FnOnce(&'s mut Self)) {
        self.invert_y = !self.invert_y;
        action(self);
        self.invert_y = !self.invert_y;
    }
}

pub struct Frame<'t>(&'t mut canvas::Frame);
impl<'t> Frame<'t> {
    pub fn new(frame: &'t mut canvas::Frame) -> Self {
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

    /// Wende die übergebene Transformation auf den Frame an.
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translate(vector) => self.0.translate((*vector).into()),
            Transformation::Rotate(angle) => self.0.rotate(angle.0),
            Transformation::Scale(scale) => self.0.scale(*scale),
        }
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

    pub fn move_to(&mut self, x: canvas::X, y: canvas::Y) {
        self.0.move_to(x.0, y.0)
    }
    pub fn rel_move_to(&mut self, dx: canvas::X, dy: canvas::Y) {
        self.0.rel_move_to(dx.0, dy.0)
    }

    pub fn line_to(&mut self, x: canvas::X, y: canvas::Y) {
        self.0.line_to(x.0, y.0)
    }
    pub fn rel_line_to(&mut self, dx: canvas::X, dy: canvas::Y) {
        self.0.rel_line_to(dx.0, dy.0)
    }

    /// Strike an arc around (xc,xy) with given radius from angle1 to angle2 (clockwise).
    ///
    /// If /move_to/ is /true/ start a new subgraph, this way the method
    /// doesn't strike a direct line from the current point to the start of the arc.
    pub fn arc(
        &mut self,
        xc: canvas::X,
        yc: canvas::Y,
        radius: canvas::Radius,
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
        xc: canvas::X,
        yc: canvas::Y,
        radius: canvas::Radius,
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

    pub fn translate(&mut self, tx: canvas::X, ty: canvas::Y) {
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
pub struct canvas::X(pub f64);
impl Add<CanvasAbstand> for canvas::X {
    type Output = canvas::X;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> canvas::X {
        canvas::X(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for canvas::X {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for canvas::X {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        canvas::X(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for canvas::X {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
impl Neg for canvas::X {
    type Output = canvas::X;

    fn neg(self) -> Self {
        canvas::X(-self.0)
    }
}
/// Vertikale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub struct canvas::Y(pub f64);
impl Add<CanvasAbstand> for canvas::Y {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        canvas::Y(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for canvas::Y {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for canvas::Y {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        canvas::Y(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for canvas::Y {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
impl Neg for canvas::Y {
    type Output = canvas::Y;

    fn neg(self) -> Self {
        canvas::Y(-self.0)
    }
}
/// Radius auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct canvas::Radius(pub f64);
impl Add<CanvasAbstand> for canvas::Radius {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        canvas::Radius(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for canvas::Radius {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for canvas::Radius {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        canvas::Radius(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for canvas::Radius {
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
// with canvas::X
impl From<canvas::X> for CanvasAbstand {
    fn from(canvas::X(input): canvas::X) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<canvas::X> for CanvasAbstand {
    type Output = canvas::X;

    fn add(self, canvas::X(rhs): canvas::X) -> canvas::X {
        canvas::X(self.0 + rhs)
    }
}
// with canvas::Y
impl From<canvas::Y> for CanvasAbstand {
    fn from(canvas::Y(input): canvas::Y) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<canvas::Y> for CanvasAbstand {
    type Output = canvas::Y;

    fn add(self, canvas::Y(rhs): canvas::Y) -> canvas::Y {
        canvas::Y(self.0 + rhs)
    }
}
// with canvas::Radius
impl From<canvas::Radius> for CanvasAbstand {
    fn from(canvas::Radius(input): canvas::Radius) -> Self {
        CanvasAbstand(input)
    }
}
impl Add<canvas::Radius> for CanvasAbstand {
    type Output = canvas::Radius;

    fn add(self, canvas::Radius(rhs): canvas::Radius) -> canvas::Radius {
        canvas::Radius(self.0 + rhs)
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
