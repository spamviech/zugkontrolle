//! newtypes für einen cairo::Context

use std::f32::consts::PI;
use std::{convert::From, marker::PhantomData};

use iced;
// re-exports
pub use iced::{
    canvas::{Fill, FillRule, Stroke, Text},
    Color,
    HorizontalAlignment,
    VerticalAlignment,
};

use super::winkel::Winkel;

pub mod koordinaten;
pub use koordinaten::*;

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
    Rotate(Winkel),
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
impl From<Winkel> for Inverted<Winkel, X> {
    fn from(winkel: Winkel) -> Self {
        Inverted(Winkel::new(PI) - winkel, PhantomData)
    }
}
impl From<Winkel> for Inverted<Winkel, Y> {
    fn from(winkel: Winkel) -> Self {
        Inverted(-winkel, PhantomData)
    }
}
impl From<Arc> for Inverted<Arc, X> {
    fn from(arc: Arc) -> Self {
        Inverted(
            Arc {
                center: Inverted::<Point, X>::from(arc.center).0,
                start: Inverted::<Winkel, X>::from(arc.start).0,
                end: Inverted::<Winkel, X>::from(arc.end).0,
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
                start: Inverted::<Winkel, Y>::from(arc.start).0,
                end: Inverted::<Winkel, Y>::from(arc.end).0,
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
    pub fn arc(&mut self, arc: A /* , new_sub_path: bool */) {
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
    /// **Warning:** problems regarding transformation/rotation/scaling from /iced::canvas::Frame/
    /// apply here as well!
    pub fn fill_text(&mut self, text: impl Into<Text>) {
        self.0.fill_text(text)
    }

    /// Stores the current transform of the Frame and executes the given drawing operations,
    /// restoring the transform afterwards.
    ///
    /// This method is useful to compose transforms and perform drawing operations in different
    /// coordinate systems.
    pub fn with_save(&mut self, action: impl for<'s> FnOnce(&'s mut Frame<'s>)) {
        self.0.with_save(|frame| action(&mut Frame(frame)))
    }

    /// Wende die übergebene Transformation auf den Frame an.
    pub fn transformation(&mut self, transformation: &Transformation) {
        match transformation {
            Transformation::Translate(vector) => self.0.translate((*vector).into()),
            Transformation::Rotate(winkel) => self.0.rotate(winkel.0),
            Transformation::Scale(scale) => self.0.scale(*scale),
        }
    }
}

#[derive(Debug)]
pub struct Cache(iced::canvas::Cache);
impl Cache {
    pub fn new() -> Self {
        Cache(iced::canvas::Cache::new())
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    pub fn draw(&self, bounds: Size, draw_fn: impl Fn(&mut Frame)) -> iced::canvas::Geometry {
        self.0.draw(bounds.into(), |frame| draw_fn(&mut Frame(frame)))
    }
}

mod vektor;
pub use vektor::Vektor;
