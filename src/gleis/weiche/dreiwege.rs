//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::gleis::types::*;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Dreiwege-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct DreiwegeWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub laenge: canvas::Abstand<canvas::X>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Angle,
    pub beschreibung: Option<&'static str>,
}
impl<Z> DreiwegeWeiche<Z> {
    pub const fn new(length: Length, radius: Radius, angle: Angle) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            beschreibung: None,
        }
    }
    pub const fn new_with_description(
        length: Length,
        radius: Radius,
        angle: Angle,
        description: &'static str,
    ) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            beschreibung: Some(description),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Links,
    Rechts,
}
impl<Z: Zugtyp> Zeichnen for DreiwegeWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> canvas::Size {
        let DreiwegeWeiche { laenge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(laenge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        let height_kurven = 2. * size_kurve.height - beschraenkung::<Z>();
        canvas::Size {
            width: size_gerade.width.max(&size_kurve.width),
            height: size_gerade.height.max(&height_kurven),
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let start_x: canvas::X = canvas::X(0.);
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
        let mut paths = Vec::new();
        let rechts_transformations =
            vec![canvas::Transformation::Translate(canvas::Vector::new(start_x, start_y))];
        let links_transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
            start_x,
            start_y + beschraenkung::<Z>(),
        ))];
        // Gerade
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.laenge,
            true,
            rechts_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        // Rechts
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschraenkung::Ende,
            rechts_transformations,
            canvas::PathBuilder::with_normal_axis,
        ));
        // Links
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschraenkung::Ende,
            links_transformations,
            canvas::PathBuilder::with_invert_y,
        ));
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let start_x: canvas::X = canvas::X(0.);
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
        let mut paths = Vec::new();
        let rechts_transformations =
            vec![canvas::Transformation::Translate(canvas::Vector::new(start_x, start_y))];
        let links_transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
            start_x,
            start_y + beschraenkung::<Z>(),
        ))];
        // Gerade
        paths.push(gerade::fuelle(
            self.zugtyp,
            self.laenge,
            rechts_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        // Rechts
        paths.push(kurve::fuelle(
            self.zugtyp,
            self.radius,
            self.winkel,
            rechts_transformations,
            canvas::PathBuilder::with_normal_axis,
        ));
        // Links
        paths.push(kurve::fuelle(
            self.zugtyp,
            self.radius,
            self.winkel,
            links_transformations,
            canvas::PathBuilder::with_invert_y,
        ));
        // return value
        paths
    }

    fn beschreibung(&self) -> Option<(canvas::Position, &'static str)> {
        self.beschreibung.map(|text| {
            let start_x: canvas::X = canvas::X(0.);
            let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
            let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
            (
                canvas::Position {
                    point: canvas::Point::new(
                        start_x + 0.5 * self.laenge,
                        start_y + 0.5 * beschraenkung::<Z>(),
                    ),
                    winkel: Angle::new(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: canvas::Vector) -> bool {
        //TODO
        println!("TODO innerhalb DreiwegeWeiche");
        false
    }

    fn anchor_points(&self) -> AnchorPoints {
        let height: canvas::Abstand<canvas::Y> = self.size().height;
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height;
        let length: canvas::Abstand<canvas::X> = self.laenge;
        let radius: canvas::Abstand<canvas::Radius> = self.radius;
        let radius_x: canvas::Abstand<canvas::X> = radius.as_x();
        let radius_y: canvas::Abstand<canvas::Y> = radius.as_y();
        let anfang_x: canvas::X = canvas::X(0.);
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point { x: anfang_x, y: half_height },
                direction: canvas::Vector::new(canvas::X(-1.), canvas::Y(0.)),
            },
            gerade: anchor::Anchor {
                position: canvas::Point { x: anfang_x + length, y: half_height },
                direction: canvas::Vector::new(canvas::X(1.), canvas::Y(0.)),
            },
            links: anchor::Anchor {
                position: canvas::Point {
                    x: anfang_x + radius_x * self.winkel.sin(),
                    y: half_height + radius_y * (1. - self.winkel.cos()),
                },
                direction: canvas::Vector::new(
                    canvas::X(self.winkel.cos()),
                    canvas::Y(self.winkel.sin()),
                ),
            },
            rechts: anchor::Anchor {
                position: canvas::Point {
                    x: anfang_x + radius_x * self.winkel.sin(),
                    y: half_height - radius_y * (1. - self.winkel.cos()),
                },
                direction: canvas::Vector::new(
                    canvas::X(self.winkel.cos()),
                    canvas::Y(-self.winkel.sin()),
                ),
            },
        }
    }
}
