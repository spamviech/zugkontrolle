//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::gleis::anchor;
use crate::gleis::gerade::{self, Gerade};
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;

/// Definition einer Dreiwege-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct DreiwegeWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: Angle,
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
        let DreiwegeWeiche { zugtyp, length, radius, angle } = *self;
        let size_gerade = Gerade { zugtyp, length, description: None }.size();
        let size_kurve = Kurve { zugtyp, radius, angle }.size();
        let height_kurven = 2. * size_kurve.height.to_abstand() - beschraenkung::<Z>();
        canvas::Size {
            width: canvas::X(0.)
                + size_gerade.width.to_abstand().max(&size_kurve.width.to_abstand()),
            height: canvas::Y(0.) + size_gerade.height.to_abstand().max(&height_kurven),
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let size: canvas::Size = self.size();
        let start_x: canvas::X = canvas::X(0.);
        let height: canvas::Y = size.height;
        let half_height: canvas::Y = canvas::Y(0.5 * height.0);
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
            self.length,
            true,
            rechts_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        // Rechts
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.angle,
            kurve::Beschraenkung::Ende,
            rechts_transformations,
            canvas::PathBuilder::with_normal_axis,
        ));
        // Links
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.angle,
            kurve::Beschraenkung::Ende,
            links_transformations,
            canvas::PathBuilder::with_invert_y,
        ));
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let size: canvas::Size = self.size();
        let start_x: canvas::X = canvas::X(0.);
        let height: canvas::Y = size.height;
        let half_height: canvas::Y = canvas::Y(0.5 * height.0);
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
            self.length,
            rechts_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        // Rechts
        paths.push(kurve::fuelle(
            self.zugtyp,
            self.radius,
            self.angle,
            rechts_transformations,
            canvas::PathBuilder::with_normal_axis,
        ));
        // Links
        paths.push(kurve::fuelle(
            self.zugtyp,
            self.radius,
            self.angle,
            links_transformations,
            canvas::PathBuilder::with_invert_y,
        ));
        // return value
        paths
    }

    fn anchor_points(&self) -> AnchorPoints {
        let height: canvas::Y = self.size().height;
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height.to_abstand();
        let length: canvas::Abstand<canvas::X> = self.length.to_abstand();
        let radius: canvas::Abstand<canvas::Radius> = self.radius.to_abstand();
        let radius_x: canvas::Abstand<canvas::X> = radius.convert();
        let radius_y: canvas::Abstand<canvas::Y> = radius.convert();
        let anfang_x: canvas::X = canvas::X(0.);
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point { x: anfang_x, y: half_height },
                direction: canvas::Vector { dx: canvas::X(-1.), dy: canvas::Y(0.) },
            },
            gerade: anchor::Anchor {
                position: canvas::Point { x: anfang_x + length, y: half_height },
                direction: canvas::Vector { dx: canvas::X(1.), dy: canvas::Y(0.) },
            },
            links: anchor::Anchor {
                position: canvas::Point {
                    x: anfang_x + radius_x * self.angle.sin(),
                    y: half_height + radius_y * (1. - self.angle.cos()),
                },
                direction: canvas::Vector {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(self.angle.sin()),
                },
            },
            rechts: anchor::Anchor {
                position: canvas::Point {
                    x: anfang_x + radius_x * self.angle.sin(),
                    y: half_height - radius_y * (1. - self.angle.cos()),
                },
                direction: canvas::Vector {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(-self.angle.sin()),
                },
            },
        }
    }
}
