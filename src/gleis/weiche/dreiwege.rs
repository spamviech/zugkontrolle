//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::gerade::{Richtung, Weiche};
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
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
        let size_gerade = Gerade { zugtyp, length }.size();
        let size_kurve = Kurve { zugtyp, radius, angle }.size();
        let height_kurven = 2. * size_kurve.height.to_abstand() - beschraenkung::<Z, canvas::Y>();
        canvas::Size {
            width: canvas::X(0.)
                + size_gerade.width.to_abstand().max(&size_kurve.width.to_abstand()),
            height: canvas::Y(0.) + size_gerade.height.to_abstand().max(&height_kurven),
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        /*
        let DreiwegeWeiche { zugtyp, length, radius, angle } = *self;
        let half_width: canvas::X = canvas::X(0.5 * self.width() as f64);
        let half_height: canvas::Y = canvas::Y(0.5 * self.height() as f64);
        let start_width: canvas::X = canvas::X(0.);
        let start_height: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
        // Weiche mit Abzweigung Rechts
        cairo.translate(start_width, start_height);
        Weiche { zugtyp, length, radius, angle, direction: Richtung::Rechts }.zeichne(cairo);
        cairo.translate(-start_width, -start_height);
        // Abzweigung Links
        cairo.translate(half_width, half_height);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        cairo.translate(start_width, start_height);
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Ende);
        */
        println!("TODO DreiwegeWeiche");
        vec![]
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        //TODO
        println!("TODO fülle DreiwegeWeiche");
        vec![]
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
            anfang: anchor::Point {
                position: anchor::Position { x: anfang_x, y: half_height },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(0.) },
            },
            gerade: anchor::Point {
                position: anchor::Position { x: anfang_x + length, y: half_height },
                direction: anchor::Direction { dx: canvas::X(1.), dy: canvas::Y(0.) },
            },
            links: anchor::Point {
                position: anchor::Position {
                    x: anfang_x + radius_x * self.angle.sin(),
                    y: half_height + radius_y * (1. - self.angle.cos()),
                },
                direction: anchor::Direction {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(self.angle.sin()),
                },
            },
            rechts: anchor::Point {
                position: anchor::Position {
                    x: anfang_x + radius_x * self.angle.sin(),
                    y: half_height - radius_y * (1. - self.angle.cos()),
                },
                direction: anchor::Direction {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(-self.angle.sin()),
                },
            },
        }
    }
}
