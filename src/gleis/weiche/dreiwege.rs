//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::gerade::{Weiche, WeichenRichtung};
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;
use crate::gleis::widget::Zeichnen;

/// Definition einer Dreiwege-Weiche
#[derive(Debug, Clone)]
pub struct DreiwegeWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
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

    fn width(&self) -> u64 {
        let DreiwegeWeiche { zugtyp, length, radius, angle } = *self;
        let width_gerade = Gerade { zugtyp, length }.width();
        let width_kurve = Kurve { zugtyp, radius, angle }.width();
        width_gerade.max(width_kurve)
    }

    fn height(&self) -> u64 {
        let DreiwegeWeiche { zugtyp, length, radius, angle } = *self;
        let height_gerade = Gerade { zugtyp, length }.height();
        let height_kurven =
            2 * Kurve { zugtyp, radius, angle }.height() - beschraenkung::<Z>().pixel();
        height_gerade.max(height_kurven)
    }

    fn zeichne(&self, cairo: &Cairo) {
        let DreiwegeWeiche { zugtyp, length, radius, angle } = *self;
        let half_width: CanvasX = CanvasX(0.5 * self.width() as f64);
        let half_height: CanvasY = CanvasY(0.5 * self.height() as f64);
        let start_width: CanvasX = CanvasX(0.);
        let start_height: CanvasY = half_height - 0.5 * beschraenkung::<Z>();
        // Weiche mit Abzweigung Rechts
        cairo.translate(start_width, start_height);
        Weiche { zugtyp, length, radius, angle, direction: WeichenRichtung::Rechts }.zeichne(cairo);
        cairo.translate(-start_width, -start_height);
        // Abzweigung Links
        cairo.translate(half_width, half_height);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        cairo.translate(start_width, start_height);
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Ende);
    }

    fn anchor_points(&self) -> AnchorPoints {
        let height: CanvasY = CanvasY(self.height() as f64);
        let half_height: CanvasY = CanvasY(0.) + 0.5 * CanvasAbstand::from(height);
        let length: CanvasAbstand = self.length.into();
        let radius: CanvasAbstand = self.radius.into();
        let anfang_x: CanvasX = CanvasX(0.);
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position { x: anfang_x, y: half_height },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            gerade: anchor::Point {
                position: anchor::Position { x: anfang_x + length, y: half_height },
                direction: anchor::Direction { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
            links: anchor::Point {
                position: anchor::Position {
                    x: anfang_x + radius * self.angle.sin(),
                    y: half_height + radius * (1. - self.angle.cos()),
                },
                direction: anchor::Direction {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(self.angle.sin()),
                },
            },
            rechts: anchor::Point {
                position: anchor::Position {
                    x: anfang_x + radius * self.angle.sin(),
                    y: half_height - radius * (1. - self.angle.cos()),
                },
                direction: anchor::Direction {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(-self.angle.sin()),
                },
            },
        }
    }
}
