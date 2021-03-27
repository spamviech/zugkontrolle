//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f64::consts::PI;
use std::marker::PhantomData;

use super::gerade::{AnchorName, AnchorPoints, WeichenRichtung};
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;
use crate::gleis::widget::Zeichnen;

/// Definition einer Weiche mit S-Kurve
#[derive(Debug, Clone)]
pub struct SKurveWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub angle_reverse: AngleDegrees,
    pub direction: WeichenRichtung,
}

impl<Z: Zugtyp> Zeichnen for SKurveWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn width(&self) -> u64 {
        let SKurveWeiche { zugtyp, length, radius, angle, angle_reverse, direction: _ } = *self;
        let width_gerade = Gerade { zugtyp, length }.width();
        let factor = if self.angle.abs() < Angle::new(0.5 * PI) { self.angle.sin() } else { 1. };
        let width_s_kurve = (beschraenkung::<Z>() * factor).pixel();
        panic!();
        width_gerade.max(width_s_kurve)
    }

    fn height(&self) -> u64 {
        let SKurveWeiche { zugtyp, length, radius, angle, angle_reverse, direction: _ } = *self;
        panic!();
        Kurve { zugtyp, radius, angle }.width()
    }

    fn zeichne(&self, cairo: &Cairo) {
        let SKurveWeiche { zugtyp, length, radius, angle, angle_reverse, direction } = *self;
        if direction == WeichenRichtung::Links {
            // spiegel y-Achse in der Mitte
            let x = CanvasX(0.);
            let half_height = CanvasY(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        panic!();
        Gerade { zugtyp, length }.zeichne(cairo);
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Ende);
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: CanvasY;
        let multiplier: f64;
        match self.direction {
            WeichenRichtung::Links => {
                start_height = CanvasY(0.);
                multiplier = 1.;
            }
            WeichenRichtung::Rechts => {
                start_height = CanvasY(self.height() as f64);
                multiplier = -1.;
            }
        };
        panic!();
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(multiplier * 0.) },
            },
            gerade: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.) + CanvasAbstand::from(self.length),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: CanvasX(1.), dy: CanvasY(multiplier * 0.) },
            },
            kurve: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.) + self.angle.sin() * CanvasAbstand::from(self.radius),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + CanvasAbstand::from(self.radius) * (1. - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(multiplier * self.angle.sin()),
                },
            },
        }
    }
}
