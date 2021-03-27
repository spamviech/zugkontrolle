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
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>angle) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[derive(Debug, Clone)]
pub struct SKurveWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub radius_reverse: Radius,
    pub angle_reverse: AngleDegrees,
    pub direction: WeichenRichtung,
}

impl<Z: Zugtyp> Zeichnen for SKurveWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    // x(phi) = radius * cos(phi)
    //          + if (phi>angle) {radius_reverse * (sin(angle-phi)-sin(angle))} else {0}
    // y(phi) = radius * (1 - sin(phi))
    //          + if (phi>angle_reverse) {radius_reverse * (cos(angle)-cos(angle-phi))} else {0}
    fn width(&self) -> u64 {
        let SKurveWeiche {
            zugtyp,
            length,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction: _,
        } = *self;
        let width_gerade = Gerade { zugtyp, length }.width();
        let factor = if angle.abs() < Angle::new(0.5 * PI) { angle.cos() } else { 1. };
        let factor_reverse = if (angle_reverse - angle).abs() < Angle::new(0.5 * PI) {
            (angle - angle_reverse).sin() - angle.sin()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen = radius_begrenzung_aussen::<Z>(radius);
        let radius_innen = radius_begrenzung_innen::<Z>(radius);
        let radius_reverse_aussen = radius_begrenzung_aussen::<Z>(radius_reverse);
        let radius_reverse_innen = radius_begrenzung_innen::<Z>(radius_reverse);
        // obere Beschränkung
        let width_oben1: CanvasAbstand = CanvasAbstand::from(radius_aussen) * factor;
        let width_oben2: CanvasAbstand = CanvasAbstand::from(radius_aussen) * angle.cos()
            + CanvasAbstand::from(radius_reverse_innen) * factor_reverse;
        let width_oben: CanvasAbstand = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1 = CanvasAbstand::from(radius_innen) * factor;
        let width_unten2 = CanvasAbstand::from(radius_innen) * angle.cos()
            + CanvasAbstand::from(radius_reverse_aussen) * factor_reverse;
        let width_unten = width_unten1.max(&width_unten2);
        let width_beschraenkung = beschraenkung::<Z>() * factor;
        width_gerade.max((width_oben.max(&width_unten) + width_beschraenkung).pixel())
    }

    fn height(&self) -> u64 {
        let SKurveWeiche {
            zugtyp,
            length,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction: _,
        } = *self;
        panic!();
        Kurve { zugtyp, radius, angle }.width()
    }

    fn zeichne(&self, cairo: &Cairo) {
        let SKurveWeiche {
            zugtyp,
            length,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction,
        } = *self;
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
