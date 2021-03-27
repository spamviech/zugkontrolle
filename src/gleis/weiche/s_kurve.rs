//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f64::consts::PI;
use std::marker::PhantomData;

use super::gerade::WeichenRichtung;
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve;
use crate::gleis::types::*;
use crate::gleis::weiche;
use crate::gleis::widget::Zeichnen;

/// Definition einer Weiche mit S-Kurve
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>angle) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[derive(Debug, Clone)]
pub struct SKurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub radius_reverse: Radius,
    pub angle_reverse: AngleDegrees,
    pub direction: WeichenRichtung,
}

impl<Z: Zugtyp> Zeichnen for SKurvenWeiche<Z> {
    type AnchorName = weiche::gerade::AnchorName;
    type AnchorPoints = weiche::gerade::AnchorPoints;

    fn width(&self) -> u64 {
        let SKurvenWeiche {
            zugtyp,
            length,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction: _,
        } = *self;
        let angle_difference = angle - angle_reverse;
        let width_gerade = Gerade { zugtyp, length }.width();
        let factor = if angle.abs() < Angle::new(0.5 * PI) { angle.sin() } else { 1. };
        let factor_reverse = if angle_difference.abs() < Angle::new(0.5 * PI) {
            angle_difference.cos() - angle.cos()
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
        let width_oben2: CanvasAbstand = CanvasAbstand::from(radius_aussen) * angle.sin()
            + CanvasAbstand::from(radius_reverse_innen) * factor_reverse;
        let width_oben: CanvasAbstand = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1 = CanvasAbstand::from(radius_innen) * factor;
        let width_unten2 = CanvasAbstand::from(radius_innen) * angle.sin()
            + CanvasAbstand::from(radius_reverse_aussen) * factor_reverse;
        let width_unten = width_unten1.max(&width_unten2);
        width_gerade.max(width_oben.max(&width_unten).pixel())
    }

    fn height(&self) -> u64 {
        let SKurvenWeiche {
            zugtyp: _,
            length: _,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction: _,
        } = *self;
        let angle_difference = angle - angle_reverse;
        let factor = if angle.abs() < Angle::new(PI) { 1. - angle.cos() } else { 1. };
        let factor_reverse = if angle_difference.abs() < Angle::new(PI) {
            angle.sin() - angle_difference.sin()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen = radius_begrenzung_aussen::<Z>(radius);
        let radius_innen = radius_begrenzung_innen::<Z>(radius);
        let radius_reverse_aussen = radius_begrenzung_aussen::<Z>(radius_reverse);
        let radius_reverse_innen = radius_begrenzung_innen::<Z>(radius_reverse);
        // obere Beschränkung
        let height_oben1: CanvasAbstand = CanvasAbstand::from(radius_aussen) * factor;
        let height_oben2: CanvasAbstand = CanvasAbstand::from(radius_aussen) * (1. - angle.cos())
            + CanvasAbstand::from(radius_reverse_innen) * factor_reverse;
        let height_oben: CanvasAbstand = height_oben1.max(&height_oben2);
        // untere Beschränkung
        let height_unten1 = beschraenkung::<Z>() + CanvasAbstand::from(radius_innen) * factor;
        let height_unten2 = beschraenkung::<Z>()
            + CanvasAbstand::from(radius_innen) * (1. - angle.cos())
            + CanvasAbstand::from(radius_reverse_aussen) * factor_reverse;
        let height_unten = height_unten1.max(&height_unten2);
        height_oben.max(&height_unten).pixel()
    }

    fn zeichne(&self, cairo: &Cairo) {
        let SKurvenWeiche {
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
        Gerade { zugtyp, length }.zeichne(cairo);
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Keine);
        // panic!();
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
        let angle_difference = self.angle - self.angle_reverse;
        weiche::gerade::AnchorPoints {
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
                    x: CanvasX(0.)
                        + CanvasAbstand::from(self.radius) * self.angle.sin()
                        + CanvasAbstand::from(self.radius_reverse)
                            * (angle_difference.cos() - self.angle.cos()),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + CanvasAbstand::from(self.radius) * (1. - self.angle.cos())
                                + CanvasAbstand::from(self.radius_reverse)
                                    * (angle_difference.sin() - self.angle.sin())),
                },
                direction: anchor::Direction {
                    dx: CanvasX(angle_difference.cos()),
                    dy: CanvasY(multiplier * angle_difference.sin()),
                },
            },
        }
    }
}
