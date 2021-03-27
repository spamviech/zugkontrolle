//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::gerade::WeichenRichtung;
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;
use crate::gleis::widget::Zeichnen;

/// Definition einer Kurven-Weiche
#[derive(Debug, Clone)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtung,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Innen,
    Aussen,
}

impl<Z: Zugtyp> Zeichnen for KurvenWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn width(&self) -> u64 {
        let KurvenWeiche { zugtyp, length, radius, angle, direction: _ } = *self;
        Gerade { zugtyp, length }.width() + Kurve { zugtyp, radius, angle }.width()
    }

    fn height(&self) -> u64 {
        let KurvenWeiche { zugtyp, length: _, radius, angle, direction: _ } = *self;
        Kurve { zugtyp, radius, angle }.height()
    }

    fn zeichne(&self, cairo: &Cairo) {
        if self.direction == WeichenRichtung::Links {
            // spiegel y-Achse in der Mitte
            let x = CanvasX(0.);
            let half_height = CanvasY(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        let gleis_oben: CanvasY = CanvasY(0.) + abstand::<Z>();
        let gleis_unten: CanvasY = CanvasY(0.) + beschraenkung::<Z>() - abstand::<Z>();
        let kurve_innen_anfang: CanvasX = CanvasX(0.);
        let kurve_aussen_anfang: CanvasX = kurve_innen_anfang + CanvasAbstand::from(self.length);
        let angle: Angle = self.angle.into();
        // innere Kurve
        kurve::zeichne::<Z>(cairo, self.radius, angle, kurve::Beschraenkung::Alle);
        // Gerade vor äußerer Kurve
        cairo.move_to(kurve_innen_anfang, gleis_oben);
        cairo.line_to(kurve_aussen_anfang, gleis_oben);
        cairo.move_to(kurve_innen_anfang, gleis_unten);
        cairo.line_to(kurve_aussen_anfang, gleis_unten);
        // äußere Kurve
        cairo.translate(kurve_aussen_anfang, CanvasY(0.));
        kurve::zeichne::<Z>(cairo, self.radius, angle, kurve::Beschraenkung::Ende);
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
        let halbe_beschraenkung: CanvasAbstand = 0.5 * beschraenkung::<Z>();
        let radius_abstand: CanvasAbstand = CanvasAbstand::from(self.radius);
        let kurve_anchor_direction: anchor::Direction = anchor::Direction {
            dx: CanvasX(self.angle.cos()),
            dy: CanvasY(multiplier * self.angle.sin()),
        };
        let kurve_anchor_x: CanvasX = CanvasX(0.) + radius_abstand * self.angle.sin();
        let kurve_anchor_y: CanvasY = start_height
            + multiplier * (halbe_beschraenkung + radius_abstand * (1. - self.angle.cos()));
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.),
                    y: start_height + multiplier * halbe_beschraenkung,
                },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(multiplier * 0.) },
            },
            innen: anchor::Point {
                position: anchor::Position { x: kurve_anchor_x, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
            aussen: anchor::Point {
                position: anchor::Position {
                    x: kurve_anchor_x + CanvasAbstand::from(self.length),
                    y: kurve_anchor_y,
                },
                direction: kurve_anchor_direction,
            },
        }
    }
}
