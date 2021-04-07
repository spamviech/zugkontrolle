//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::Richtung;
use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;

/// Definition einer Kurven-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(Debug, Clone)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: Richtung,
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

    fn size(&self) -> canvas::Size {
        let KurvenWeiche { zugtyp, length, radius, angle, direction: _ } = *self;
        let size_gerade = Gerade { zugtyp, length }.size();
        let size_kurve = Kurve { zugtyp, radius, angle }.size();
        canvas::Size {
            width: canvas::X(0.)
                + size_gerade.width.to_abstand().max(&size_kurve.width.to_abstand()),
            height: size_kurve.height,
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        /*
        if self.direction == Richtung::Links {
            // spiegel y-Achse in der Mitte
            let x = canvas::X(0.);
            let half_height = canvas::Y(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        let gleis_oben: canvas::Y = canvas::Y(0.) + abstand::<Z>();
        let gleis_unten: canvas::Y = canvas::Y(0.) + beschraenkung::<Z>() - abstand::<Z>();
        let kurve_innen_anfang: canvas::X = canvas::X(0.);
        let kurve_aussen_anfang: canvas::X = kurve_innen_anfang + self.length.to_abstand();
        let angle: Angle = self.angle.into();
        // innere Kurve
        kurve::zeichne::<Z>(cairo, self.radius, angle, kurve::Beschraenkung::Alle);
        // Gerade vor äußerer Kurve
        cairo.move_to(kurve_innen_anfang, gleis_oben);
        cairo.line_to(kurve_aussen_anfang, gleis_oben);
        cairo.move_to(kurve_innen_anfang, gleis_unten);
        cairo.line_to(kurve_aussen_anfang, gleis_unten);
        // äußere Kurve
        cairo.translate(kurve_aussen_anfang, canvas::Y(0.));
        kurve::zeichne::<Z>(cairo, self.radius, angle, kurve::Beschraenkung::Ende);
        */
        println!("TODO KurvenWeiche");
        vec![]
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        //TODO
        println!("TODO KurvenWeiche");
        vec![]
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: canvas::Y;
        let multiplier: f32;
        match self.direction {
            Richtung::Rechts => {
                start_height = canvas::Y(0.);
                multiplier = 1.;
            }
            Richtung::Links => {
                start_height = self.size().height;
                multiplier = -1.;
            }
        };
        let halbe_beschraenkung: canvas::Abstand = 0.5 * beschraenkung::<Z>();
        let radius_abstand: canvas::Abstand = self.radius.to_abstand();
        let kurve_anchor_direction: anchor::Direction = anchor::Direction {
            dx: canvas::X(self.angle.cos()),
            dy: canvas::Y(multiplier * self.angle.sin()),
        };
        let kurve_anchor_x: canvas::X = canvas::X(0.) + radius_abstand * self.angle.sin();
        let kurve_anchor_y: canvas::Y = start_height
            + multiplier * (halbe_beschraenkung + radius_abstand * (1. - self.angle.cos()));
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.),
                    y: start_height + multiplier * halbe_beschraenkung,
                },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(multiplier * 0.) },
            },
            innen: anchor::Point {
                position: anchor::Position { x: kurve_anchor_x, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
            aussen: anchor::Point {
                position: anchor::Position {
                    x: kurve_anchor_x + self.length.to_abstand(),
                    y: kurve_anchor_y,
                },
                direction: kurve_anchor_direction,
            },
        }
    }
}
