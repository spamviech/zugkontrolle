//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::Richtung;
use crate::gleis::anchor;
use crate::gleis::gerade::{self, Gerade};
use crate::gleis::kurve::{self, Kurve};
use crate::gleis::types::*;

/// Definition einer Kurven-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: Angle,
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
        // utility sizes
        let aussen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.length.to_abstand(),
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.direction == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Alle,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.length,
                false,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(aussen_transformation);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Ende,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Alle,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.length,
                false,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Ende,
                vec![aussen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let aussen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.length.to_abstand(),
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.direction == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Innere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.length,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(aussen_transformation);
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.length,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                vec![aussen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
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
        let halbe_beschraenkung: canvas::Abstand<canvas::Y> = 0.5 * beschraenkung::<Z>();
        let radius_abstand: canvas::Abstand<canvas::Radius> = self.radius.to_abstand();
        let kurve_anchor_direction: anchor::Direction = anchor::Direction {
            dx: canvas::X(self.angle.cos()),
            dy: canvas::Y(multiplier * self.angle.sin()),
        };
        let kurve_anchor_x: canvas::X = canvas::X(0.) + radius_abstand.convert() * self.angle.sin();
        let kurve_anchor_y: canvas::Y = start_height
            + multiplier
                * (halbe_beschraenkung + radius_abstand.convert() * (1. - self.angle.cos()));
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
