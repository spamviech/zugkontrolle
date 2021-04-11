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

/// Definition einer Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: Angle,
    pub direction: Richtung,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Richtung {
    Links,
    Rechts,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Kurve,
}

impl<Z: Zugtyp> Zeichnen for Weiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> canvas::Size {
        let Weiche { zugtyp, length, radius, angle, direction: _ } = *self;
        let gerade_size = Gerade { zugtyp, length, description: None }.size();
        let kurve_size = Kurve { zugtyp, radius, angle }.size();
        canvas::Size {
            width: canvas::X(0.)
                + gerade_size.width.to_abstand().max(&kurve_size.width.to_abstand()),
            height: kurve_size.height,
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp, length, radius, angle, direction } = *self;
        if direction == Richtung::Links {
            let transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            vec![
                gerade::zeichne(
                    zugtyp,
                    length,
                    true,
                    transformations.clone(),
                    canvas::PathBuilder::with_invert_y,
                ),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    angle,
                    kurve::Beschraenkung::Ende,
                    transformations,
                    canvas::PathBuilder::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::zeichne(
                    zugtyp,
                    length,
                    true,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    angle,
                    kurve::Beschraenkung::Ende,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
            ]
        }
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp, length, radius, angle, direction } = *self;
        if direction == Richtung::Links {
            let transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            vec![
                gerade::fuelle(
                    zugtyp,
                    length,
                    transformations.clone(),
                    canvas::PathBuilder::with_invert_y,
                ),
                kurve::fuelle(
                    zugtyp,
                    radius,
                    angle,
                    transformations,
                    canvas::PathBuilder::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::fuelle(zugtyp, length, Vec::new(), canvas::PathBuilder::with_normal_axis),
                kurve::fuelle(
                    zugtyp,
                    radius,
                    angle,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
            ]
        }
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
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: canvas::Vector { dx: canvas::X(-1.), dy: canvas::Y(multiplier * 0.) },
            },
            gerade: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.) + self.length.to_abstand(),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: canvas::Vector { dx: canvas::X(1.), dy: canvas::Y(multiplier * 0.) },
            },
            kurve: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.) + self.angle.sin() * self.radius.to_abstand().convert(),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.to_abstand().convert() * (1. - self.angle.cos())),
                },
                direction: canvas::Vector {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(multiplier * self.angle.sin()),
                },
            },
        }
    }
}
