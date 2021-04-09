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
#[derive(Debug, Clone)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
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
        let gerade_size = Gerade { zugtyp, length }.size();
        let kurve_size = Kurve { zugtyp, radius, angle }.size();
        canvas::Size {
            width: canvas::X(0.)
                + gerade_size.width.to_abstand().max(&kurve_size.width.to_abstand()),
            height: kurve_size.height,
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp: _, length, radius, angle, direction } = *self;
        let transformations = if direction == Richtung::Links {
            vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))]
        } else {
            Vec::new()
        };
        let mut path_builder = canvas::PathBuilder::new_with_transformations(transformations);
        if direction == Richtung::Links {
            path_builder.with_invert_y(|builder| {
                gerade::zeichne::<Z>(builder, length);
                kurve::zeichne::<Z>(builder, radius, angle.into(), kurve::Beschraenkung::Ende);
            });
        } else {
            gerade::zeichne::<Z>(&mut path_builder, length);
            kurve::zeichne::<Z>(
                &mut path_builder,
                radius,
                angle.into(),
                kurve::Beschraenkung::Ende,
            );
        }
        vec![path_builder.build()]
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp: _, length, radius, angle, direction } = *self;
        let transformations = if direction == Richtung::Links {
            vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))]
        } else {
            Vec::new()
        };
        let mut gerade_builder =
            canvas::PathBuilder::new_with_transformations(transformations.clone());
        let mut kurve_builder = canvas::PathBuilder::new_with_transformations(transformations);
        if direction == Richtung::Links {
            gerade_builder.with_invert_y(|builder| {
                gerade::fuelle::<Z>(builder, length);
            });
            kurve_builder.with_invert_y(|builder| {
                kurve::fuelle::<Z>(builder, radius, angle.into());
            });
        } else {
            gerade::fuelle::<Z>(&mut gerade_builder, length);
            kurve::fuelle::<Z>(&mut kurve_builder, radius, angle.into());
        }
        vec![gerade_builder.build(), kurve_builder.build()]
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
            anfang: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(multiplier * 0.) },
            },
            gerade: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.) + self.length.to_abstand(),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: canvas::X(1.), dy: canvas::Y(multiplier * 0.) },
            },
            kurve: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.) + self.angle.sin() * self.radius.to_abstand(),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.to_abstand() * (1. - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(multiplier * self.angle.sin()),
                },
            },
        }
    }
}
