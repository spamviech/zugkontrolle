//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::gleis::types::*;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub laenge: canvas::Abstand<canvas::X>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Angle,
    pub richtung: Richtung,
    pub beschreibung: Option<&'static str>,
}
impl<Z> Weiche<Z> {
    pub const fn new(length: Length, radius: Radius, angle: Angle, richtung: Richtung) -> Self {
        Weiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            richtung,
            beschreibung: None,
        }
    }
    pub const fn new_with_description(
        length: Length,
        radius: Radius,
        angle: Angle,
        richtung: Richtung,
        description: &'static str,
    ) -> Self {
        Weiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            richtung,
            beschreibung: Some(description),
        }
    }
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
        let Weiche { laenge, radius, winkel, .. } = *self;
        let gerade_size = gerade::size::<Z>(laenge);
        let kurve_size = kurve::size::<Z>(radius, winkel);
        canvas::Size { width: gerade_size.width.max(&kurve_size.width), height: kurve_size.height }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp, laenge, radius, winkel, richtung, .. } = *self;
        if richtung == Richtung::Links {
            let transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            vec![
                gerade::zeichne(
                    zugtyp,
                    laenge,
                    true,
                    transformations.clone(),
                    canvas::PathBuilder::with_invert_y,
                ),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    winkel,
                    kurve::Beschraenkung::Ende,
                    transformations,
                    canvas::PathBuilder::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::zeichne(
                    zugtyp,
                    laenge,
                    true,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    winkel,
                    kurve::Beschraenkung::Ende,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
            ]
        }
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        let Weiche { zugtyp, laenge, radius, winkel, richtung, .. } = *self;
        if richtung == Richtung::Links {
            let transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            vec![
                gerade::fuelle(
                    zugtyp,
                    laenge,
                    transformations.clone(),
                    canvas::PathBuilder::with_invert_y,
                ),
                kurve::fuelle(
                    zugtyp,
                    radius,
                    winkel,
                    transformations,
                    canvas::PathBuilder::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::fuelle(zugtyp, laenge, Vec::new(), canvas::PathBuilder::with_normal_axis),
                kurve::fuelle(
                    zugtyp,
                    radius,
                    winkel,
                    Vec::new(),
                    canvas::PathBuilder::with_normal_axis,
                ),
            ]
        }
    }

    fn beschreibung(&self) -> Option<(canvas::Position, &'static str)> {
        self.beschreibung.map(|text| {
            let start_height: canvas::Y;
            let multiplier: f32;
            match self.richtung {
                Richtung::Rechts => {
                    start_height = canvas::Y(0.);
                    multiplier = 1.;
                }
                Richtung::Links => {
                    start_height = canvas::Y(0.) + self.size().height;
                    multiplier = -1.;
                }
            };
            (
                canvas::Position {
                    point: canvas::Point::new(
                        canvas::X(0.) + 0.5 * self.laenge,
                        start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                    ),
                    winkel: Angle::new(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: canvas::Vector) -> bool {
        //TODO
        println!("TODO innerhalb Weiche");
        false
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: canvas::Y;
        let multiplier: f32;
        match self.richtung {
            Richtung::Rechts => {
                start_height = canvas::Y(0.);
                multiplier = 1.;
            }
            Richtung::Links => {
                start_height = canvas::Y(0.) + self.size().height;
                multiplier = -1.;
            }
        };
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: canvas::Vector::new(canvas::X(-1.), canvas::Y(multiplier * 0.)),
            },
            gerade: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.) + self.laenge,
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: canvas::Vector::new(canvas::X(1.), canvas::Y(multiplier * 0.)),
            },
            kurve: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.) + self.winkel.sin() * self.radius.as_x(),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.as_y() * (1. - self.winkel.cos())),
                },
                direction: canvas::Vector::new(
                    canvas::X(self.winkel.cos()),
                    canvas::Y(multiplier * self.winkel.sin()),
                ),
            },
        }
    }
}
