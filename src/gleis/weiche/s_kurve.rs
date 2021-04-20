//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f32::consts::PI;
use std::marker::PhantomData;

use super::Richtung;
use crate::gleis::types::*;
use crate::gleis::weiche;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Weiche mit S-Kurve
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>angle) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct SKurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub laenge: canvas::Abstand<canvas::X>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Angle,
    pub radius_reverse: canvas::Abstand<canvas::Radius>,
    pub winkel_reverse: Angle,
    pub richtung: Richtung,
    pub beschreibung: Option<&'static str>,
}
impl<Z> SKurvenWeiche<Z> {
    pub const fn new(
        length: Length,
        radius: Radius,
        angle: Angle,
        radius_reverse: Radius,
        angle_reverse: Angle,
        direction: Richtung,
    ) -> Self {
        SKurvenWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            radius_reverse: radius_reverse.to_abstand(),
            winkel_reverse: angle_reverse,
            richtung: direction,
            beschreibung: None,
        }
    }
    pub const fn new_with_description(
        length: Length,
        radius: Radius,
        angle: Angle,
        radius_reverse: Radius,
        angle_reverse: Angle,
        direction: Richtung,
        description: &'static str,
    ) -> Self {
        SKurvenWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            radius_reverse: radius_reverse.to_abstand(),
            winkel_reverse: angle_reverse,
            richtung: direction,
            beschreibung: Some(description),
        }
    }
}

impl<Z: Zugtyp> Zeichnen for SKurvenWeiche<Z> {
    type AnchorName = weiche::gerade::AnchorName;
    type AnchorPoints = weiche::gerade::AnchorPoints;

    fn size(&self) -> canvas::Size {
        let SKurvenWeiche { laenge, radius, winkel, radius_reverse, winkel_reverse, .. } = *self;
        let angle_difference = winkel - winkel_reverse;
        let size_gerade = gerade::size::<Z>(laenge);

        //Breiten-Berechnung
        let factor_width = if winkel.abs() < Angle::new(0.5 * PI) { winkel.sin() } else { 1. };
        let factor_width_reverse = if angle_difference.abs() < Angle::new(0.5 * PI) {
            winkel.sin() - angle_difference.sin()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen = radius_begrenzung_aussen::<Z>(radius);
        let radius_aussen_x: canvas::Abstand<canvas::X> = radius_aussen.as_x();
        let radius_innen = radius_begrenzung_innen::<Z>(radius);
        let radius_innen_x: canvas::Abstand<canvas::X> = radius_innen.as_x();
        let radius_reverse_aussen = radius_begrenzung_aussen::<Z>(radius_reverse);
        let radius_reverse_innen = radius_begrenzung_innen::<Z>(radius_reverse);
        // obere Beschränkung
        let width_oben1: canvas::Abstand<canvas::X> = radius_aussen_x * factor_width;
        let width_oben2: canvas::Abstand<canvas::X> =
            radius_aussen_x * winkel.sin() + radius_reverse_innen.as_x() * factor_width_reverse;
        let width_oben: canvas::Abstand<canvas::X> = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1: canvas::Abstand<canvas::X> = radius_innen_x * factor_width;
        let width_unten2: canvas::Abstand<canvas::X> =
            radius_innen_x * winkel.sin() + radius_reverse_aussen.as_x() * factor_width_reverse;
        let width_unten: canvas::Abstand<canvas::X> = width_unten1.max(&width_unten2);

        // Höhen-Berechnung
        let factor_height = if winkel.abs() < Angle::new(PI) { 1. - winkel.cos() } else { 1. };
        let factor_height_reverse = if angle_difference.abs() < Angle::new(PI) {
            angle_difference.cos() - winkel.cos()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_aussen::<Z>(radius).as_y();
        let radius_reverse_innen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_innen::<Z>(radius_reverse).as_y();
        // obere Beschränkung
        let height_oben1: canvas::Abstand<canvas::Y> = radius_aussen * factor_height;
        let height_oben2: canvas::Abstand<canvas::Y> =
            radius_aussen * (1. - winkel.cos()) + radius_reverse_innen * factor_height_reverse;
        let height_oben: canvas::Abstand<canvas::Y> = height_oben1.max(&height_oben2);
        // untere Beschränkung
        let gleis_unten_start = beschraenkung::<Z>();
        let radius_innen: canvas::Abstand<canvas::Y> = radius_begrenzung_innen::<Z>(radius).as_y();
        let radius_reverse_aussen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_aussen::<Z>(radius_reverse).as_y();
        let height_unten1 = gleis_unten_start + radius_innen * factor_height;
        let height_unten2 = gleis_unten_start
            + radius_innen * (1. - winkel.cos())
            + radius_reverse_aussen * factor_height_reverse;
        let height_unten = height_unten1.max(&height_unten2);

        canvas::Size {
            width: size_gerade.width.max(&width_oben.max(&width_unten)),
            height: height_oben.max(&height_unten),
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(self.radius);
        let s_kurve_transformations = vec![
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.) + radius_begrenzung_aussen.as_x() * self.winkel.sin(),
                canvas::Y(0.) + radius_begrenzung_aussen.as_y() * (1. - self.winkel.cos()),
            )),
            canvas::Transformation::Rotate(self.winkel),
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                canvas::Y(0.) + beschraenkung::<Z>(),
            )),
        ];
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.laenge,
                true,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Keine,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschraenkung::Ende,
                transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.laenge,
                true,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Keine,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschraenkung::Ende,
                s_kurve_transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(self.radius);
        let s_kurve_transformations = vec![
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.) + radius_begrenzung_aussen.as_x() * self.winkel.sin(),
                canvas::Y(0.) + radius_begrenzung_aussen.as_y() * (1. - self.winkel.cos()),
            )),
            canvas::Transformation::Rotate(self.winkel),
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                canvas::Y(0.) + beschraenkung::<Z>(),
            )),
        ];
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Gerade
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.laenge,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations);
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.laenge,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                s_kurve_transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        }
        // return value
        paths
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
        println!("TODO innerhalb SKurvenWeiche");
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
        let angle_difference = self.winkel - self.winkel_reverse;
        weiche::gerade::AnchorPoints {
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
                    x: canvas::X(0.)
                        + self.radius.as_x() * self.winkel.sin()
                        + self.radius_reverse.as_x() * (self.winkel.sin() - angle_difference.sin()),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.as_y() * (1. - self.winkel.cos())
                                + self.radius_reverse.as_y()
                                    * (angle_difference.cos() - self.winkel.cos())),
                },
                direction: canvas::Vector::new(
                    canvas::X(angle_difference.cos()),
                    canvas::Y(multiplier * angle_difference.sin()),
                ),
            },
        }
    }
}
