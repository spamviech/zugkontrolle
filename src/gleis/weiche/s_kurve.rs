//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f32::consts::PI;
use std::marker::PhantomData;

use super::Richtung;
use crate::gleis::anchor;
use crate::gleis::gerade::{self, Gerade};
use crate::gleis::kurve;
use crate::gleis::types::*;
use crate::gleis::weiche;

/// Definition einer Weiche mit S-Kurve
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>angle) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct SKurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: Angle,
    pub radius_reverse: Radius,
    pub angle_reverse: Angle,
    pub direction: Richtung,
}

impl<Z: Zugtyp> Zeichnen for SKurvenWeiche<Z> {
    type AnchorName = weiche::gerade::AnchorName;
    type AnchorPoints = weiche::gerade::AnchorPoints;

    fn size(&self) -> canvas::Size {
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
        let size_gerade = Gerade { zugtyp, length, description: None }.size();

        //Breiten-Berechnung
        let factor_width = if angle.abs() < Angle::new(0.5 * PI) { angle.sin() } else { 1. };
        let factor_width_reverse = if angle_difference.abs() < Angle::new(0.5 * PI) {
            angle.sin() - angle_difference.sin()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen = radius_begrenzung_aussen::<Z>(radius);
        let radius_aussen_x: canvas::Abstand<canvas::X> = radius_aussen.convert();
        let radius_innen = radius_begrenzung_innen::<Z>(radius);
        let radius_innen_x: canvas::Abstand<canvas::X> = radius_innen.convert();
        let radius_reverse_aussen = radius_begrenzung_aussen::<Z>(radius_reverse);
        let radius_reverse_innen = radius_begrenzung_innen::<Z>(radius_reverse);
        // obere Beschränkung
        let width_oben1: canvas::Abstand<canvas::X> = radius_aussen_x * factor_width;
        let width_oben2: canvas::Abstand<canvas::X> =
            radius_aussen_x * angle.sin() + radius_reverse_innen.convert() * factor_width_reverse;
        let width_oben: canvas::Abstand<canvas::X> = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1: canvas::Abstand<canvas::X> = radius_innen_x * factor_width;
        let width_unten2: canvas::Abstand<canvas::X> =
            radius_innen_x * angle.sin() + radius_reverse_aussen.convert() * factor_width_reverse;
        let width_unten: canvas::Abstand<canvas::X> = width_unten1.max(&width_unten2);

        // Höhen-Berechnung
        let factor_height = if angle.abs() < Angle::new(PI) { 1. - angle.cos() } else { 1. };
        let factor_height_reverse = if angle_difference.abs() < Angle::new(PI) {
            angle_difference.cos() - angle.cos()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_aussen::<Z>(radius).convert();
        let radius_reverse_innen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_innen::<Z>(radius_reverse).convert();
        // obere Beschränkung
        let height_oben1: canvas::Abstand<canvas::Y> = radius_aussen * factor_height;
        let height_oben2: canvas::Abstand<canvas::Y> =
            radius_aussen * (1. - angle.cos()) + radius_reverse_innen * factor_height_reverse;
        let height_oben: canvas::Abstand<canvas::Y> = height_oben1.max(&height_oben2);
        // untere Beschränkung
        let gleis_unten_start = beschraenkung::<Z>();
        let radius_innen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_innen::<Z>(radius).convert();
        let radius_reverse_aussen: canvas::Abstand<canvas::Y> =
            radius_begrenzung_aussen::<Z>(radius_reverse).convert();
        let height_unten1 = gleis_unten_start + radius_innen * factor_height;
        let height_unten2 = gleis_unten_start
            + radius_innen * (1. - angle.cos())
            + radius_reverse_aussen * factor_height_reverse;
        let height_unten = height_unten1.max(&height_unten2);

        canvas::Size {
            width: canvas::X(0.)
                + size_gerade.width.to_abstand().max(&width_oben.max(&width_unten)),
            height: canvas::Y(0.) + height_oben.max(&height_unten),
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(self.radius);
        let s_kurve_transformations = vec![
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.) + radius_begrenzung_aussen.convert() * self.angle.sin(),
                canvas::Y(0.) + radius_begrenzung_aussen.convert() * (1. - self.angle.cos()),
            )),
            canvas::Transformation::Rotate(self.angle),
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                canvas::Y(0.) + beschraenkung::<Z>(),
            )),
        ];
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.direction == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.length,
                true,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Keine,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.angle_reverse,
                kurve::Beschraenkung::Ende,
                transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.length,
                true,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.angle,
                kurve::Beschraenkung::Keine,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.angle_reverse,
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
                canvas::X(0.) + radius_begrenzung_aussen.convert() * self.angle.sin(),
                canvas::Y(0.) + radius_begrenzung_aussen.convert() * (1. - self.angle.cos()),
            )),
            canvas::Transformation::Rotate(self.angle),
            canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                canvas::Y(0.) + beschraenkung::<Z>(),
            )),
        ];
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.direction == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Gerade
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.length,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations);
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius_reverse,
                self.angle_reverse,
                transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.length,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.angle,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius_reverse,
                self.angle_reverse,
                s_kurve_transformations,
                canvas::PathBuilder::with_invert_y,
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
        let angle_difference = self.angle - self.angle_reverse;
        weiche::gerade::AnchorPoints {
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
                    x: canvas::X(0.)
                        + self.radius.to_abstand().convert() * self.angle.sin()
                        + self.radius_reverse.to_abstand().convert()
                            * (self.angle.sin() - angle_difference.sin()),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.to_abstand().convert() * (1. - self.angle.cos())
                                + self.radius_reverse.to_abstand().convert()
                                    * (angle_difference.cos() - self.angle.cos())),
                },
                direction: canvas::Vector {
                    dx: canvas::X(angle_difference.cos()),
                    dy: canvas::Y(multiplier * angle_difference.sin()),
                },
            },
        }
    }
}
