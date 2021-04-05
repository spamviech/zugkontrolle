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
use crate::gleis::gerade::Gerade;
use crate::gleis::kurve;
use crate::gleis::types::*;
use crate::gleis::weiche;

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
        let size_gerade = Gerade { zugtyp, length }.size();

        //Breiten-Berechnung
        let factor_width = if angle.abs() < Angle::new(0.5 * PI) { angle.sin() } else { 1. };
        let factor_width_reverse = if angle_difference.abs() < Angle::new(0.5 * PI) {
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
        let width_oben1: canvas::Abstand = radius_aussen.to_abstand() * factor_width;
        let width_oben2: canvas::Abstand = radius_aussen.to_abstand() * angle.sin()
            + radius_reverse_innen.to_abstand() * factor_width_reverse;
        let width_oben: canvas::Abstand = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1 = radius_innen.to_abstand() * factor_width;
        let width_unten2 = radius_innen.to_abstand() * angle.sin()
            + radius_reverse_aussen.to_abstand() * factor_width_reverse;
        let width_unten = width_unten1.max(&width_unten2);

        // Höhen-Berechnung
        let factor_height = if angle.abs() < Angle::new(PI) { 1. - angle.cos() } else { 1. };
        let factor_height_reverse = if angle_difference.abs() < Angle::new(PI) {
            angle_difference.cos() - angle.cos()
        } else {
            1.
        }
        .max(0.);
        let radius_aussen: canvas::Abstand = radius_begrenzung_aussen::<Z>(radius).into();
        let radius_reverse_innen: canvas::Abstand =
            radius_begrenzung_innen::<Z>(radius_reverse).into();
        // obere Beschränkung
        let height_oben1: canvas::Abstand = radius_aussen * factor_height;
        let height_oben2: canvas::Abstand =
            radius_aussen * (1. - angle.cos()) + radius_reverse_innen * factor_height_reverse;
        let height_oben: canvas::Abstand = height_oben1.max(&height_oben2);
        // untere Beschränkung
        let gleis_unten_start = beschraenkung::<Z>();
        let radius_innen: canvas::Abstand = radius_begrenzung_innen::<Z>(radius).into();
        let radius_reverse_aussen: canvas::Abstand =
            radius_begrenzung_aussen::<Z>(radius_reverse).into();
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
        /*
        let SKurvenWeiche {
            zugtyp,
            length,
            radius,
            angle,
            radius_reverse,
            angle_reverse,
            direction,
        } = *self;
        if direction == Richtung::Links {
            // spiegel y-Achse in der Mitte
            let x = canvas::X(0.);
            let half_height = canvas::Y(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        // zeichne gerade
        Gerade { zugtyp, length }.zeichne(cairo);
        // zeichne ersten Teil der S-Kurve
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Keine);
        let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(self.radius);
        // verschiebe Kontext an Position nach erster Kurve
        cairo.translate(
            canvas::X(0.) + radius_begrenzung_aussen * self.angle.sin(),
            canvas::Y(0.) + radius_begrenzung_aussen * (1. - self.angle.cos()),
        );
        cairo.rotate(self.angle.into());
        // spiegel die y-Achse, damit die Kurve in die entgegengesetzte Richtung geht
        let x = canvas::X(0.);
        let half_beschraenkung = canvas::Y(0.) + 0.5 * beschraenkung::<Z>();
        cairo.translate(x, half_beschraenkung);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(x, -half_beschraenkung);
        // zeichne zweiten Teil der S-Kurve
        kurve::zeichne::<Z>(
            cairo,
            radius_reverse,
            angle_reverse.into(),
            kurve::Beschraenkung::Ende,
        );
        */
        println!("TODO SKurvenWeiche");
        vec![]
    }

    /*
    fn fuelle(&self, cairo: &mut Cairo) {
        //TODO
        println!("TODO")
    }
    */

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
                    x: canvas::X(0.)
                        + self.radius.to_abstand() * self.angle.sin()
                        + self.radius_reverse.to_abstand()
                            * (self.angle.sin() - angle_difference.sin()),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.to_abstand() * (1. - self.angle.cos())
                                + self.radius_reverse.to_abstand()
                                    * (angle_difference.cos() - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: canvas::X(angle_difference.cos()),
                    dy: canvas::Y(multiplier * angle_difference.sin()),
                },
            },
        }
    }
}
