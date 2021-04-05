//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f64::consts::PI;
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
        let Weiche { zugtyp, length, radius, angle, direction } = *self;
        let transformations = if direction == Richtung::Links {
            vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))]
        } else {
            Vec::new()
        };
        let path_builder = canvas::PathBuilder::new_with_transformations(transformations);
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

    /*
    fn fuelle(&self, cairo: &mut Cairo) {
        let Weiche { zugtyp: _, length, radius, angle, direction } = *self;
        if direction == Richtung::Links {
            // spiegel y-Achse in der Mitte
            let x = canvas::X(0.);
            let half_height = canvas::Y(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        // utility größen
        let radius_abstand = radius.to_abstand();
        let spurweite: CanvasAbstand = Z::SPURWEITE.to_abstand();
        let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
        let winkel_ende: Angle = winkel_anfang + angle;
        let radius_innen_abstand = radius_abstand - 0.5 * spurweite;
        let radius_innen: canvas::Radius = canvas::Radius(0.) + radius_innen_abstand;
        let radius_aussen_abstand = radius_abstand + 0.5 * spurweite;
        let radius_aussen: canvas::Radius = canvas::Radius(0.) + radius_aussen_abstand;
        let bogen_zentrum_y: canvas::Y = canvas::Y(0.) + abstand::<Z>() + radius_aussen.into();
        let gleis_links: canvas::X = canvas::X(0.);
        let gerade_rechts: canvas::X = gleis_links + length.to_abstand();
        let gerade_oben: canvas::Y = canvas::Y(0.) + abstand::<Z>();
        let gerade_unten: canvas::Y = gerade_oben + spurweite;
        let winkel_ueberschneiden: Angle = Angle::acos(1. - spurweite / radius_abstand);
        let gerade_kurve_ueberschneiden: canvas::X =
            gleis_links + radius_abstand * winkel_ueberschneiden.sin();
        // zeichne Gleis
        cairo.arc_negative(
            gleis_links,
            bogen_zentrum_y,
            radius_innen,
            winkel_ende,
            winkel_anfang,
            false,
        );
        cairo.line_to(gleis_links, gerade_oben);
        cairo.line_to(gerade_rechts, gerade_oben);
        cairo.line_to(gerade_rechts, gerade_unten);
        cairo.line_to(gerade_kurve_ueberschneiden, gerade_unten);
        cairo.arc(
            gleis_links,
            bogen_zentrum_y,
            radius_aussen,
            winkel_anfang + winkel_ueberschneiden,
            winkel_ende,
            false,
        );
        cairo.close_path();
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
