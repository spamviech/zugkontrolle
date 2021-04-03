//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f64::consts::PI;
use std::marker::PhantomData;

use crate::gleis::anchor;
use crate::gleis::gerade::Gerade;
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

    fn width(&self) -> u64 {
        let Weiche { zugtyp, length, radius, angle, direction: _ } = *self;
        let width_gerade = Gerade { zugtyp, length }.width();
        let width_kurve = Kurve { zugtyp, radius, angle }.width();
        width_gerade.max(width_kurve)
    }

    fn height(&self) -> u64 {
        let Weiche { zugtyp, length: _, radius, angle, direction: _ } = *self;
        Kurve { zugtyp, radius, angle }.height()
    }

    fn zeichne(&self, cairo: &mut Cairo) {
        let Weiche { zugtyp, length, radius, angle, direction } = *self;
        if direction == Richtung::Links {
            // spiegel y-Achse in der Mitte
            let x = CanvasX(0.);
            let half_height = CanvasY(0.5 * (self.height() as f64));
            cairo.translate(x, half_height);
            cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
            cairo.translate(-x, -half_height);
        }
        Gerade { zugtyp, length }.zeichne(cairo);
        kurve::zeichne::<Z>(cairo, radius, angle.into(), kurve::Beschraenkung::Ende);
    }

    fn fuelle(&self, cairo: &mut Cairo) {
        let Weiche { zugtyp: _, length, radius, angle, direction } = *self;
        if direction == Richtung::Links {
            // spiegel y-Achse in der Mitte
            let x = CanvasX(0.);
            let half_height = CanvasY(0.5 * (self.height() as f64));
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
        let radius_innen: CanvasRadius = CanvasRadius(0.) + radius_innen_abstand;
        let radius_aussen_abstand = radius_abstand + 0.5 * spurweite;
        let radius_aussen: CanvasRadius = CanvasRadius(0.) + radius_aussen_abstand;
        let bogen_zentrum_y: CanvasY = CanvasY(0.) + abstand::<Z>() + radius_aussen.into();
        let gleis_links: CanvasX = CanvasX(0.);
        let gerade_rechts: CanvasX = gleis_links + length.to_abstand();
        let gerade_oben: CanvasY = CanvasY(0.) + abstand::<Z>();
        let gerade_unten: CanvasY = gerade_oben + spurweite;
        let winkel_ueberschneiden: Angle = Angle::acos(1. - spurweite / radius_abstand);
        let gerade_kurve_ueberschneiden: CanvasX =
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

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: CanvasY;
        let multiplier: f64;
        match self.direction {
            Richtung::Rechts => {
                start_height = CanvasY(0.);
                multiplier = 1.;
            }
            Richtung::Links => {
                start_height = CanvasY(self.height() as f64);
                multiplier = -1.;
            }
        };
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(multiplier * 0.) },
            },
            gerade: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.) + self.length.to_abstand(),
                    y: start_height + multiplier * 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: CanvasX(1.), dy: CanvasY(multiplier * 0.) },
            },
            kurve: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.) + self.angle.sin() * self.radius.to_abstand(),
                    y: start_height
                        + multiplier
                            * (0.5 * beschraenkung::<Z>()
                                + self.radius.to_abstand() * (1. - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(multiplier * self.angle.sin()),
                },
            },
        }
    }
}
