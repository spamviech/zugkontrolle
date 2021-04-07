//! Definition und zeichnen einer Kurve

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f32::consts::PI;
use std::marker::PhantomData;

use super::anchor;
use super::types::*;

/// Definition einer Kurve
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(Debug, Clone)]
pub struct Kurve<T> {
    pub zugtyp: PhantomData<*const T>,
    pub radius: Radius,
    pub angle: AngleDegrees,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Kurve<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> canvas::Size {
        // Breite
        let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(self.radius);
        let width_factor =
            if self.angle.abs() < Angle::new(0.5 * PI) { self.angle.sin() } else { 1. };
        let width = canvas::X(0.) + radius_begrenzung_aussen * width_factor;
        // Höhe des Bogen
        let angle_abs = self.angle.abs();
        let comparison = if angle_abs < Angle::new(0.5 * PI) {
            radius_begrenzung_aussen * (1. - self.angle.cos())
                + beschraenkung::<Z>() * self.angle.cos()
        } else if angle_abs < Angle::new(PI) {
            radius_begrenzung_aussen * (1. - self.angle.cos())
        } else {
            radius_begrenzung_aussen
        };
        // Mindesthöhe: Beschränkung einer Geraden
        let height = canvas::Y(0.) + beschraenkung::<Z>().max(&comparison);

        canvas::Size { width, height }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        let mut path_builder = canvas::PathBuilder::new();
        zeichne::<Z>(&mut path_builder, self.radius, self.angle.into(), Beschraenkung::Alle);
        vec![path_builder.build()]
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        let mut path_builder = canvas::PathBuilder::new();
        fuelle::<Z>(&mut path_builder, self.radius, self.angle.into());
        path_builder.close();
        vec![path_builder.build()]
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.),
                    y: canvas::Y(0.) + 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(0.) },
            },
            ende: anchor::Point {
                position: anchor::Position {
                    x: canvas::X(0.) + self.radius.to_abstand() * self.angle.sin(),
                    y: canvas::Y(0.)
                        + (0.5 * beschraenkung::<Z>()
                            + self.radius.to_abstand() * (1. - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: canvas::X(self.angle.cos()),
                    dy: canvas::Y(self.angle.sin()),
                },
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Beschraenkung {
    Keine,
    Ende,
    Alle,
}
impl Beschraenkung {
    fn anfangs_beschraenkung(&self) -> bool {
        match self {
            Beschraenkung::Alle => true,
            Beschraenkung::Keine | Beschraenkung::Ende => false,
        }
    }

    fn end_beschraenkung(&self) -> bool {
        match self {
            Beschraenkung::Ende | Beschraenkung::Alle => true,
            Beschraenkung::Keine => false,
        }
    }
}

// factor_y is expected to be -1 or +1, although other values should work as well
pub(crate) fn zeichne<Z: Zugtyp>(
    path_builder: &mut canvas::PathBuilder,
    radius: Radius,
    winkel: Angle,
    beschraenkungen: Beschraenkung,
) {
    // Utility Größen
    let radius_abstand: canvas::Abstand = radius.to_abstand();
    let spurweite: canvas::Abstand = Z::SPURWEITE.to_abstand();
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let gleis_links: canvas::X = canvas::X(0.);
    let gleis_links_oben: canvas::Y = canvas::Y(0.);
    let gleis_links_unten: canvas::Y = gleis_links_oben + beschraenkung::<Z>();
    let radius_innen: canvas::Radius = canvas::Radius(0.) + radius_abstand - 0.5 * spurweite;
    let radius_aussen: canvas::Radius = radius_innen + spurweite;
    let radius_begrenzung_aussen: canvas::Abstand = radius_aussen.to_abstand() + abstand::<Z>();
    let begrenzung_x0: canvas::X = gleis_links + radius_begrenzung_aussen * winkel.sin();
    let begrenzung_y0: canvas::Y =
        gleis_links_oben + radius_begrenzung_aussen * (1. - winkel.cos());
    let begrenzung_x1: canvas::X = begrenzung_x0 - beschraenkung::<Z>() * winkel.sin();
    let begrenzung_y1: canvas::Y = begrenzung_y0 + beschraenkung::<Z>() * winkel.cos();
    let bogen_zentrum_y: canvas::Y = gleis_links_oben + radius_begrenzung_aussen;
    // Beschränkungen
    if beschraenkungen.anfangs_beschraenkung() {
        path_builder.move_to(canvas::Point::new(gleis_links, gleis_links_oben));
        path_builder.line_to(canvas::Point::new(gleis_links, gleis_links_unten));
    }
    if beschraenkungen.end_beschraenkung() {
        path_builder.move_to(canvas::Point::new(begrenzung_x0, begrenzung_y0));
        path_builder.line_to(canvas::Point::new(begrenzung_x1, begrenzung_y1));
    }
    // Gleis
    path_builder.arc(canvas::Arc {
        center: canvas::Point::new(gleis_links, bogen_zentrum_y),
        radius: canvas::Radius(0.) + radius_aussen.to_abstand(),
        start: winkel_anfang,
        end: winkel_ende,
    });
    path_builder.arc(canvas::Arc {
        center: canvas::Point::new(gleis_links, bogen_zentrum_y),
        radius: canvas::Radius(0.) + radius_innen.to_abstand(),
        start: winkel_anfang,
        end: winkel_ende,
    });
}

pub(crate) fn fuelle<Z: Zugtyp>(
    path_builder: &mut canvas::PathBuilder,
    radius: Radius,
    winkel: Angle,
) {
    let radius_abstand = radius.to_abstand();
    let spurweite = Z::SPURWEITE.to_abstand();
    // Koordinaten für den Bogen
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let radius_innen_abstand = radius_abstand - 0.5 * spurweite;
    let radius_innen: canvas::Radius = canvas::Radius(0.) + radius_innen_abstand;
    let radius_aussen_abstand = radius_abstand + 0.5 * spurweite;
    let radius_aussen: canvas::Radius = canvas::Radius(0.) + radius_aussen_abstand;
    let radius_aussen_abstand: canvas::Abstand = radius_aussen.to_abstand();
    let bogen_zentrum_y: canvas::Y = canvas::Y(0.) + abstand::<Z>() + radius_aussen.into();
    // Koordinaten links
    let gleis_links: canvas::X = canvas::X(0.);
    let beschraenkung_oben: canvas::Y = canvas::Y(0.);
    let gleis_links_oben: canvas::Y = beschraenkung_oben + abstand::<Z>();
    let gleis_links_unten: canvas::Y = gleis_links_oben + Z::SPURWEITE.to_abstand();
    // Koordinaten rechts
    let gleis_rechts_oben_x: canvas::X = gleis_links + radius_aussen_abstand * winkel.sin();
    let gleis_rechts_oben_y: canvas::Y =
        gleis_links_oben + radius_aussen_abstand * (1. - winkel.cos());
    let gleis_rechts_unten_x: canvas::X = gleis_rechts_oben_x - spurweite * winkel.sin();
    let gleis_rechts_unten_y: canvas::Y = gleis_rechts_oben_y + spurweite * winkel.cos();
    // obere Kurve
    path_builder.move_to(canvas::Point::new(gleis_links, gleis_links_unten));
    path_builder.arc(canvas::Arc {
        center: canvas::Point::new(gleis_links, bogen_zentrum_y),
        radius: radius_aussen,
        start: winkel_anfang,
        end: winkel_ende,
    });
    path_builder.line_to(canvas::Point::new(gleis_rechts_unten_x, gleis_rechts_unten_y));
    // untere Kurve
    path_builder.move_to(canvas::Point::new(gleis_links, gleis_links_oben));
    path_builder.arc(canvas::Arc {
        center: canvas::Point::new(gleis_links, bogen_zentrum_y),
        radius: radius_innen,
        start: winkel_anfang,
        end: winkel_ende,
    });
    path_builder.line_to(canvas::Point::new(gleis_rechts_oben_x, gleis_rechts_oben_y));
}
