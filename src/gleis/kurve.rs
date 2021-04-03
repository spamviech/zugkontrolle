//! Definition und zeichnen einer Kurve

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f64::consts::PI;
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

    fn width(&self) -> u64 {
        let factor = if self.angle.abs() < Angle::new(0.5 * PI) { self.angle.sin() } else { 1. };
        (radius_begrenzung_aussen::<Z>(self.radius) * factor).pixel()
    }

    fn height(&self) -> u64 {
        // Höhe des Bogen
        let angle_abs = self.angle.abs();
        let comparison = if angle_abs < Angle::new(0.5 * PI) {
            radius_begrenzung_aussen::<Z>(self.radius) * (1. - self.angle.cos())
                + beschraenkung::<Z>() * self.angle.cos()
        } else if angle_abs < Angle::new(PI) {
            radius_begrenzung_aussen::<Z>(self.radius) * (1. - self.angle.cos())
        } else {
            radius_begrenzung_aussen::<Z>(self.radius)
        };
        // Mindestgröße: Beschränkung einer Geraden
        beschraenkung::<Z>().max(&comparison).pixel()
    }

    fn zeichne(&self, cairo: &mut Cairo) {
        zeichne::<Z>(cairo, self.radius, self.angle.into(), Beschraenkung::Alle)
    }

    fn fuelle(&self, cairo: &mut Cairo) {
        fuelle::<Z>(cairo, self.radius, self.angle.into(), Rand::Alle);
        cairo.close_path()
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.),
                    y: CanvasY(0.) + 0.5 * beschraenkung::<Z>(),
                },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende: anchor::Point {
                position: anchor::Position {
                    x: CanvasX(0.) + CanvasAbstand::from(self.radius) * self.angle.sin(),
                    y: CanvasY(0.)
                        + (0.5 * beschraenkung::<Z>()
                            + CanvasAbstand::from(self.radius) * (1. - self.angle.cos())),
                },
                direction: anchor::Direction {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(self.angle.sin()),
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

pub(crate) fn zeichne<Z: Zugtyp>(
    cairo: &mut Cairo,
    radius: Radius,
    winkel: Angle,
    beschraenkungen: Beschraenkung,
) {
    let radius_abstand = CanvasAbstand::from(radius);
    let spurweite = CanvasAbstand::from(Z::SPURWEITE);
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let gleis_links: CanvasX = CanvasX(0.);
    let gleis_links_oben: CanvasY = CanvasY(0.);
    let gleis_links_unten: CanvasY = CanvasY(0.) + beschraenkung::<Z>();
    let radius_innen: CanvasRadius = CanvasRadius(0.) + radius_abstand - 0.5 * spurweite;
    let radius_aussen: CanvasRadius = CanvasRadius(0.) + radius_abstand + 0.5 * spurweite;
    let radius_begrenzung_aussen: CanvasAbstand =
        CanvasAbstand::from(radius_aussen) + abstand::<Z>();
    let begrenzung_x0: CanvasX = CanvasX(0.) + radius_begrenzung_aussen * winkel.sin();
    let begrenzung_y0: CanvasY = CanvasY(0.) + radius_begrenzung_aussen * (1. - winkel.cos());
    let begrenzung_x1: CanvasX = begrenzung_x0 - beschraenkung::<Z>() * winkel.sin();
    let begrenzung_y1: CanvasY = begrenzung_y0 + beschraenkung::<Z>() * winkel.cos();
    let bogen_zentrum_y: CanvasY = CanvasY(0.) + radius_begrenzung_aussen;
    // Beschränkungen
    if beschraenkungen.anfangs_beschraenkung() {
        cairo.move_to(gleis_links, gleis_links_oben);
        cairo.line_to(gleis_links, gleis_links_unten);
    }
    if beschraenkungen.end_beschraenkung() {
        cairo.move_to(begrenzung_x0, begrenzung_y0);
        cairo.line_to(begrenzung_x1, begrenzung_y1);
    }
    // Gleis
    cairo.arc(gleis_links, bogen_zentrum_y, radius_aussen, winkel_anfang, winkel_ende, true);
    cairo.arc(gleis_links, bogen_zentrum_y, radius_innen, winkel_anfang, winkel_ende, true);
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Rand {
    Innen,
    Aussen,
    Alle,
}
impl Rand {
    fn aussen(&self) -> bool {
        match self {
            Rand::Alle | Rand::Aussen => true,
            Rand::Innen => false,
        }
    }

    fn innen(&self) -> bool {
        match self {
            Rand::Alle | Rand::Innen => true,
            Rand::Aussen => false,
        }
    }
}
pub(crate) fn fuelle<Z: Zugtyp>(cairo: &mut Cairo, radius: Radius, winkel: Angle, rand: Rand) {
    let radius_abstand = CanvasAbstand::from(radius);
    let spurweite = CanvasAbstand::from(Z::SPURWEITE);
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let gleis_links: CanvasX = CanvasX(0.);
    let radius_innen_abstand = radius_abstand - 0.5 * spurweite;
    let radius_innen: CanvasRadius = CanvasRadius(0.) + radius_innen_abstand;
    let radius_aussen_abstand = radius_abstand + 0.5 * spurweite;
    let radius_aussen: CanvasRadius = CanvasRadius(0.) + radius_aussen_abstand;
    let bogen_zentrum_y: CanvasY = CanvasY(0.) + abstand::<Z>() + radius_aussen.into();
    // path schreiben
    if rand.aussen() {
        cairo.arc(gleis_links, bogen_zentrum_y, radius_aussen, winkel_anfang, winkel_ende, false)
    }
    if rand.innen() {
        cairo.arc_negative(
            gleis_links,
            bogen_zentrum_y,
            radius_innen,
            winkel_ende,
            winkel_anfang,
            false,
        )
    }
}
