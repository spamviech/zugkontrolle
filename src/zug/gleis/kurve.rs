//! Definition und zeichnen einer Kurve

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::collections::HashMap;
use std::f64::consts::PI;
use std::marker::PhantomData;

use super::anchor::*;
use super::types::*;
use super::widget::*;

/// Definition einer Kurve
#[derive(Debug, Clone)]
pub struct Kurve<T> {
    pub zugtyp: PhantomData<*const T>,
    pub radius: Radius,
    pub angle: AngleDegrees,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum KurveAnchors {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Kurve<Z> {
    type AnchorName = KurveAnchors;

    fn width(&self) -> u64 {
        let factor = if self.angle.abs() < Angle(0.5 * PI) { self.angle.sin() } else { 1. };
        (Z::beschraenkung() * factor).pixel()
    }

    fn height(&self) -> u64 {
        // Höhe des Bogen
        let angle_abs = self.angle.abs();
        let comparison = if angle_abs < Angle(0.5 * PI) {
            Z::radius_begrenzung(self.radius) * (1. - self.angle.cos())
                + Z::beschraenkung() * self.angle.cos()
        } else if angle_abs < Angle(PI) {
            Z::radius_begrenzung(self.radius) * (1. - self.angle.cos())
        } else {
            Z::radius_begrenzung(self.radius)
        };
        // Mindestgröße: Beschränkung einer Geraden
        Z::beschraenkung().max(&comparison).pixel()
    }

    fn zeichne(&self, cairo: Cairo) {
        zeichne_kurve::<Z>(cairo, self.radius, self.angle.into(), KurvenBeschraenkung::Alle)
    }

    fn anchor_points(&self) -> AnchorPointMap<Self::AnchorName> {
        let mut anchor_points = HashMap::with_capacity(2);
        anchor_points.insert(
            KurveAnchors::Anfang,
            AnchorPoint {
                position: AnchorPosition {
                    x: CanvasX::default(),
                    y: CanvasY::default() + 0.5 * Z::beschraenkung(),
                },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
        );
        anchor_points.insert(
            KurveAnchors::Ende,
            AnchorPoint {
                position: AnchorPosition {
                    x: CanvasX(self.radius.0 * self.angle.sin()),
                    y: CanvasY::default()
                        + (0.5 * Z::beschraenkung()
                            + CanvasAbstand::new(self.radius.0) * (1. - self.angle.cos())),
                },
                direction: AnchorDirection {
                    dx: CanvasX(self.angle.cos()),
                    dy: CanvasY(self.angle.sin()),
                },
            },
        );
        anchor_points
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum KurvenBeschraenkung {
    Keine,
    Anfang,
    Ende,
    Alle,
}

impl KurvenBeschraenkung {
    fn anfangs_beschraenkung(&self) -> bool {
        match self {
            KurvenBeschraenkung::Anfang | KurvenBeschraenkung::Alle => true,
            KurvenBeschraenkung::Keine | KurvenBeschraenkung::Ende => false,
        }
    }

    fn end_beschraenkung(&self) -> bool {
        match self {
            KurvenBeschraenkung::Ende | KurvenBeschraenkung::Alle => true,
            KurvenBeschraenkung::Keine | KurvenBeschraenkung::Anfang => false,
        }
    }
}

pub(crate) fn zeichne_kurve<Z: Zugtyp>(
    cairo: Cairo,
    radius: Radius,
    winkel: Angle,
    beschraenkungen: KurvenBeschraenkung,
) {
    let radius_abstand = CanvasAbstand::new(radius.0);
    let spurweite = CanvasAbstand::new(Z::spurweite.0);
    let winkel_anfang: Angle = Angle(3. * PI / 2.);
    let gleis_links: CanvasX = CanvasX::default();
    let gleis_links_oben: CanvasY = CanvasY::default();
    let gleis_links_unten: CanvasY = CanvasY::default() + Z::beschraenkung();
    let radius_innen: CanvasRadius = CanvasRadius::default() + radius_abstand - 0.5 * spurweite;
    let radius_aussen: CanvasRadius = CanvasRadius::default() + radius_abstand + 0.5 * spurweite;
    let radius_begrenzung_aussen: CanvasAbstand = CanvasAbstand::from(radius_aussen) + Z::abstand;
    let begrenzung_x0: CanvasX = CanvasX::default() + radius_begrenzung_aussen * winkel.sin();
    let begrenzung_y0: CanvasY =
        CanvasY::default() + radius_begrenzung_aussen * (1. - winkel.cos());
    let begrenzung_x1: CanvasX = begrenzung_x0 - Z::beschraenkung() * winkel.sin();
    let begrenzung_y1: CanvasY = begrenzung_y0 + Z::beschraenkung() * winkel.cos();
    let bogen_zentrum_y: CanvasY = CanvasY::default() + Z::abstand + radius_aussen.into();
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
    cairo.arc(gleis_links, bogen_zentrum_y, radius_aussen, winkel_anfang, winkel_anfang + winkel);
    cairo.arc(gleis_links, bogen_zentrum_y, radius_innen, winkel_anfang, winkel_anfang + winkel);
}
