//! Definition und zeichnen einer Kreuzung

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::collections::HashMap;
use std::marker::PhantomData;

use super::anchor::*;
use super::gerade::Gerade;
use super::kurve::*;
use super::types::*;
use super::widget::Zeichnen;

/// Definition einer Kreuzung
#[derive(Debug, Clone)]
pub struct Kreuzung<T> {
    pub zugtyp: PhantomData<*const T>,
    pub length: Length,
    pub radius: Radius,
    // TODO: winkel kann aus radius und länge berechnet werden?
    pub angle: AngleDegrees,
    pub kreuzungs_art: KreuzungsArt,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KreuzungsArt {
    MitKurve,
    OhneKurve,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum KreuzungAnchors {
    Anfang0,
    Ende0,
    Anfang1,
    Ende1,
}

impl<Z: Zugtyp> Zeichnen for Kreuzung<Z> {
    type AnchorName = KreuzungAnchors;

    fn width(&self) -> u64 {
        let width_kurve =
            Kurve { zugtyp: self.zugtyp, radius: self.radius, angle: self.angle }.width();
        CanvasAbstand::new(self.length.0).pixel().max(width_kurve)
    }

    fn height(&self) -> u64 {
        let height_beschraenkung = Z::beschraenkung().pixel();
        let height_kurve =
            Kurve { zugtyp: self.zugtyp, radius: self.radius, angle: self.angle }.height();
        let height_kurven = 2 * height_kurve - height_beschraenkung;
        height_beschraenkung.max(height_kurven)
    }

    fn zeichne(&self, cairo: &Cairo) {
        // utility sizes
        let width: CanvasX = CanvasX(self.width() as f64);
        let half_width: CanvasX = CanvasX(0.5 * width.0);
        let start_x: CanvasX = CanvasX::default();
        let height: CanvasY = CanvasY(self.height() as f64);
        let half_height: CanvasY = CanvasY(0.5 * height.0);
        let start_y: CanvasY = half_height - 0.5 * Z::beschraenkung();
        let gerade = Gerade { zugtyp: self.zugtyp, length: self.length };
        // horizontale Gerade + erste Kurve
        cairo.save();
        cairo.translate(start_x, start_y);
        gerade.zeichne(cairo);
        if self.kreuzungs_art == KreuzungsArt::MitKurve {
            zeichne_kurve::<Z>(cairo, self.radius, self.angle.into(), KurvenBeschraenkung::Keine);
        }
        cairo.restore();
        // gedrehte Gerade + zweite Kurve
        cairo.translate(half_width, half_height);
        cairo.rotate(self.angle.into());
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        cairo.translate(start_x, start_y);
        gerade.zeichne(cairo);
        if self.kreuzungs_art == KreuzungsArt::MitKurve {
            zeichne_kurve::<Z>(cairo, self.radius, self.angle.into(), KurvenBeschraenkung::Keine);
        }
    }

    fn anchor_points(&self) -> AnchorPointMap<Self::AnchorName> {
        let width: CanvasX = CanvasX(self.width() as f64);
        let anfang0_x: CanvasX = CanvasX::default();
        let ende0_x: CanvasX = anfang0_x + CanvasAbstand::new(self.length.0);
        let half_height: CanvasY = CanvasY(0.5 * (self.height() as f64));
        let radius_abstand: CanvasAbstand = CanvasAbstand::new(self.radius.0);
        let anfang1_x: CanvasX = CanvasX::default() + radius_abstand * self.angle.sin();
        let anfang1_y: CanvasY = half_height + radius_abstand * (1. - self.angle.cos());
        let ende1_x: CanvasX = width - radius_abstand * self.angle.sin();
        let ende1_y: CanvasY = half_height - radius_abstand * (1. - self.angle.cos());
        let mut anchor_points = HashMap::with_capacity(2);
        anchor_points.insert(
            KreuzungAnchors::Anfang0,
            AnchorPoint {
                position: AnchorPosition { x: anfang0_x, y: half_height },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
        );
        anchor_points.insert(
            KreuzungAnchors::Ende0,
            AnchorPoint {
                position: AnchorPosition { x: ende0_x, y: half_height },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        );
        anchor_points.insert(
            KreuzungAnchors::Anfang1,
            AnchorPoint {
                position: AnchorPosition { x: anfang1_x, y: anfang1_y },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
        );
        anchor_points.insert(
            KreuzungAnchors::Ende1,
            AnchorPoint {
                position: AnchorPosition { x: ende1_x, y: ende1_y },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        );
        anchor_points
    }
}
