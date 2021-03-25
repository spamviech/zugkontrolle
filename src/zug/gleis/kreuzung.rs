//! Definition und zeichnen einer Kreuzung

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::anchor::*;
use super::gerade::Gerade;
use super::kurve::{self, zeichne_kurve, Kurve};
use super::types::*;
use super::widget::{AnchorLookup, Zeichnen};

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
pub enum AnchorName {
    Anfang0,
    Ende0,
    Anfang1,
    Ende1,
}
#[derive(Debug)]
pub struct AnchorPoints {
    anfang0: AnchorPoint,
    ende0: AnchorPoint,
    anfang1: AnchorPoint,
    ende1: AnchorPoint,
}

impl<Z: Zugtyp> Zeichnen for Kreuzung<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

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
            zeichne_kurve::<Z>(cairo, self.radius, self.angle.into(), kurve::Beschraenkung::Keine);
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
            zeichne_kurve::<Z>(cairo, self.radius, self.angle.into(), kurve::Beschraenkung::Keine);
        }
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let width: CanvasX = CanvasX(self.width() as f64);
        let anfang0_x: CanvasX = CanvasX::default();
        let ende0_x: CanvasX = anfang0_x + CanvasAbstand::new(self.length.0);
        let half_height: CanvasY = CanvasY(0.5 * (self.height() as f64));
        let radius_abstand: CanvasAbstand = CanvasAbstand::new(self.radius.0);
        let anfang1_x: CanvasX = CanvasX::default() + radius_abstand * self.angle.sin();
        let anfang1_y: CanvasY = half_height + radius_abstand * (1. - self.angle.cos());
        let ende1_x: CanvasX = width - radius_abstand * self.angle.sin();
        let ende1_y: CanvasY = half_height - radius_abstand * (1. - self.angle.cos());
        AnchorPoints {
            anfang0: AnchorPoint {
                position: AnchorPosition { x: anfang0_x, y: half_height },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende0: AnchorPoint {
                position: AnchorPosition { x: ende0_x, y: half_height },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
            anfang1: AnchorPoint {
                position: AnchorPosition { x: anfang1_x, y: anfang1_y },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende1: AnchorPoint {
                position: AnchorPosition { x: ende1_x, y: ende1_y },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        }
    }
}

impl AnchorLookup<AnchorName> for AnchorPoints {
    fn get(&self, key: AnchorName) -> &AnchorPoint {
        match key {
            AnchorName::Anfang0 => &self.anfang0,
            AnchorName::Ende0 => &self.ende0,
            AnchorName::Anfang1 => &self.anfang1,
            AnchorName::Ende1 => &self.ende1,
        }
    }
    fn get_mut(&mut self, key: AnchorName) -> &mut AnchorPoint {
        match key {
            AnchorName::Anfang0 => &mut self.anfang0,
            AnchorName::Ende0 => &mut self.ende0,
            AnchorName::Anfang1 => &mut self.anfang1,
            AnchorName::Ende1 => &mut self.ende1,
        }
    }
    fn map<F: FnMut(&AnchorPoint)>(&self, mut action: F) {
        action(&self.anfang0);
        action(&self.ende0);
        action(&self.anfang1);
        action(&self.ende1);
    }
}
