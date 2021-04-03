//! Definition und zeichnen einer Kreuzung

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::anchor;
use super::gerade::Gerade;
use super::kurve::{self, Kurve};
use super::types::*;

/// Definition einer Kreuzung
#[derive(Debug, Clone)]
pub struct Kreuzung<T> {
    pub zugtyp: PhantomData<*const T>,
    pub length: Length,
    pub radius: Radius,
    pub variante: Variante,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variante {
    MitKurve,
    OhneKurve,
}
impl<Z> Kreuzung<Z> {
    fn angle(&self) -> Angle {
        // angle solves the formula `x = L/2 * (1 + sin(alpha)) = R * cos(alpha)`
        // https://www.wolframalpha.com/input/?i=sin%28alpha%29-C*cos%28alpha%29%3DC
        // length=0 gives angle=0, but is not properly defined,
        // since it violates the formula above (pi/2 required)
        // pi/2 doesn't work either, since it violates the formula
        // `y = L/2 * sin(alpha) = R * (1 - cos(alpha))`
        // only for radius=0 as well both formulas are satisfied by any angle
        Angle::new(2. * (0.5 * (self.length / self.radius)).atan())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang0,
    Ende0,
    Anfang1,
    Ende1,
}

impl<Z: Zugtyp> Zeichnen for Kreuzung<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn width(&self) -> u64 {
        let width_kurve =
            Kurve { zugtyp: self.zugtyp, radius: self.radius, angle: self.angle().into() }.width();
        CanvasAbstand::from(self.length).pixel().max(width_kurve)
    }

    fn height(&self) -> u64 {
        let height_beschraenkung = beschraenkung::<Z>().pixel();
        let height_kurve =
            Kurve { zugtyp: self.zugtyp, radius: self.radius, angle: self.angle().into() }.height();
        let height_kurven = 2 * height_kurve - height_beschraenkung;
        height_beschraenkung.max(height_kurven)
    }

    fn zeichne(&self, cairo: &mut Cairo) {
        // utility sizes
        let width: CanvasX = CanvasX(self.width() as f64);
        let half_width: CanvasX = CanvasX(0.5 * width.0);
        let start_x: CanvasX = CanvasX(0.);
        let height: CanvasY = CanvasY(self.height() as f64);
        let half_height: CanvasY = CanvasY(0.5 * height.0);
        let start_y: CanvasY = half_height - 0.5 * beschraenkung::<Z>();
        let gerade = Gerade { zugtyp: self.zugtyp, length: self.length };
        let angle = self.angle();
        let zeichne_kontur = |cairo: &mut Cairo| {
            cairo.translate(start_x, start_y);
            gerade.zeichne(cairo);
            if self.variante == Variante::MitKurve {
                kurve::zeichne::<Z>(cairo, self.radius, angle, kurve::Beschraenkung::Keine);
            }
        };
        // horizontale Gerade + erste Kurve
        cairo.with_save_restore(zeichne_kontur);
        // gedrehte Gerade + zweite Kurve
        cairo.translate(half_width, half_height);
        cairo.rotate(angle);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        zeichne_kontur(cairo);
    }

    fn fuelle(&self, cairo: &mut Cairo) {
        // utility sizes
        let width: CanvasX = CanvasX(self.width() as f64);
        let half_width: CanvasX = CanvasX(0.5 * width.0);
        let start_x: CanvasX = CanvasX(0.);
        let height: CanvasY = CanvasY(self.height() as f64);
        let half_height: CanvasY = CanvasY(0.5 * height.0);
        let start_y: CanvasY = half_height - 0.5 * beschraenkung::<Z>();
        let angle = self.angle();
        let spurweite: CanvasAbstand = Z::SPURWEITE.into();
        let translated_gerade_oben_y: CanvasY = CanvasY(0.) + abstand::<Z>();
        let translated_gerade_unten_y: CanvasY = translated_gerade_oben_y + spurweite;
        let translated_gerade_links_x: CanvasX = CanvasX(0.);
        // approximation to avoid divide-by-zero error
        let inv_tan: f64 = if angle.cos() < 0.01 { 0. } else { 1. / angle.tan() };
        let inv_sin: f64 = if angle.sin() < 0.5 * spurweite / width.to_abstand() {
            2. * width.to_abstand() / spurweite
        } else {
            1. / angle.sin()
        };
        let delta_x: CanvasAbstand = 0.5 * spurweite * (inv_sin + inv_tan);
        let delta_x_oben_unten: CanvasAbstand = spurweite * inv_tan;
        let translated_oben_links_ueberschneiden_x: CanvasX =
            translated_gerade_links_x + 0.5 * CanvasAbstand::from(width) - delta_x;
        let translated_unten_links_ueberschneiden_x: CanvasX =
            translated_oben_links_ueberschneiden_x + delta_x_oben_unten;
        let translated_unten_rechts_ueberschneiden_x: CanvasX =
            translated_gerade_links_x + 0.5 * CanvasAbstand::from(width) + delta_x;
        let translated_gedreht_unten_rechts_x: CanvasX = translated_unten_links_ueberschneiden_x
            + translated_unten_links_ueberschneiden_x.to_abstand() * angle.cos();
        let translated_gedreht_unten_rechts_y: CanvasY = translated_gerade_unten_y
            + translated_unten_links_ueberschneiden_x.to_abstand() * angle.sin();
        let translated_gedreht_oben_rechts_x: CanvasX =
            translated_gedreht_unten_rechts_x + spurweite * angle.sin();
        let translated_gedreht_oben_rechts_y: CanvasY =
            translated_gedreht_unten_rechts_y - spurweite * angle.cos();
        let zeichne_rand = |cairo: &mut Cairo| {
            cairo.translate(start_x, start_y);
            cairo.line_to(translated_unten_rechts_ueberschneiden_x, translated_gerade_unten_y);
            cairo.line_to(translated_gedreht_oben_rechts_x, translated_gedreht_oben_rechts_y);
            cairo.line_to(translated_gedreht_unten_rechts_x, translated_gedreht_unten_rechts_y);
            if self.variante == Variante::MitKurve {
                kurve::fuelle::<Z>(cairo, self.radius, angle, kurve::Rand::Innen);
            } else {
                cairo.line_to(translated_oben_links_ueberschneiden_x, translated_gerade_unten_y);
                cairo.line_to(translated_gerade_links_x, translated_gerade_unten_y);
            }
            cairo.line_to(translated_gerade_links_x, translated_gerade_oben_y);
            cairo.line_to(translated_oben_links_ueberschneiden_x, translated_gerade_oben_y);
        };
        // horizontale Gerade + erste Kurve
        cairo.new_path();
        cairo.with_save_restore(zeichne_rand);
        cairo.new_sub_path();
        // gedrehte Gerade + zweite Kurve
        cairo.translate(half_width, half_height);
        cairo.rotate(angle);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        zeichne_rand(cairo)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let width: CanvasX = CanvasX(self.width() as f64);
        let anfang0_x: CanvasX = CanvasX(0.);
        let ende0_x: CanvasX = anfang0_x + CanvasAbstand::from(self.length);
        let half_height: CanvasY = CanvasY(0.5 * (self.height() as f64));
        let radius_abstand: CanvasAbstand = CanvasAbstand::from(self.radius);
        let angle = self.angle();
        let anfang1_x: CanvasX = CanvasX(0.) + radius_abstand * angle.sin();
        let anfang1_y: CanvasY = half_height + radius_abstand * (1. - angle.cos());
        let ende1_x: CanvasX = width - radius_abstand * angle.sin();
        let ende1_y: CanvasY = half_height - radius_abstand * (1. - angle.cos());
        AnchorPoints {
            anfang_0: anchor::Point {
                position: anchor::Position { x: anfang0_x, y: half_height },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende_0: anchor::Point {
                position: anchor::Position { x: ende0_x, y: half_height },
                direction: anchor::Direction { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
            anfang_1: anchor::Point {
                position: anchor::Position { x: anfang1_x, y: anfang1_y },
                direction: anchor::Direction { dx: CanvasX(angle.cos()), dy: CanvasY(angle.sin()) },
            },
            ende_1: anchor::Point {
                position: anchor::Position { x: ende1_x, y: ende1_y },
                direction: anchor::Direction {
                    dx: CanvasX(-angle.cos()),
                    dy: CanvasY(-angle.sin()),
                },
            },
        }
    }
}
