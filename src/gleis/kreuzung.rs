//! Definition und zeichnen einer Kreuzung

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::anchor;
use super::gerade::{self};
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

    fn size(&self) -> canvas::Size {
        let size_kurve =
            Kurve { zugtyp: self.zugtyp, radius: self.radius, angle: self.angle().into() }.size();
        let height_beschraenkung = beschraenkung::<Z>();
        let height_kurven = 2. * size_kurve.height.to_abstand() - height_beschraenkung;
        canvas::Size::new(
            canvas::X(0.) + self.length.to_abstand().max(&size_kurve.width.to_abstand()),
            canvas::Y(0.) + height_beschraenkung.max(&height_kurven),
        )
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let size: canvas::Size = self.size();
        let width: canvas::X = size.width;
        let half_width: canvas::X = canvas::X(0.5 * width.0);
        let start_x: canvas::X = canvas::X(0.);
        let height: canvas::Y = size.height;
        let half_height: canvas::Y = canvas::Y(0.5 * height.0);
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
        let angle = self.angle();
        let zeichne_kontur = |path_builder: &mut canvas::PathBuilder| {
            gerade::zeichne::<Z>(path_builder, self.length);
            if self.variante == Variante::MitKurve {
                kurve::zeichne::<Z>(path_builder, self.radius, angle, kurve::Beschraenkung::Keine);
            }
        };
        // horizontale Gerade + erste Kurve
        let mut horizontal_builder =
            canvas::PathBuilder::new_with_transformations(vec![canvas::Transformation::Translate(
                canvas::Vector::new(start_x, start_y),
            )]);
        zeichne_kontur(&mut horizontal_builder);
        // gedrehte Gerade + zweite Kurve
        let mut gedreht_builder = canvas::PathBuilder::new_with_transformations(vec![
            canvas::Transformation::Translate(canvas::Vector::new(half_width, half_height)),
            canvas::Transformation::Rotate(angle),
            // transformations with assumed inverted y-Axis
            canvas::Transformation::Translate(canvas::Vector::new(-half_width, half_height)),
            canvas::Transformation::Translate(canvas::Vector::new(start_x, -start_y)),
        ]);
        gedreht_builder.with_invert_y(|path_builder| zeichne_kontur(path_builder));
        // return value
        vec![horizontal_builder.build(), gedreht_builder.build()]
    }

    /*
    fn fuelle(&self, cairo: &mut Cairo) {
        // utility sizes
        let width: canvas::X = canvas::X(self.width() as f64);
        let half_width: canvas::X = canvas::X(0.5 * width.0);
        let start_x: canvas::X = canvas::X(0.);
        let height: canvas::Y = canvas::Y(self.height() as f64);
        let half_height: canvas::Y = canvas::Y(0.5 * height.0);
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z>();
        let angle = self.angle();
        let spurweite: CanvasAbstand = Z::SPURWEITE.into();
        let translated_gerade_oben_y: canvas::Y = canvas::Y(0.) + abstand::<Z>();
        let translated_gerade_unten_y: canvas::Y = translated_gerade_oben_y + spurweite;
        let translated_gerade_links_x: canvas::X = canvas::X(0.);
        // approximation to avoid divide-by-zero error
        let inv_tan: f64 = if angle.cos() < 0.01 { 0. } else { 1. / angle.tan() };
        let inv_sin: f64 = if angle.sin() < 0.5 * spurweite / width.to_abstand() {
            2. * width.to_abstand() / spurweite
        } else {
            1. / angle.sin()
        };
        let delta_x: CanvasAbstand = 0.5 * spurweite * (inv_sin + inv_tan);
        let delta_x_oben_unten: CanvasAbstand = spurweite * inv_tan;
        let translated_oben_links_ueberschneiden_x: canvas::X =
            translated_gerade_links_x + 0.5 * width.to_abstand() - delta_x;
        let translated_unten_links_ueberschneiden_x: canvas::X =
            translated_oben_links_ueberschneiden_x + delta_x_oben_unten;
        let translated_unten_rechts_ueberschneiden_x: canvas::X =
            translated_gerade_links_x + 0.5 * width.to_abstand() + delta_x;
        let translated_gedreht_unten_rechts_x: canvas::X = translated_unten_links_ueberschneiden_x
            + translated_unten_links_ueberschneiden_x.to_abstand() * angle.cos();
        let translated_gedreht_unten_rechts_y: canvas::Y = translated_gerade_unten_y
            + translated_unten_links_ueberschneiden_x.to_abstand() * angle.sin();
        let translated_gedreht_oben_rechts_x: canvas::X =
            translated_gedreht_unten_rechts_x + spurweite * angle.sin();
        let translated_gedreht_oben_rechts_y: canvas::Y =
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
        cairo.with_save_restore(zeichne_rand);
        cairo.new_sub_path();
        // gedrehte Gerade + zweite Kurve
        cairo.translate(half_width, half_height);
        cairo.rotate(angle);
        cairo.transform(Matrix { x0: 0., y0: 0., xx: 1., xy: 0., yx: 0., yy: -1. });
        cairo.translate(-half_width, -half_height);
        zeichne_rand(cairo)
    }
    */

    fn anchor_points(&self) -> Self::AnchorPoints {
        let canvas::Size { width, height } = self.size();
        let anfang0_x: canvas::X = canvas::X(0.);
        let ende0_x: canvas::X = anfang0_x + self.length.to_abstand();
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height.to_abstand();
        let radius_abstand: canvas::Abstand = self.radius.to_abstand();
        let angle = self.angle();
        let anfang1_x: canvas::X = canvas::X(0.) + radius_abstand * angle.sin();
        let anfang1_y: canvas::Y = half_height + radius_abstand * (1. - angle.cos());
        let ende1_x: canvas::X = width - radius_abstand * angle.sin();
        let ende1_y: canvas::Y = half_height - radius_abstand * (1. - angle.cos());
        AnchorPoints {
            anfang_0: anchor::Point {
                position: anchor::Position { x: anfang0_x, y: half_height },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(0.) },
            },
            ende_0: anchor::Point {
                position: anchor::Position { x: ende0_x, y: half_height },
                direction: anchor::Direction { dx: canvas::X(1.), dy: canvas::Y(0.) },
            },
            anfang_1: anchor::Point {
                position: anchor::Position { x: anfang1_x, y: anfang1_y },
                direction: anchor::Direction {
                    dx: canvas::X(angle.cos()),
                    dy: canvas::Y(angle.sin()),
                },
            },
            ende_1: anchor::Point {
                position: anchor::Position { x: ende1_x, y: ende1_y },
                direction: anchor::Direction {
                    dx: canvas::X(-angle.cos()),
                    dy: canvas::Y(-angle.sin()),
                },
            },
        }
    }
}
