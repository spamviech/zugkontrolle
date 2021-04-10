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
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
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
        let height_beschraenkung = beschraenkung::<Z, canvas::Y>();
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
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z, canvas::Y>();
        let angle = self.angle();
        let mut paths = Vec::new();
        // Geraden
        let horizontal_transformations =
            vec![canvas::Transformation::Translate(canvas::Vector::new(start_x, start_y))];
        let gedreht_transformations = vec![
            canvas::Transformation::Translate(canvas::Vector::new(half_width, half_height)),
            canvas::Transformation::Rotate(angle),
            // transformations with assumed inverted y-Axis
            canvas::Transformation::Translate(canvas::Vector::new(-half_width, half_height)),
            canvas::Transformation::Translate(canvas::Vector::new(start_x, -start_y)),
        ];
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.length,
            true,
            horizontal_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.length,
            true,
            gedreht_transformations.clone(),
            canvas::PathBuilder::with_invert_y,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                angle,
                kurve::Beschraenkung::Keine,
                horizontal_transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                angle,
                kurve::Beschraenkung::Keine,
                gedreht_transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let size: canvas::Size = self.size();
        let width: canvas::X = size.width;
        let half_width: canvas::X = canvas::X(0.5 * width.0);
        let start_x: canvas::X = canvas::X(0.);
        let height: canvas::Y = size.height;
        let half_height: canvas::Y = canvas::Y(0.5 * height.0);
        let start_y: canvas::Y = half_height - 0.5 * beschraenkung::<Z, canvas::Y>();
        let angle = self.angle();
        let mut paths = Vec::new();
        // Geraden
        let horizontal_transformations =
            vec![canvas::Transformation::Translate(canvas::Vector::new(start_x, start_y))];
        let gedreht_transformations = vec![
            canvas::Transformation::Translate(canvas::Vector::new(half_width, half_height)),
            canvas::Transformation::Rotate(angle),
            // transformations with assumed inverted y-Axis
            canvas::Transformation::Translate(canvas::Vector::new(-half_width, half_height)),
            canvas::Transformation::Translate(canvas::Vector::new(start_x, -start_y)),
        ];
        paths.push(gerade::fuelle(
            self.zugtyp,
            self.length,
            horizontal_transformations.clone(),
            canvas::PathBuilder::with_normal_axis,
        ));
        paths.push(gerade::fuelle(
            self.zugtyp,
            self.length,
            gedreht_transformations.clone(),
            canvas::PathBuilder::with_invert_y,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                angle,
                horizontal_transformations,
                canvas::PathBuilder::with_normal_axis,
            ));
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                angle,
                gedreht_transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let canvas::Size { width, height } = self.size();
        let anfang0_x: canvas::X = canvas::X(0.);
        let ende0_x: canvas::X = anfang0_x + self.length.to_abstand();
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height.to_abstand();
        let radius_abstand: canvas::Abstand<canvas::Radius> = self.radius.to_abstand();
        let radius_abstand_x: canvas::Abstand<canvas::X> = radius_abstand.convert();
        let radius_abstand_y: canvas::Abstand<canvas::Y> = radius_abstand.convert();
        let angle = self.angle();
        let anfang1_x: canvas::X = canvas::X(0.) + radius_abstand_x * angle.sin();
        let anfang1_y: canvas::Y = half_height + radius_abstand_y * (1. - angle.cos());
        let ende1_x: canvas::X = width - radius_abstand_x * angle.sin();
        let ende1_y: canvas::Y = half_height - radius_abstand_y * (1. - angle.cos());
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
