//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::Richtung;
use crate::gleis::types::*;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Kurven-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub laenge: canvas::Abstand<canvas::X>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Angle,
    pub richtung: Richtung,
    pub beschreibung: Option<&'static str>,
}
impl<Z> KurvenWeiche<Z> {
    pub const fn new(length: Length, radius: Radius, angle: Angle, richtung: Richtung) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            richtung,
            beschreibung: None,
        }
    }
    pub const fn new_with_description(
        length: Length,
        radius: Radius,
        angle: Angle,
        richtung: Richtung,
        description: &'static str,
    ) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            laenge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel: angle,
            richtung,
            beschreibung: Some(description),
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Innen,
    Aussen,
}

impl<Z: Zugtyp> Zeichnen for KurvenWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> canvas::Size {
        let KurvenWeiche { laenge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(laenge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        canvas::Size {
            width: canvas::X(0.)
                + size_gerade.width.to_abstand().max(&size_kurve.width.to_abstand()),
            height: size_kurve.height,
        }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let aussen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.laenge,
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Alle,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.laenge,
                false,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(aussen_transformation);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Ende,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Alle,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.laenge,
                false,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschraenkung::Ende,
                vec![aussen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let aussen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.laenge,
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector::new(
                canvas::X(0.),
                self.size().height,
            ))];
            // Innere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.laenge,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(aussen_transformation);
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fuelle(
                self.zugtyp,
                self.laenge,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::fuelle(
                self.zugtyp,
                self.radius,
                self.winkel,
                vec![aussen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: canvas::Y;
        let multiplier: f32;
        match self.richtung {
            Richtung::Rechts => {
                start_height = canvas::Y(0.);
                multiplier = 1.;
            }
            Richtung::Links => {
                start_height = self.size().height;
                multiplier = -1.;
            }
        };
        let halbe_beschraenkung: canvas::Abstand<canvas::Y> = 0.5 * beschraenkung::<Z>();
        let radius_abstand: canvas::Abstand<canvas::Radius> = self.radius;
        let kurve_anchor_direction: canvas::Vector = canvas::Vector {
            dx: canvas::X(self.winkel.cos()),
            dy: canvas::Y(multiplier * self.winkel.sin()),
        };
        let kurve_anchor_x: canvas::X = canvas::X(0.) + radius_abstand.as_x() * self.winkel.sin();
        let kurve_anchor_y: canvas::Y = start_height
            + multiplier * (halbe_beschraenkung + radius_abstand.as_y() * (1. - self.winkel.cos()));
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.),
                    y: start_height + multiplier * halbe_beschraenkung,
                },
                direction: canvas::Vector { dx: canvas::X(-1.), dy: canvas::Y(multiplier * 0.) },
            },
            innen: anchor::Anchor {
                position: canvas::Point { x: kurve_anchor_x, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
            aussen: anchor::Anchor {
                position: canvas::Point { x: kurve_anchor_x + self.laenge, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
        }
    }
}
