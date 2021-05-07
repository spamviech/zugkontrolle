//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use super::Richtung;
use crate::gleis::typen::*;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Kurven-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct KurvenWeiche<Z> {
    pub zugtyp: PhantomData<Z>,
    pub länge: canvas::Abstand<canvas::X>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Winkel,
    pub richtung: Richtung,
    pub beschreibung: Option<String>,
}
impl<Z> KurvenWeiche<Z> {
    pub const fn new(length: Länge, radius: Radius, winkel: Winkel, richtung: Richtung) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            richtung,
            beschreibung: None,
        }
    }

    pub fn new_with_description(
        length: Länge,
        radius: Radius,
        winkel: Winkel,
        richtung: Richtung,
        description: impl Into<String>,
    ) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            richtung,
            beschreibung: Some(description.into()),
        }
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Innen,
    Außen,
}

impl<Z: Zugtyp> Zeichnen for KurvenWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> canvas::Size {
        let KurvenWeiche { länge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(länge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        canvas::Size { width: size_gerade.width + size_kurve.width, height: size_kurve.height }
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        // utility sizes
        let außen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.länge,
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                false,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                false,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                vec![außen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self) -> Vec<canvas::Path> {
        // utility sizes
        let außen_transformation = canvas::Transformation::Translate(canvas::Vector::new(
            canvas::X(0.) + self.länge,
            canvas::Y(0.),
        ));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(canvas::Vector {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Innere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                transformations.clone(),
                canvas::PathBuilder::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations,
                canvas::PathBuilder::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                Vec::new(),
                canvas::PathBuilder::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                vec![außen_transformation],
                canvas::PathBuilder::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn beschreibung(&self) -> Option<(canvas::Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let start_height: canvas::Y;
            let multiplier: f32;
            match self.richtung {
                Richtung::Rechts => {
                    start_height = canvas::Y(0.);
                    multiplier = 1.;
                },
                Richtung::Links => {
                    start_height = canvas::Y(0.) + self.size().height;
                    multiplier = -1.;
                },
            };
            (
                canvas::Position {
                    point: canvas::Point::new(
                        canvas::X(0.) + self.länge.min(&(0.5 * self.size().width)),
                        start_height + multiplier * 0.5 * beschränkung::<Z>(),
                    ),
                    winkel: Winkel::new(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: canvas::Vector) -> bool {
        // utility sizes
        let start_x: canvas::X = canvas::X(0.);
        let start_height: canvas::Y;
        let multiplier: f32;
        match self.richtung {
            Richtung::Rechts => {
                start_height = canvas::Y(0.);
                multiplier = 1.;
            },
            Richtung::Links => {
                start_height = canvas::Y(0.) + self.size().height;
                multiplier = -1.;
            },
        };
        let start_vector = canvas::Vector::new(start_x, start_height);
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.dy *= multiplier;
        let verschoben_vector =
            relative_vector - canvas::Vector { dx: self.länge, dy: canvas::Y(0.).to_abstand() };
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, verschoben_vector)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: canvas::Y;
        let multiplier: f32;
        match self.richtung {
            Richtung::Rechts => {
                start_height = canvas::Y(0.);
                multiplier = 1.;
            },
            Richtung::Links => {
                start_height = canvas::Y(0.) + self.size().height;
                multiplier = -1.;
            },
        };
        let halbe_beschränkung: canvas::Abstand<canvas::Y> = 0.5 * beschränkung::<Z>();
        let radius_abstand: canvas::Abstand<canvas::Radius> = self.radius;
        let kurve_anchor_direction: canvas::Vector = canvas::Vector::new(
            canvas::X(self.winkel.cos()),
            canvas::Y(multiplier * self.winkel.sin()),
        );
        let kurve_anchor_x: canvas::X = canvas::X(0.) + radius_abstand.as_x() * self.winkel.sin();
        let kurve_anchor_y: canvas::Y = start_height
            + multiplier * (halbe_beschränkung + radius_abstand.as_y() * (1. - self.winkel.cos()));
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.),
                    y: start_height + multiplier * halbe_beschränkung,
                },
                direction: canvas::Vector::new(canvas::X(-1.), canvas::Y(multiplier * 0.)),
            },
            innen: anchor::Anchor {
                position: canvas::Point { x: kurve_anchor_x, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
            außen: anchor::Anchor {
                position: canvas::Point { x: kurve_anchor_x + self.länge, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
        }
    }
}
