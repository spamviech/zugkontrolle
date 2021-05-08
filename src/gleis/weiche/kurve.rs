//! Definition und zeichnen einer Weiche

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
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub richtung: Richtung,
    pub beschreibung: Option<String>,
}
impl<Z> KurvenWeiche<Z> {
    pub const fn new(länge: Länge, radius: Radius, winkel: Winkel, richtung: Richtung) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: None,
        }
    }

    pub fn new_with_description(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        richtung: Richtung,
        description: impl Into<String>,
    ) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
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

    fn size(&self) -> Vektor {
        let KurvenWeiche { länge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(länge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        Vektor { width: size_gerade.width + size_kurve.width, height: size_kurve.height }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor::new(canvas::X(0.) + self.länge, canvas::Y(0.)));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![Transformation::Translation(Vektor {
                dx: canvas::X(0.).als_skalar(),
                dy: self.size().height,
            })];
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                false,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                transformations,
                pfad::Erbauer::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                false,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                vec![außen_transformation],
                pfad::Erbauer::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self) -> Vec<Pfad> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor::new(canvas::X(0.) + self.länge, canvas::Y(0.)));
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![Transformation::Translation(Vektor {
                dx: canvas::X(0.).als_skalar(),
                dy: self.size().height,
            })];
            // Innere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations,
                pfad::Erbauer::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                vec![außen_transformation],
                pfad::Erbauer::with_normal_axis,
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
                    point: Vektor::new(
                        canvas::X(0.) + self.länge.min(&(0.5 * self.size().width)),
                        start_height + multiplier * 0.5 * beschränkung::<Z>(),
                    ),
                    winkel: Winkel::new(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
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
        let start_vector = Vektor::new(start_x, start_height);
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.dy *= multiplier;
        let verschoben_vector =
            relative_vector - Vektor { dx: self.länge, dy: canvas::Y(0.).als_skalar() };
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
        let halbe_beschränkung: Skalar = 0.5 * beschränkung::<Z>();
        let radius_abstand: Skalar = self.radius;
        let kurve_anchor_direction: Vektor =
            Vektor::new(canvas::X(self.winkel.cos()), canvas::Y(multiplier * self.winkel.sin()));
        let kurve_anchor_x: canvas::X = canvas::X(0.) + radius_abstand.as_x() * self.winkel.sin();
        let kurve_anchor_y: canvas::Y = start_height
            + multiplier * (halbe_beschränkung + radius_abstand.as_y() * (1. - self.winkel.cos()));
        AnchorPoints {
            anfang: anchor::Anchor {
                position: Vektor {
                    x: canvas::X(0.),
                    y: start_height + multiplier * halbe_beschränkung,
                },
                direction: Vektor::new(canvas::X(-1.), canvas::Y(multiplier * 0.)),
            },
            innen: anchor::Anchor {
                position: Vektor { x: kurve_anchor_x, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
            außen: anchor::Anchor {
                position: Vektor { x: kurve_anchor_x + self.länge, y: kurve_anchor_y },
                direction: kurve_anchor_direction,
            },
        }
    }
}
