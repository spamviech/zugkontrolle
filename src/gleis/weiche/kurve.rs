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
    pub const fn neu(länge: Länge, radius: Radius, winkel: Winkel, richtung: Richtung) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: None,
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        richtung: Richtung,
        beschreibung: impl Into<String>,
    ) -> Self {
        KurvenWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: Some(beschreibung.into()),
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
        Vektor { x: size_gerade.x + size_kurve.x, y: size_kurve.y }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: self.size().y })];
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
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: self.size().y })];
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

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let start_height: Skalar;
            let multiplier: Skalar;
            match self.richtung {
                Richtung::Rechts => {
                    start_height = Skalar(0.);
                    multiplier = Skalar(1.);
                },
                Richtung::Links => {
                    start_height = self.size().y;
                    multiplier = Skalar(-1.);
                },
            };
            (
                Position {
                    punkt: Vektor {
                        x: self.länge.min(&(self.size().y.halbiert())),
                        y: start_height + multiplier * beschränkung::<Z>().halbiert(),
                    },
                    winkel: Winkel(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.richtung {
            Richtung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Richtung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            },
        };
        let start_vector = Vektor { x: Skalar(0.), y: start_height };
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.y *= multiplier;
        let verschoben_vector = relative_vector - Vektor { x: self.länge, y: Skalar(0.) };
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, verschoben_vector)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.richtung {
            Richtung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Richtung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            },
        };
        let halbe_beschränkung: Skalar = beschränkung::<Z>().halbiert();
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        let innen = anfang
            + Vektor {
                x: self.radius * self.winkel.sin(),
                y: multiplier * self.radius * (Skalar(1.) - self.winkel.cos()),
            };
        AnchorPoints {
            anfang: anchor::Anchor { position: anfang, richtung: winkel::PI },
            innen: anchor::Anchor { position: innen, richtung: multiplier.0 * self.winkel },
            außen: anchor::Anchor {
                position: innen + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: multiplier.0 * self.winkel,
            },
        }
    }
}
