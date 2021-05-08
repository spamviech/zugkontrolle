//! Definition und zeichnen einer Weiche

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::gleis::typen::*;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Dreiwege-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct DreiwegeWeiche<Z> {
    pub zugtyp: PhantomData<Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub beschreibung: Option<String>,
}
impl<Z> DreiwegeWeiche<Z> {
    pub const fn neu(länge: Länge, radius: Radius, winkel: Winkel) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: None,
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Links,
    Rechts,
}
impl<Z: Zugtyp> Zeichnen for DreiwegeWeiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let DreiwegeWeiche { länge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(länge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        let height_kurven = size_kurve.y.doppelt() - beschränkung::<Z>();
        Vektor { x: size_gerade.x.max(&size_kurve.x), y: size_gerade.y.max(&height_kurven) }
    }

    fn zeichne(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + Clone + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + Clone + 'static,
    ) -> Vec<Pfad> {
        // utility sizes
        let half_height = self.size().y.halbiert();
        let beschränkung = beschränkung::<Z>();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        let mut paths = Vec::new();
        let rechts_transformations = vec![Transformation::Translation(start)];
        let links_transformations =
            vec![Transformation::Translation(start + Vektor { x: Skalar(0.), y: beschränkung })];
        // Gerade
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.länge,
            true,
            rechts_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor.clone(),
        ));
        // Rechts
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            rechts_transformations,
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor.clone(),
            zu_iced_bogen.clone(),
        ));
        // Links
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            links_transformations,
            pfad::Erbauer::with_invert_y,
            zu_iced_vektor,
            zu_iced_bogen,
        ));
        // return value
        paths
    }

    fn fülle(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + Clone + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + Clone + 'static,
    ) -> Vec<Pfad> {
        // utility sizes
        let half_height = self.size().y.halbiert();
        let beschränkung = beschränkung::<Z>();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        let mut paths = Vec::new();
        let rechts_transformations = vec![Transformation::Translation(start)];
        let links_transformations =
            vec![Transformation::Translation(start + Vektor { x: Skalar(0.), y: beschränkung })];
        // Gerade
        paths.push(gerade::fülle(
            self.zugtyp,
            self.länge,
            rechts_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor.clone(),
        ));
        // Rechts
        paths.push(kurve::fülle(
            self.zugtyp,
            self.radius,
            self.winkel,
            rechts_transformations,
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor.clone(),
            zu_iced_bogen.clone(),
        ));
        // Links
        paths.push(kurve::fülle(
            self.zugtyp,
            self.radius,
            self.winkel,
            links_transformations,
            pfad::Erbauer::with_invert_y,
            zu_iced_vektor,
            zu_iced_bogen,
        ));
        // return value
        paths
    }

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let half_height = self.size().y.halbiert();
            let halbe_beschränkung = beschränkung::<Z>().halbiert();
            let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
            (
                Position {
                    punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
                    winkel: winkel::ZERO,
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        // utility sizes
        let Vektor { x: _, y: height } = self.size();
        let half_height = height.halbiert();
        let beschränkung = beschränkung::<Z>();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        // sub-checks
        let relative_vector = relative_position - start;
        let inverted_vector = Vektor { x: relative_vector.x, y: beschränkung - relative_vector.y };
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, inverted_vector)
    }

    fn anchor_points(&self) -> AnchorPoints {
        let height: Skalar = self.size().y;
        let half_height = height.halbiert();
        let länge: Skalar = self.länge;
        let radius: Skalar = self.radius;
        let anfang = Vektor { x: Skalar(0.), y: half_height };
        AnchorPoints {
            anfang: anchor::Anchor { position: anfang, richtung: winkel::PI },
            gerade: anchor::Anchor {
                position: anfang + Vektor { x: länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            links: anchor::Anchor {
                position: anfang
                    + Vektor {
                        x: radius * Skalar(self.winkel.sin()),
                        y: radius * Skalar(1. - self.winkel.cos()),
                    },
                richtung: self.winkel,
            },
            rechts: anchor::Anchor {
                position: anfang
                    + Vektor {
                        x: radius * Skalar(self.winkel.sin()),
                        y: -radius * Skalar(1. - self.winkel.cos()),
                    },
                richtung: -self.winkel,
            },
        }
    }
}
