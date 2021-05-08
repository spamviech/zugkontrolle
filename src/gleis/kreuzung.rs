//! Definition und zeichnen einer Kreuzung

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use super::typen::*;
use super::{anchor, gerade, kurve};

/// Definition einer Kreuzung
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Kreuzung<T> {
    pub zugtyp: PhantomData<T>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub variante: Variante,
    pub beschreibung: Option<String>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    MitKurve,
    OhneKurve,
}
impl<Z> Kreuzung<Z> {
    fn winkel(&self) -> Winkel {
        // winkel solves the formula `x = L/2 * (1 + sin(alpha)) = R * cos(alpha)`
        // https://www.wolframalpha.com/input/?i=sin%28alpha%29-C*cos%28alpha%29%3DC
        // länge=0 gives winkel=0, but is not properly defined,
        // since it violates the formula above (pi/2 required)
        // pi/2 doesn't work either, since it violates the formula
        // `y = L/2 * sin(alpha) = R * (1 - cos(alpha))`
        // only for radius=0 as well both formulas are satisfied by any winkel
        Winkel(2. * (0.5 * (self.länge / self.radius).0).atan())
    }

    pub const fn new(länge: Länge, radius: Radius, variante: Variante) -> Self {
        Kreuzung {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: None,
        }
    }

    pub fn new_with_description(
        länge: Länge,
        radius: Radius,
        variante: Variante,
        beschreibung: impl Into<String>,
    ) -> Self {
        Kreuzung {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: Some(beschreibung.into()),
        }
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

    fn size(&self) -> Vektor {
        let size_kurve = kurve::size::<Z>(self.radius, self.winkel());
        let height_beschränkung = beschränkung::<Z>();
        let height_kurven = size_kurve.y.doppelt() - height_beschränkung;
        Vektor { x: self.länge.max(&size_kurve.x), y: height_beschränkung.max(&height_kurven) }
    }

    fn zeichne(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Point + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + 'static,
    ) -> Vec<Pfad> {
        // utility sizes
        let Vektor { x: width, y: height } = self.size();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung::<Z>().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor { x: start.x, y: -start.y };
        let zentrum_invert_y = Vektor { x: zentrum.x, y: -zentrum.y };
        let winkel = self.winkel();
        let mut paths = Vec::new();
        // Transformationen
        let horizontal_transformations = vec![Transformation::Translation(start)];
        let gedreht_transformations = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformations with assumed inverted y-Axis
            Transformation::Translation(-zentrum_invert_y),
            Transformation::Translation(start_invert_y),
        ];
        // Geraden
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.länge,
            true,
            horizontal_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor,
        ));
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.länge,
            true,
            gedreht_transformations.clone(),
            pfad::Erbauer::with_invert_y,
            zu_iced_vektor,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                horizontal_transformations,
                pfad::Erbauer::with_normal_axis,
                zu_iced_vektor,
                zu_iced_bogen,
            ));
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                gedreht_transformations,
                pfad::Erbauer::with_invert_y,
                zu_iced_vektor,
                zu_iced_bogen,
            ));
        }
        // return value
        paths
    }

    fn fülle(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Point + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + 'static,
    ) -> Vec<Pfad> {
        // utility sizes
        let Vektor { x: width, y: height } = self.size();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung::<Z>().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor { x: start.x, y: -start.y };
        let zentrum_invert_y = Vektor { x: zentrum.x, y: -zentrum.y };
        let winkel = self.winkel();
        let mut paths = Vec::new();
        // Transformationen
        let horizontal_transformations = vec![Transformation::Translation(start)];
        let gedreht_transformations = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformations with assumed inverted y-Axis
            Transformation::Translation(-zentrum_invert_y),
            Transformation::Translation(start_invert_y),
        ];
        // Geraden
        paths.push(gerade::fülle(
            self.zugtyp,
            self.länge,
            horizontal_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor,
        ));
        paths.push(gerade::fülle(
            self.zugtyp,
            self.länge,
            gedreht_transformations.clone(),
            pfad::Erbauer::with_invert_y,
            zu_iced_vektor,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                winkel,
                horizontal_transformations,
                pfad::Erbauer::with_normal_axis,
                zu_iced_vektor,
                zu_iced_bogen,
            ));
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                winkel,
                gedreht_transformations,
                pfad::Erbauer::with_invert_y,
                zu_iced_vektor,
                zu_iced_bogen,
            ));
        }
        // return value
        paths
    }

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            // utility sizes
            let half_height = self.size().y.halbiert();
            let halbe_beschränkung = beschränkung::<Z>().halbiert();
            let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
            (
                Position {
                    punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
                    winkel: Winkel(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        // utility sizes
        let Vektor { x: width, y: height } = self.size();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung::<Z>().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let winkel = self.winkel();
        // sub-checks
        let horizontal_vector = relative_position - start;
        let mut gedreht_vector = (relative_position - zentrum).rotiere(-winkel);
        gedreht_vector.y = -gedreht_vector.y;
        gedreht_vector += zentrum - start;
        gerade::innerhalb::<Z>(self.länge, horizontal_vector)
            || gerade::innerhalb::<Z>(self.länge, gedreht_vector)
            || (self.variante == Variante::MitKurve
                && (kurve::innerhalb::<Z>(self.radius, winkel, horizontal_vector)
                    || kurve::innerhalb::<Z>(self.radius, winkel, gedreht_vector)))
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let Vektor { x: width, y: height } = self.size();
        let half_height = height.halbiert();
        let anfang0 = Vektor { x: Skalar(0.), y: half_height };
        let ende0 = anfang0 + Vektor { x: self.länge, y: Skalar(0.) };
        let radius_abstand: Skalar = self.radius;
        let radius_abstand_x: Skalar = radius_abstand;
        let radius_abstand_y: Skalar = radius_abstand;
        let winkel = self.winkel();
        let anfang1 =
            self.radius * Vektor { x: Skalar(winkel.sin()), y: Skalar(1. - winkel.cos()) };
        let ende1 = Vektor {
            x: width - self.radius * Skalar(winkel.sin()),
            y: half_height - radius_abstand_y * Skalar(1. - winkel.cos()),
        };
        AnchorPoints {
            anfang_0: anchor::Anchor { position: anfang0, richtung: winkel::PI },
            ende_0: anchor::Anchor { position: ende0, richtung: winkel::ZERO },
            anfang_1: anchor::Anchor { position: anfang1, richtung: winkel },
            ende_1: anchor::Anchor { position: ende1, richtung: -winkel },
        }
    }
}
