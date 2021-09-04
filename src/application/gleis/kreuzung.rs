//! Definition und zeichnen einer Kreuzung

use std::{fmt::Debug, marker::PhantomData};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::alias_serialisiert_unit;

pub use crate::application::gleis::weiche::gerade::{
    Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert,
};
use crate::{
    application::{
        gleis::{gerade, kurve, verbindung},
        typen::*,
    },
    lookup::impl_lookup,
    steuerung,
};

/// Definition einer Kreuzung
#[alias_serialisiert_unit(steuerung::BenannteWeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Kreuzung<Z, Anschlüsse = Option<steuerung::BenannteWeiche<Richtung, RichtungAnschlüsse>>>
{
    pub zugtyp: PhantomData<fn() -> Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub variante: Variante,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    MitKurve,
    OhneKurve,
}
impl<Z> KreuzungUnit<Z> {
    pub fn neu(länge: Länge, radius: Radius, variante: Variante) -> Self {
        KreuzungUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: None,
            steuerung: (),
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        variante: Variante,
        beschreibung: impl Into<String>,
    ) -> Self {
        KreuzungUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

impl<Z, Anschlüsse> Kreuzung<Z, Anschlüsse> {
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
}

#[impl_lookup(verbindung::Verbindung, Points)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AnchorName {
    Anfang0,
    Ende0,
    Anfang1,
    Ende1,
}

impl<Z: Zugtyp, Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for Kreuzung<Z, Anschlüsse> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let size_kurve = kurve::size::<Z>(self.radius, self.winkel());
        let height_beschränkung = beschränkung::<Z>();
        let height_kurven = size_kurve.y.doppelt() - height_beschränkung;
        Vektor { x: self.länge.max(&size_kurve.x), y: height_beschränkung.max(&height_kurven) }
    }

    fn zeichne(&self) -> Vec<Pfad> {
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
        ));
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.länge,
            true,
            gedreht_transformations.clone(),
            pfad::Erbauer::with_invert_y,
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
            ));
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                gedreht_transformations,
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
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
        let (gerade_transparenz, kurve_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        // Geraden
        paths.push((
            gerade::fülle(
                self.zugtyp,
                self.länge,
                horizontal_transformations.clone(),
                pfad::Erbauer::with_normal_axis,
            ),
            gerade_transparenz,
        ));
        paths.push((
            gerade::fülle(
                self.zugtyp,
                self.länge,
                gedreht_transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ),
            gerade_transparenz,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius,
                    winkel,
                    horizontal_transformations,
                    pfad::Erbauer::with_normal_axis,
                ),
                kurve_transparenz,
            ));
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius,
                    winkel,
                    gedreht_transformations,
                    pfad::Erbauer::with_invert_y,
                ),
                kurve_transparenz,
            ));
        }
        // return value
        paths
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        // utility sizes
        let half_height = self.size().y.halbiert();
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
        (
            Position {
                punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.steuerung.name(),
        )
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
        let mut gedreht_vector = (relative_position - zentrum).rotiert(-winkel);
        gedreht_vector.y = -gedreht_vector.y;
        gedreht_vector += zentrum - start;
        gerade::innerhalb::<Z>(self.länge, horizontal_vector)
            || gerade::innerhalb::<Z>(self.länge, gedreht_vector)
            || (self.variante == Variante::MitKurve
                && (kurve::innerhalb::<Z>(self.radius, winkel, horizontal_vector)
                    || kurve::innerhalb::<Z>(self.radius, winkel, gedreht_vector)))
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let Vektor { x: _, y: height } = self.size();
        let half_height = height.halbiert();
        let anfang0 = Vektor { x: Skalar(0.), y: half_height };
        let ende0 = anfang0 + Vektor { x: self.länge, y: Skalar(0.) };
        let winkel = self.winkel();
        let kurve = self.radius * Vektor { x: winkel.sin(), y: Skalar(1.) - winkel.cos() };
        let anfang1 = ende0 - kurve;
        let ende1 = anfang0 + kurve;
        AnchorPoints {
            anfang_0: verbindung::Verbindung { position: anfang0, richtung: winkel::PI },
            ende_0: verbindung::Verbindung { position: ende0, richtung: winkel::ZERO },
            anfang_1: verbindung::Verbindung { position: anfang1, richtung: winkel::PI + winkel },
            ende_1: verbindung::Verbindung { position: ende1, richtung: winkel },
        }
    }
}
