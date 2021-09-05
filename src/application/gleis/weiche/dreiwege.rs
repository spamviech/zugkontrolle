//! Definition und zeichnen einer Weiche

use std::{fmt::Debug, marker::PhantomData};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::{alias_serialisiert_unit, create_richtung};

use crate::{
    application::gleis::{gerade, kurve, verbindung},
    steuerung,
    {application::typen::*, lookup::impl_lookup},
};

/// Definition einer Dreiwege-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[alias_serialisiert_unit(steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct DreiwegeWeiche
<Z, Anschlüsse = Option<steuerung::Weiche<Richtung, RichtungAnschlüsse>>> {
    pub zugtyp: PhantomData<fn() -> Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}
impl<Z> DreiwegeWeicheUnit<Z> {
    pub fn neu(länge: Länge, radius: Radius, winkel: Winkel) -> Self {
        DreiwegeWeicheUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: None,
            steuerung: (),
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        DreiwegeWeicheUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

#[create_richtung]
#[impl_lookup(verbindung::Verbindung, Points)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Links,
    Rechts,
}

impl<Z: Zugtyp, Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen
    for DreiwegeWeiche<Z, Anschlüsse>
{
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let DreiwegeWeiche { länge, radius, winkel, .. } = *self;
        let size_gerade = gerade::size::<Z>(länge);
        let size_kurve = kurve::size::<Z>(radius, winkel);
        let height_kurven = size_kurve.y.doppelt() - beschränkung::<Z>();
        Vektor { x: size_gerade.x.max(&size_kurve.x), y: size_gerade.y.max(&height_kurven) }
    }

    fn zeichne(&self) -> Vec<Pfad> {
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
        ));
        // Links
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            links_transformations,
            pfad::Erbauer::with_invert_y,
        ));
        // Rechts
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            rechts_transformations,
            pfad::Erbauer::with_normal_axis,
        ));
        // return value
        paths
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
        // utility sizes
        let half_height = self.size().y.halbiert();
        let beschränkung = beschränkung::<Z>();
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        let mut paths = Vec::new();
        let rechts_transformations = vec![Transformation::Translation(start)];
        let links_transformations =
            vec![Transformation::Translation(start + Vektor { x: Skalar(0.), y: beschränkung })];
        let (gerade_transparenz, links_transparenz, rechts_transparenz) =
            match self.steuerung.aktuelle_richtung() {
                None => (Transparenz::Voll, Transparenz::Voll, Transparenz::Voll),
                Some(Richtung::Gerade) => {
                    (Transparenz::Voll, Transparenz::Reduziert, Transparenz::Reduziert)
                }
                Some(Richtung::Links) => {
                    (Transparenz::Reduziert, Transparenz::Voll, Transparenz::Reduziert)
                }
                Some(Richtung::Rechts) => {
                    (Transparenz::Reduziert, Transparenz::Reduziert, Transparenz::Voll)
                }
            };
        // Gerade
        paths.push((
            gerade::fülle(
                self.zugtyp,
                self.länge,
                rechts_transformations.clone(),
                pfad::Erbauer::with_normal_axis,
            ),
            gerade_transparenz,
        ));
        // Links
        paths.push((
            kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                links_transformations,
                pfad::Erbauer::with_invert_y,
            ),
            links_transparenz,
        ));
        // Rechts
        paths.push((
            kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                rechts_transformations,
                pfad::Erbauer::with_normal_axis,
            ),
            rechts_transparenz,
        ));
        // return value
        paths
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        let half_height = self.size().y.halbiert();
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
        (
            Position {
                punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
                winkel: winkel::ZERO,
            },
            self.beschreibung.as_ref(),
            self.steuerung.name(),
        )
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

    fn anchor_points(&self) -> Self::AnchorPoints {
        let height: Skalar = self.size().y;
        let half_height = height.halbiert();
        let länge: Skalar = self.länge;
        let radius: Skalar = self.radius;
        let anfang = Vektor { x: Skalar(0.), y: half_height };
        AnchorPoints {
            anfang: verbindung::Verbindung { position: anfang, richtung: winkel::PI },
            gerade: verbindung::Verbindung {
                position: anfang + Vektor { x: länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            links: verbindung::Verbindung {
                position: anfang
                    + Vektor {
                        x: radius * self.winkel.sin(),
                        y: radius * (Skalar(1.) - self.winkel.cos()),
                    },
                richtung: self.winkel,
            },
            rechts: verbindung::Verbindung {
                position: anfang
                    + Vektor {
                        x: radius * self.winkel.sin(),
                        y: -radius * (Skalar(1.) - self.winkel.cos()),
                    },
                richtung: -self.winkel,
            },
        }
    }
}
