//! Definition und zeichnen einer Weiche

use std::{fmt::Debug, marker::PhantomData};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::{alias_serialisiert_unit, create_richtung};

use crate::{
    application::{
        gleis::{gerade, kurve, verbindung},
        typen::*,
    },
    lookup::impl_lookup,
    steuerung,
};

/// Definition einer Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[alias_serialisiert_unit(steuerung::Weiche<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Weiche<Z, Anschlüsse = Option<steuerung::Weiche<Richtung, RichtungAnschlüsse>>> {
    pub zugtyp: PhantomData<fn() -> Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub orientierung: Orientierung,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}
impl<Z> WeicheUnit<Z> {
    pub fn neu(länge: Länge, radius: Radius, winkel: Winkel, orientierung: Orientierung) -> Self {
        WeicheUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: None,
            steuerung: (),
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        orientierung: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        WeicheUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientierung {
    Links,
    Rechts,
}
#[create_richtung]
#[impl_lookup(verbindung::Verbindung, Points)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Kurve,
}

impl<Z: Zugtyp, Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for Weiche<Z, Anschlüsse> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let Weiche { länge, radius, winkel, .. } = *self;
        let gerade_size = gerade::size::<Z>(länge);
        let kurve_size = kurve::size::<Z>(radius, winkel);
        Vektor { x: gerade_size.x.max(&kurve_size.x), y: kurve_size.y }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        let Weiche { zugtyp, länge, radius, winkel, orientierung, .. } = *self;
        if orientierung == Orientierung::Links {
            let transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: self.size().y })];
            vec![
                gerade::zeichne(
                    zugtyp,
                    länge,
                    true,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    winkel,
                    kurve::Beschränkung::Ende,
                    transformations,
                    pfad::Erbauer::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::zeichne(zugtyp, länge, true, Vec::new(), pfad::Erbauer::with_normal_axis),
                kurve::zeichne(
                    zugtyp,
                    radius,
                    winkel,
                    kurve::Beschränkung::Ende,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
            ]
        }
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
        let Weiche { zugtyp, länge, radius, winkel, orientierung, .. } = *self;
        let (gerade_transparenz, kurve_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        if orientierung == Orientierung::Links {
            let transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: self.size().y })];
            vec![
                (
                    gerade::fülle(
                        zugtyp,
                        länge,
                        transformations.clone(),
                        pfad::Erbauer::with_invert_y,
                    ),
                    gerade_transparenz,
                ),
                (
                    kurve::fülle(
                        zugtyp,
                        radius,
                        winkel,
                        transformations,
                        pfad::Erbauer::with_invert_y,
                    ),
                    kurve_transparenz,
                ),
            ]
        } else {
            vec![
                (
                    gerade::fülle(zugtyp, länge, Vec::new(), pfad::Erbauer::with_normal_axis),
                    gerade_transparenz,
                ),
                (
                    kurve::fülle(
                        zugtyp,
                        radius,
                        winkel,
                        Vec::new(),
                        pfad::Erbauer::with_normal_axis,
                    ),
                    kurve_transparenz,
                ),
            ]
        }
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            }
        };
        (
            Position {
                punkt: Vektor {
                    x: self.länge.halbiert(),
                    y: start_height + multiplier * beschränkung::<Z>().halbiert(),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.steuerung.name(),
        )
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            }
        };
        let start = Vektor { x: Skalar(0.), y: start_height };
        // sub-checks
        let mut relative_vector = relative_position - start;
        relative_vector.y *= multiplier;
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            }
        };
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        AnchorPoints {
            anfang: verbindung::Verbindung { position: anfang, richtung: winkel::PI },
            gerade: verbindung::Verbindung {
                position: anfang + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            kurve: verbindung::Verbindung {
                position: anfang
                    + Vektor {
                        x: self.winkel.sin() * self.radius,
                        y: multiplier * self.radius * (Skalar(1.) - self.winkel.cos()),
                    },
                richtung: multiplier.0 * self.winkel,
            },
        }
    }
}
