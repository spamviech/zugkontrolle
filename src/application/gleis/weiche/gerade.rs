//! Definition und zeichnen einer Weiche

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss,
    application::{
        gleis::{anchor, gerade, kurve},
        typen::*,
    },
    steuerung,
};

/// Definition einer Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub richtung: Orientierung,
    pub beschreibung: Option<String>,
    pub steuerung: Option<()>,
    // pub steuerung: Option<steuerung::Weiche<Richtung>>,
}
impl<Z> Weiche<Z> {
    pub const fn neu(
        länge: Länge, radius: Radius, winkel: Winkel, richtung: Orientierung
    ) -> Self {
        Weiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: None,
            steuerung: None,
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        richtung: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        Weiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: Some(beschreibung.into()),
            steuerung: None,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientierung {
    Links,
    Rechts,
}
#[anchor::impl_lookup(anchor::Anchor)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Kurve,
}
#[anchor::impl_lookup(anschluss::OutputAnschluss)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Richtung {
    Links,
    Rechts,
}

impl<Z: Zugtyp> Zeichnen for Weiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorElements;

    fn size(&self) -> Vektor {
        let Weiche { länge, radius, winkel, .. } = *self;
        let gerade_size = gerade::size::<Z>(länge);
        let kurve_size = kurve::size::<Z>(radius, winkel);
        Vektor { x: gerade_size.x.max(&kurve_size.x), y: kurve_size.y }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        let Weiche { zugtyp, länge, radius, winkel, richtung, .. } = *self;
        if richtung == Orientierung::Links {
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

    fn fülle(&self) -> Vec<Pfad> {
        let Weiche { zugtyp, länge, radius, winkel, richtung, .. } = *self;
        if richtung == Orientierung::Links {
            let transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: self.size().y })];
            vec![
                gerade::fülle(
                    zugtyp,
                    länge,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                kurve::fülle(
                    zugtyp,
                    radius,
                    winkel,
                    transformations,
                    pfad::Erbauer::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::fülle(zugtyp, länge, Vec::new(), pfad::Erbauer::with_normal_axis),
                kurve::fülle(zugtyp, radius, winkel, Vec::new(), pfad::Erbauer::with_normal_axis),
            ]
        }
    }

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let start_height: Skalar;
            let multiplier: Skalar;
            match self.richtung {
                Orientierung::Rechts => {
                    start_height = Skalar(0.);
                    multiplier = Skalar(1.);
                },
                Orientierung::Links => {
                    start_height = self.size().y;
                    multiplier = Skalar(-1.);
                },
            };
            (
                Position {
                    punkt: Vektor {
                        x: self.länge.halbiert(),
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
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            },
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
        match self.richtung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                start_height = self.size().y;
                multiplier = Skalar(-1.);
            },
        };
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        AnchorElements {
            anfang: anchor::Anchor { position: anfang, richtung: winkel::PI },
            gerade: anchor::Anchor {
                position: anfang + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            kurve: anchor::Anchor {
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
