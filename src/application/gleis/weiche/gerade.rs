//! Definition und zeichnen einer Weiche

use std::fmt::Debug;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{self, Anschlüsse},
    application::{
        gleis::{anchor, gerade, kurve},
        typen::*,
    },
    lookup::impl_lookup,
    steuerung,
};

pub type Weiche<Z> = WeicheData<Z, Option<steuerung::Weiche<RichtungAnschlüsse>>>;
pub type WeicheSave<Z> = WeicheData<Z, Option<steuerung::Weiche<RichtungAnschlüsseSave>>>;
pub type WeicheUnit<Z> = WeicheData<Z, ()>;
impl<Z> Weiche<Z> {
    pub fn to_save(&self) -> WeicheSave<Z> {
        let Weiche { zugtyp, länge, radius, winkel, orientierung, beschreibung, steuerung } = self;
        WeicheSave {
            zugtyp: *zugtyp,
            länge: *länge,
            radius: *radius,
            winkel: *winkel,
            orientierung: *orientierung,
            beschreibung: beschreibung.clone(),
            steuerung: steuerung.as_ref().map(|steuerung::Weiche { anschlüsse }| {
                steuerung::Weiche { anschlüsse: anschlüsse.to_save() }
            }),
        }
    }

    pub fn to_unit(&self) -> WeicheUnit<Z> {
        let Weiche { zugtyp, länge, radius, winkel, orientierung, beschreibung, steuerung: _ } =
            self;
        WeicheUnit {
            zugtyp: *zugtyp,
            länge: *länge,
            radius: *radius,
            winkel: *winkel,
            orientierung: *orientierung,
            beschreibung: beschreibung.clone(),
            steuerung: (),
        }
    }
}

/// Definition einer Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct WeicheData<Z, Anschlüsse> {
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
#[impl_lookup(anchor::Anchor, Points)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Kurve,
}
#[impl_lookup(anschluss::OutputAnschluss, Anschlüsse, Debug)]
#[impl_lookup(anschluss::OutputSave, AnschlüsseSave, Debug, Clone, Serialize, Deserialize)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Richtung {
    Gerade,
    Kurve,
}
impl RichtungAnschlüsse {
    pub fn to_save(&self) -> RichtungAnschlüsseSave {
        let RichtungAnschlüsse { gerade, kurve } = self;
        RichtungAnschlüsseSave { gerade: gerade.to_save(), kurve: kurve.to_save() }
    }
}
impl RichtungAnschlüsseSave {
    pub fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
    ) -> Result<RichtungAnschlüsse, anschluss::Error> {
        let RichtungAnschlüsseSave { gerade, kurve } = self;
        Ok(RichtungAnschlüsse {
            gerade: gerade.reserviere(anschlüsse)?,
            kurve: kurve.reserviere(anschlüsse)?,
        })
    }
}

impl<Z: Zugtyp, A> Zeichnen for WeicheData<Z, A> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let WeicheData { länge, radius, winkel, .. } = *self;
        let gerade_size = gerade::size::<Z>(länge);
        let kurve_size = kurve::size::<Z>(radius, winkel);
        Vektor { x: gerade_size.x.max(&kurve_size.x), y: kurve_size.y }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        let WeicheData { zugtyp, länge, radius, winkel, orientierung, .. } = *self;
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

    fn fülle(&self) -> Vec<Pfad> {
        let WeicheData { zugtyp, länge, radius, winkel, orientierung, .. } = *self;
        if orientierung == Orientierung::Links {
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
            match self.orientierung {
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
        match self.orientierung {
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
        match self.orientierung {
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
        AnchorPoints {
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
