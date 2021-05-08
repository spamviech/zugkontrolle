//! Definition und zeichnen einer Weiche

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::gleis::typen::*;
use crate::gleis::{anchor, gerade, kurve};

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
    pub richtung: Richtung,
    pub beschreibung: Option<String>,
}
impl<Z> Weiche<Z> {
    pub const fn new(länge: Länge, radius: Radius, winkel: Winkel, richtung: Richtung) -> Self {
        Weiche {
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
        Weiche {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            richtung,
            beschreibung: Some(description.into()),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Richtung {
    Links,
    Rechts,
}
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Gerade,
    Kurve,
}

impl<Z: Zugtyp> Zeichnen for Weiche<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        let Weiche { länge, radius, winkel, .. } = *self;
        let gerade_size = gerade::size::<Z>(länge);
        let kurve_size = kurve::size::<Z>(radius, winkel);
        Vektor { width: gerade_size.width.max(&kurve_size.width), height: kurve_size.height }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        let Weiche { zugtyp, länge, radius, winkel, richtung, .. } = *self;
        if richtung == Richtung::Links {
            let transformations = vec![Transformation::Translation(Vektor {
                dx: canvas::X(0.).als_skalar(),
                dy: self.size().height,
            })];
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
        if richtung == Richtung::Links {
            let transformations = vec![Transformation::Translation(Vektor {
                dx: canvas::X(0.).als_skalar(),
                dy: self.size().height,
            })];
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
                        canvas::X(0.) + 0.5 * self.länge,
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
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
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
        AnchorPoints {
            anfang: anchor::Anchor {
                position: Vektor {
                    x: canvas::X(0.),
                    y: start_height + multiplier * 0.5 * beschränkung::<Z>(),
                },
                direction: Vektor::new(canvas::X(-1.), canvas::Y(multiplier * 0.)),
            },
            gerade: anchor::Anchor {
                position: Vektor {
                    x: canvas::X(0.) + self.länge,
                    y: start_height + multiplier * 0.5 * beschränkung::<Z>(),
                },
                direction: Vektor::new(canvas::X(1.), canvas::Y(multiplier * 0.)),
            },
            kurve: anchor::Anchor {
                position: Vektor {
                    x: canvas::X(0.) + self.winkel.sin() * self.radius.as_x(),
                    y: start_height
                        + multiplier
                            * (0.5 * beschränkung::<Z>()
                                + self.radius.as_y() * (1. - self.winkel.cos())),
                },
                direction: Vektor::new(
                    canvas::X(self.winkel.cos()),
                    canvas::Y(multiplier * self.winkel.sin()),
                ),
            },
        }
    }
}
