//! Definition und zeichnen einer Kurve

use std::f32::consts::PI;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use super::anchor;
use super::typen::*;

/// Definition einer Kurve
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Kurve<Z> {
    pub zugtyp: PhantomData<Z>,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub beschreibung: Option<String>,
}
impl<Z> Kurve<Z> {
    pub const fn neu(radius: Radius, winkel: Winkel) -> Self {
        Kurve { zugtyp: PhantomData, radius: radius.als_skalar(), winkel, beschreibung: None }
    }

    pub fn neu_mit_beschreibung(
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        Kurve {
            zugtyp: PhantomData,
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Kurve<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        size::<Z>(self.radius, self.winkel)
    }

    fn zeichne(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + 'static,
    ) -> Vec<Pfad> {
        vec![zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            Beschränkung::Alle,
            Vec::new(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor,
            zu_iced_bogen,
        )]
    }

    fn fülle(
        &self,
        zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + 'static,
        zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + 'static,
    ) -> Vec<Pfad> {
        vec![fülle(
            self.zugtyp,
            self.radius,
            self.winkel,
            Vec::new(),
            pfad::Erbauer::with_normal_axis,
            zu_iced_vektor,
            zu_iced_bogen,
        )]
    }

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let half_angle = 0.5 * self.winkel;
            (
                Position {
                    punkt: Vektor {
                        x: self.radius * Skalar(half_angle.sin()),
                        y: beschränkung::<Z>().halbiert()
                            + self.radius * Skalar(1. - half_angle.cos()),
                    },
                    winkel: Winkel(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        innerhalb::<Z>(self.radius, self.winkel, relative_position)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
        AnchorPoints {
            anfang: anchor::Anchor {
                position: Vektor { x: Skalar(0.), y: halbe_beschränkung },
                richtung: winkel::PI,
            },
            ende: anchor::Anchor {
                position: Vektor {
                    x: self.radius * Skalar(self.winkel.sin()),
                    y: halbe_beschränkung + self.radius * Skalar(1. - self.winkel.cos()),
                },
                richtung: self.winkel,
            },
        }
    }
}

pub(crate) fn size<Z: Zugtyp>(radius: Skalar, winkel: Winkel) -> Vektor {
    // Breite
    let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(radius);
    let width_factor = Skalar(if winkel.abs() < winkel::FRAC_PI_2 { winkel.sin() } else { 1. });
    let width = radius_begrenzung_außen * width_factor;
    // Höhe des Bogen
    let angle_abs = winkel.abs();
    let comparison = if angle_abs < winkel::FRAC_PI_2 {
        radius_begrenzung_außen * Skalar(1. - winkel.cos())
            + beschränkung::<Z>() * Skalar(winkel.cos())
    } else if angle_abs < winkel::PI {
        radius_begrenzung_außen * Skalar(1. - winkel.cos())
    } else {
        radius_begrenzung_außen
    };
    // Mindesthöhe: Beschränkung einer Geraden
    let height = beschränkung::<Z>().max(&comparison);
    // Rückgabewert
    Vektor { x: width, y: height }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Beschränkung {
    Keine,
    Ende,
    Alle,
}
impl Beschränkung {
    fn anfangs_beschränkung(&self) -> bool {
        match self {
            Beschränkung::Alle => true,
            Beschränkung::Keine | Beschränkung::Ende => false,
        }
    }

    fn end_beschränkung(&self) -> bool {
        match self {
            Beschränkung::Ende | Beschränkung::Alle => true,
            Beschränkung::Keine => false,
        }
    }
}

pub(crate) fn zeichne<Z, P, A, F, G>(
    _zugtyp: PhantomData<Z>,
    radius: Skalar,
    winkel: Winkel,
    beschränkungen: Beschränkung,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn for<'s> FnOnce(&'s mut pfad::Erbauer<P, A>)>,
    ),
    zu_iced_vektor: F,
    zu_iced_bogen: G,
) -> Pfad
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    F: 'static + Fn(Vektor) -> iced::Vector,
    G: 'static + Fn(Bogen) -> iced::canvas::path::Arc,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| {
            zeichne_internal::<Z, P, A, F, G>(
                builder,
                radius,
                winkel,
                beschränkungen,
                zu_iced_vektor,
                zu_iced_bogen,
            )
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

// factor_y is expected to be -1 or +1, although other values should work as well
fn zeichne_internal<Z, P, A, F, G>(
    path_builder: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
    beschränkungen: Beschränkung,
    zu_iced_vektor: F,
    zu_iced_bogen: G,
) where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    F: Fn(Vektor) -> iced::Vector,
    G: Fn(Bogen) -> iced::canvas::path::Arc,
{
    // Utility Größen
    let spurweite: Skalar = spurweite::<Z>();
    let beschränkung: Skalar = beschränkung::<Z>();
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: beschränkung };
    let radius_begrenzung_außen: Skalar = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand::<Z>();
    let radius_innen = radius_außen - spurweite;
    let begrenzung0 = gleis_links_oben
        + radius_begrenzung_außen
            * Vektor { x: Skalar(winkel.sin()), y: Skalar(1. - winkel.cos()) };
    let begrenzung1 =
        begrenzung0 + beschränkung * Vektor { x: Skalar(-winkel.sin()), y: Skalar(winkel.cos()) };
    let bogen_zentrum = gleis_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Beschränkungen
    if beschränkungen.anfangs_beschränkung() {
        path_builder.move_to(gleis_links_oben.into(), &zu_iced_vektor);
        path_builder.line_to(gleis_links_unten.into(), &zu_iced_vektor);
    }
    if beschränkungen.end_beschränkung() {
        path_builder.move_to(begrenzung0.into(), &zu_iced_vektor);
        path_builder.line_to(begrenzung1.into(), zu_iced_vektor);
    }
    // Gleis
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_außen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
        &zu_iced_bogen,
    );
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_innen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
        zu_iced_bogen,
    );
}

pub(crate) fn fülle<Z, P, A, F, G>(
    _zugtyp: PhantomData<Z>,
    radius: Skalar,
    winkel: Winkel,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn for<'s> FnOnce(&'s mut pfad::Erbauer<P, A>)>,
    ),
    zu_iced_vektor: F,
    zu_iced_bogen: G,
) -> Pfad
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    F: 'static + Fn(Vektor) -> iced::Vector,
    G: 'static + Fn(Bogen) -> iced::canvas::path::Arc,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| {
            fülle_internal::<Z, P, A, F, G>(builder, radius, winkel, zu_iced_vektor, zu_iced_bogen)
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

/// Geplant für canvas::PathType::EvenOdd
fn fülle_internal<Z, P, A, F, G>(
    path_builder: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
    zu_iced_vektor: F,
    zu_iced_bogen: G,
) where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    F: Fn(Vektor) -> iced::Vector,
    G: Fn(Bogen) -> iced::canvas::path::Arc,
{
    let spurweite = spurweite::<Z>();
    let abstand = abstand::<Z>();
    let beschränkung_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    // Koordinaten für den Bogen
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let radius_begrenzung_außen: Skalar = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite;
    let bogen_zentrum =
        beschränkung_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Koordinaten links
    let gleis_links_oben = beschränkung_links_oben + Vektor { x: Skalar(0.), y: abstand };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: spurweite };
    // Koordinaten rechts
    let gleis_rechts_oben: Vektor = gleis_links_oben
        + radius_außen * Vektor { x: Skalar(winkel.sin()), y: Skalar(1. - winkel.cos()) };
    let gleis_rechts_unten: Vektor = gleis_rechts_oben
        + Vektor { x: -spurweite * Skalar(winkel.sin()), y: spurweite * Skalar(winkel.cos()) };
    // obere Kurve
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_außen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
        &zu_iced_bogen,
    );
    path_builder.close();
    // untere Kurve
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_innen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
        zu_iced_bogen,
    );
    path_builder.close();
    // Zwischen-Teil
    path_builder.move_to(gleis_links_oben.into(), &zu_iced_vektor);
    path_builder.line_to(gleis_rechts_oben.into(), &zu_iced_vektor);
    path_builder.line_to(gleis_rechts_unten.into(), &zu_iced_vektor);
    path_builder.line_to(gleis_links_unten.into(), zu_iced_vektor);
    path_builder.close();
}

pub(crate) fn innerhalb<Z: Zugtyp>(
    radius: Skalar,
    winkel: Winkel,
    relative_position: Vektor,
) -> bool {
    let spurweite = spurweite::<Z>();
    let abstand = abstand::<Z>();
    let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite;
    let bogen_zentrum = Vektor { x: Skalar(0.), y: abstand + radius_außen };
    let radius_vector = bogen_zentrum - relative_position;
    let länge = radius_vector.länge();
    if länge > radius_innen && länge < radius_außen {
        let mut test_winkel: Winkel = if radius_vector.x > Skalar(0.) {
            -Winkel::acos((radius_vector.y / länge).0)
        } else {
            Winkel::acos((radius_vector.y / länge).0)
        };
        // normalisiere winkel
        while test_winkel < Winkel(0.) {
            test_winkel += Winkel(2. * std::f32::consts::PI)
        }
        if test_winkel < winkel {
            return true
        }
    }
    false
}
