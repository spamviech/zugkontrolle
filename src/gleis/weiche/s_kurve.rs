//! Definition und zeichnen einer Weiche

use std::f32::consts::PI;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use super::Richtung;
use crate::gleis::typen::*;
use crate::gleis::weiche;
use crate::gleis::{anchor, gerade, kurve};

/// Definition einer Weiche mit S-Kurve
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct SKurvenWeiche<Z> {
    pub zugtyp: PhantomData<Z>,
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub radius_reverse: Skalar,
    pub winkel_reverse: Winkel,
    pub richtung: Richtung,
    pub beschreibung: Option<String>,
}
impl<Z> SKurvenWeiche<Z> {
    pub const fn new(
        length: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_reverse: Radius,
        angle_reverse: Winkel,
        direction: Richtung,
    ) -> Self {
        SKurvenWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            radius_reverse: radius_reverse.to_abstand(),
            winkel_reverse: angle_reverse,
            richtung: direction,
            beschreibung: None,
        }
    }

    pub fn new_with_description(
        length: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_reverse: Radius,
        angle_reverse: Winkel,
        direction: Richtung,
        description: impl Into<String>,
    ) -> Self {
        SKurvenWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            radius_reverse: radius_reverse.to_abstand(),
            winkel_reverse: angle_reverse,
            richtung: direction,
            beschreibung: Some(description.into()),
        }
    }
}

impl<Z: Zugtyp> Zeichnen for SKurvenWeiche<Z> {
    type AnchorName = weiche::gerade::AnchorName;
    type AnchorPoints = weiche::gerade::AnchorPoints;

    fn size(&self) -> Vektor {
        let SKurvenWeiche { länge, radius, winkel, radius_reverse, winkel_reverse, .. } = *self;
        let angle_difference = winkel - winkel_reverse;
        let size_gerade = gerade::size::<Z>(länge);

        //Breiten-Berechnung
        let factor_width = if winkel.abs() < Winkel::new(0.5 * PI) { winkel.sin() } else { 1. };
        let factor_width_reverse = if angle_difference.abs() < Winkel::new(0.5 * PI) {
            winkel.sin() - angle_difference.sin()
        } else {
            1.
        }
        .max(0.);
        let radius_außen = radius_begrenzung_außen::<Z>(radius);
        let radius_außen_x: Skalar = radius_außen.as_x();
        let radius_innen = radius_begrenzung_innen::<Z>(radius);
        let radius_innen_x: Skalar = radius_innen.as_x();
        let radius_reverse_außen = radius_begrenzung_außen::<Z>(radius_reverse);
        let radius_reverse_innen = radius_begrenzung_innen::<Z>(radius_reverse);
        // obere Beschränkung
        let width_oben1: Skalar = radius_außen_x * factor_width;
        let width_oben2: Skalar =
            radius_außen_x * winkel.sin() + radius_reverse_innen.as_x() * factor_width_reverse;
        let width_oben: Skalar = width_oben1.max(&width_oben2);
        // untere Beschränkung
        let width_unten1: Skalar = radius_innen_x * factor_width;
        let width_unten2: Skalar =
            radius_innen_x * winkel.sin() + radius_reverse_außen.as_x() * factor_width_reverse;
        let width_unten: Skalar = width_unten1.max(&width_unten2);

        // Höhen-Berechnung
        let factor_height = if winkel.abs() < Winkel::new(PI) { 1. - winkel.cos() } else { 1. };
        let factor_height_reverse = if angle_difference.abs() < Winkel::new(PI) {
            angle_difference.cos() - winkel.cos()
        } else {
            1.
        }
        .max(0.);
        let radius_außen: Skalar = radius_begrenzung_außen::<Z>(radius).as_y();
        let radius_reverse_innen: Skalar = radius_begrenzung_innen::<Z>(radius_reverse).as_y();
        // obere Beschränkung
        let height_oben1: Skalar = radius_außen * factor_height;
        let height_oben2: Skalar =
            radius_außen * (1. - winkel.cos()) + radius_reverse_innen * factor_height_reverse;
        let height_oben: Skalar = height_oben1.max(&height_oben2);
        // untere Beschränkung
        let gleis_unten_start = beschränkung::<Z>();
        let radius_innen: Skalar = radius_begrenzung_innen::<Z>(radius).as_y();
        let radius_reverse_außen: Skalar = radius_begrenzung_außen::<Z>(radius_reverse).as_y();
        let height_unten1 = gleis_unten_start + radius_innen * factor_height;
        let height_unten2 = gleis_unten_start
            + radius_innen * (1. - winkel.cos())
            + radius_reverse_außen * factor_height_reverse;
        let height_unten = height_unten1.max(&height_unten2);

        Vektor {
            width: size_gerade.width.max(&width_oben.max(&width_unten)),
            height: height_oben.max(&height_unten),
        }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        // utility sizes
        let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(self.radius);
        let s_kurve_transformations = |multiplier: f32| {
            let winkel = multiplier * self.winkel;
            vec![
                canvas::Transformation::Translate(Vektor {
                    dx: multiplier * radius_begrenzung_außen.as_x() * winkel.sin(),
                    dy: multiplier * radius_begrenzung_außen.as_y() * (1. - winkel.cos()),
                }),
                canvas::Transformation::Rotate(winkel),
                canvas::Transformation::Translate(Vektor {
                    dx: canvas::X(0.).to_abstand(),
                    dy: multiplier * beschränkung::<Z>(),
                }),
            ]
        };
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(Vektor {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                true,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Keine,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations(-1.));
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschränkung::Ende,
                transformations,
                pfad::Erbauer::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                true,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Keine,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschränkung::Ende,
                s_kurve_transformations(1.),
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self) -> Vec<Pfad> {
        // utility sizes
        let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(self.radius);
        let s_kurve_transformations = |multiplier: f32| {
            let winkel = multiplier * self.winkel;
            vec![
                canvas::Transformation::Translate(Vektor {
                    dx: multiplier * radius_begrenzung_außen.as_x() * winkel.sin(),
                    dy: multiplier * radius_begrenzung_außen.as_y() * (1. - winkel.cos()),
                }),
                canvas::Transformation::Rotate(winkel),
                canvas::Transformation::Translate(Vektor {
                    dx: canvas::X(0.).to_abstand(),
                    dy: multiplier * beschränkung::<Z>(),
                }),
            ]
        };
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.richtung == Richtung::Links {
            let mut transformations = vec![canvas::Transformation::Translate(Vektor {
                dx: canvas::X(0.).to_abstand(),
                dy: self.size().height,
            })];
            // Gerade
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations(-1.));
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                transformations,
                pfad::Erbauer::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::fülle(
                self.zugtyp,
                self.länge,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::fülle(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                s_kurve_transformations(1.),
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        paths
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
        let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(self.radius);
        let multiplied_winkel = multiplier * self.winkel;
        let s_kurve_start_vector = Vektor {
            dx: multiplier * radius_begrenzung_außen.as_x() * multiplied_winkel.sin(),
            dy: radius_begrenzung_außen.as_y() * (1. - multiplied_winkel.cos()),
        };
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.dy *= multiplier;
        let mut s_kurve_vector = (relative_vector - s_kurve_start_vector).rotate(-self.winkel);
        s_kurve_vector -= Vektor { dx: canvas::X(0.).to_abstand(), dy: beschränkung::<Z>() };
        s_kurve_vector.dy *= -1.;
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
            || kurve::innerhalb::<Z>(self.radius_reverse, self.winkel_reverse, s_kurve_vector)
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
        let angle_difference = self.winkel - self.winkel_reverse;
        weiche::gerade::AnchorPoints {
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
                    x: canvas::X(0.)
                        + self.radius.as_x() * self.winkel.sin()
                        + self.radius_reverse.as_x() * (self.winkel.sin() - angle_difference.sin()),
                    y: start_height
                        + multiplier
                            * (0.5 * beschränkung::<Z>()
                                + self.radius.as_y() * (1. - self.winkel.cos())
                                + self.radius_reverse.as_y()
                                    * (angle_difference.cos() - self.winkel.cos())),
                },
                direction: Vektor::new(
                    canvas::X(angle_difference.cos()),
                    canvas::Y(multiplier * angle_difference.sin()),
                ),
            },
        }
    }
}
