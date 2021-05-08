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
    pub const fn new(length: Länge, radius: Radius, winkel: Winkel) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            beschreibung: None,
        }
    }

    pub fn new_with_description(
        length: Länge,
        radius: Radius,
        winkel: Winkel,
        description: impl Into<String>,
    ) -> Self {
        DreiwegeWeiche {
            zugtyp: PhantomData,
            länge: length.to_abstand(),
            radius: radius.to_abstand(),
            winkel,
            beschreibung: Some(description.into()),
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
        let height_kurven = 2. * size_kurve.height - beschränkung::<Z>();
        Vektor {
            width: size_gerade.width.max(&size_kurve.width),
            height: size_gerade.height.max(&height_kurven),
        }
    }

    fn zeichne(&self) -> Vec<Pfad> {
        // utility sizes
        let start_x: canvas::X = canvas::X(0.);
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
        let start_y: canvas::Y = half_height - 0.5 * beschränkung::<Z>();
        let mut paths = Vec::new();
        let rechts_transformations =
            vec![canvas::Transformation::Translate(Vektor::new(start_x, start_y))];
        let links_transformations = vec![canvas::Transformation::Translate(Vektor::new(
            start_x,
            start_y + beschränkung::<Z>(),
        ))];
        // Gerade
        paths.push(gerade::zeichne(
            self.zugtyp,
            self.länge,
            true,
            rechts_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
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
        // Links
        paths.push(kurve::zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            links_transformations,
            pfad::Erbauer::with_invert_y,
        ));
        // return value
        paths
    }

    fn fülle(&self) -> Vec<Pfad> {
        // utility sizes
        let start_x: canvas::X = canvas::X(0.);
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
        let start_y: canvas::Y = half_height - 0.5 * beschränkung::<Z>();
        let mut paths = Vec::new();
        let rechts_transformations =
            vec![canvas::Transformation::Translate(Vektor::new(start_x, start_y))];
        let links_transformations = vec![canvas::Transformation::Translate(Vektor::new(
            start_x,
            start_y + beschränkung::<Z>(),
        ))];
        // Gerade
        paths.push(gerade::fülle(
            self.zugtyp,
            self.länge,
            rechts_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
        ));
        // Rechts
        paths.push(kurve::fülle(
            self.zugtyp,
            self.radius,
            self.winkel,
            rechts_transformations,
            pfad::Erbauer::with_normal_axis,
        ));
        // Links
        paths.push(kurve::fülle(
            self.zugtyp,
            self.radius,
            self.winkel,
            links_transformations,
            pfad::Erbauer::with_invert_y,
        ));
        // return value
        paths
    }

    fn beschreibung(&self) -> Option<(canvas::Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            let start_x: canvas::X = canvas::X(0.);
            let half_height: canvas::Y = canvas::Y(0.) + 0.5 * self.size().height;
            let start_y: canvas::Y = half_height - 0.5 * beschränkung::<Z>();
            (
                canvas::Position {
                    point: Vektor::new(
                        start_x + 0.5 * self.länge,
                        start_y + 0.5 * beschränkung::<Z>(),
                    ),
                    winkel: Winkel::new(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        // utility sizes
        let Vektor { width: _, height } = self.size();
        let start_x: canvas::X = canvas::X(0.);
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height;
        let start_y: canvas::Y = half_height - 0.5 * beschränkung::<Z>();
        let start_vector = Vektor::new(start_x, start_y);
        // sub-checks
        let relative_vector = relative_position - start_vector;
        let inverted_vector =
            Vektor { dx: relative_vector.dx, dy: beschränkung::<Z>() - relative_vector.dy };
        gerade::innerhalb::<Z>(self.länge, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, relative_vector)
            || kurve::innerhalb::<Z>(self.radius, self.winkel, inverted_vector)
    }

    fn anchor_points(&self) -> AnchorPoints {
        let height: Skalar = self.size().height;
        let half_height: canvas::Y = canvas::Y(0.) + 0.5 * height;
        let length: Skalar = self.länge;
        let radius: Skalar = self.radius;
        let radius_x: Skalar = radius.as_x();
        let radius_y: Skalar = radius.as_y();
        let anfang_x: canvas::X = canvas::X(0.);
        AnchorPoints {
            anfang: anchor::Anchor {
                position: Vektor { x: anfang_x, y: half_height },
                direction: Vektor::new(canvas::X(-1.), canvas::Y(0.)),
            },
            gerade: anchor::Anchor {
                position: Vektor { x: anfang_x + length, y: half_height },
                direction: Vektor::new(canvas::X(1.), canvas::Y(0.)),
            },
            links: anchor::Anchor {
                position: Vektor {
                    x: anfang_x + radius_x * self.winkel.sin(),
                    y: half_height + radius_y * (1. - self.winkel.cos()),
                },
                direction: Vektor::new(canvas::X(self.winkel.cos()), canvas::Y(self.winkel.sin())),
            },
            rechts: anchor::Anchor {
                position: Vektor {
                    x: anfang_x + radius_x * self.winkel.sin(),
                    y: half_height - radius_y * (1. - self.winkel.cos()),
                },
                direction: Vektor::new(canvas::X(self.winkel.cos()), canvas::Y(-self.winkel.sin())),
            },
        }
    }
}
