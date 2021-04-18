//! Definition und zeichnen einer Kurve

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::f32::consts::PI;
use std::marker::PhantomData;

use super::anchor;
use super::types::*;

/// Definition einer Kurve
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct Kurve<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub radius: canvas::Abstand<canvas::Radius>,
    pub winkel: Angle,
    pub beschreibung: Option<&'static str>,
}
impl<Z> Kurve<Z> {
    pub const fn new(radius: Radius, angle: Angle) -> Self {
        Kurve {
            zugtyp: PhantomData,
            radius: radius.to_abstand(),
            winkel: angle,
            beschreibung: None,
        }
    }
    pub const fn new_with_description(
        radius: Radius,
        angle: Angle,
        description: &'static str,
    ) -> Self {
        Kurve {
            zugtyp: PhantomData,
            radius: radius.to_abstand(),
            winkel: angle,
            beschreibung: Some(description),
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

    fn size(&self) -> canvas::Size {
        size::<Z>(self.radius, self.winkel)
    }

    fn zeichne(&self) -> Vec<canvas::Path> {
        vec![zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            Beschraenkung::Alle,
            Vec::new(),
            canvas::PathBuilder::with_normal_axis,
        )]
    }

    fn fuelle(&self) -> Vec<canvas::Path> {
        vec![fuelle(
            self.zugtyp,
            self.radius,
            self.winkel,
            Vec::new(),
            canvas::PathBuilder::with_normal_axis,
        )]
    }

    fn beschreibung(&self) -> Option<(canvas::Position, &'static str)> {
        self.beschreibung.map(|text| {
            let half_angle = 0.5 * self.winkel;
            (
                canvas::Position {
                    point: canvas::Point::new(
                        canvas::X(0.) + self.radius.as_x() * half_angle.sin(),
                        canvas::Y(0.)
                            + 0.5 * beschraenkung::<Z>()
                            + self.radius.as_y() * (1. - half_angle.cos()),
                    ),
                    winkel: Angle::new(0.),
                },
                text,
            )
        })
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        AnchorPoints {
            anfang: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.),
                    y: canvas::Y(0.) + 0.5 * beschraenkung::<Z>(),
                },
                direction: canvas::Vector::new(canvas::X(-1.), canvas::Y(0.)),
            },
            ende: anchor::Anchor {
                position: canvas::Point {
                    x: canvas::X(0.) + self.radius.as_x() * self.winkel.sin(),
                    y: canvas::Y(0.)
                        + 0.5 * beschraenkung::<Z>()
                        + self.radius.as_y() * (1. - self.winkel.cos()),
                },
                direction: canvas::Vector::new(
                    canvas::X(self.winkel.cos()),
                    canvas::Y(self.winkel.sin()),
                ),
            },
        }
    }
}

pub(crate) fn size<Z: Zugtyp>(
    radius: canvas::Abstand<canvas::Radius>,
    winkel: Angle,
) -> canvas::Size {
    // Breite
    let radius_begrenzung_aussen = radius_begrenzung_aussen::<Z>(radius);
    let radius_begrenzung_aussen_y = radius_begrenzung_aussen.as_y();
    let width_factor = if winkel.abs() < Angle::new(0.5 * PI) { winkel.sin() } else { 1. };
    let width = radius_begrenzung_aussen.as_x() * width_factor;
    // Höhe des Bogen
    let angle_abs = winkel.abs();
    let comparison = if angle_abs < Angle::new(0.5 * PI) {
        radius_begrenzung_aussen_y * (1. - winkel.cos()) + beschraenkung::<Z>() * winkel.cos()
    } else if angle_abs < Angle::new(PI) {
        radius_begrenzung_aussen_y * (1. - winkel.cos())
    } else {
        radius_begrenzung_aussen_y
    };
    // Mindesthöhe: Beschränkung einer Geraden
    let height = beschraenkung::<Z>().max(&comparison);
    // Rückgabewert
    canvas::Size { width, height }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Beschraenkung {
    Keine,
    Ende,
    Alle,
}
impl Beschraenkung {
    fn anfangs_beschraenkung(&self) -> bool {
        match self {
            Beschraenkung::Alle => true,
            Beschraenkung::Keine | Beschraenkung::Ende => false,
        }
    }

    fn end_beschraenkung(&self) -> bool {
        match self {
            Beschraenkung::Ende | Beschraenkung::Alle => true,
            Beschraenkung::Keine => false,
        }
    }
}

pub(crate) fn zeichne<Z, P, A>(
    _zugtyp: PhantomData<*const Z>,
    radius: canvas::Abstand<canvas::Radius>,
    winkel: Angle,
    beschraenkungen: Beschraenkung,
    transformations: Vec<canvas::Transformation>,
    with_invert_axis: impl FnOnce(
        &mut canvas::PathBuilder<canvas::Point, canvas::Arc>,
        Box<dyn for<'s> FnOnce(&'s mut canvas::PathBuilder<P, A>)>,
    ),
) -> canvas::Path
where
    Z: Zugtyp,
    P: From<canvas::Point> + canvas::ToPoint,
    A: From<canvas::Arc> + canvas::ToArc,
{
    let mut path_builder = canvas::PathBuilder::new();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| {
            zeichne_internal::<Z, P, A>(builder, radius, winkel, beschraenkungen)
        }),
    );
    path_builder.build_under_transformations(transformations)
}

// factor_y is expected to be -1 or +1, although other values should work as well
fn zeichne_internal<Z, P, A>(
    path_builder: &mut canvas::PathBuilder<P, A>,
    radius: canvas::Abstand<canvas::Radius>,
    winkel: Angle,
    beschraenkungen: Beschraenkung,
) where
    Z: Zugtyp,
    P: From<canvas::Point> + canvas::ToPoint,
    A: From<canvas::Arc> + canvas::ToArc,
{
    // Utility Größen
    let spurweite: canvas::Abstand<canvas::Radius> = Z::SPURWEITE.to_abstand().as_radius();
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let gleis_links: canvas::X = canvas::X(0.);
    let gleis_links_oben: canvas::Y = canvas::Y(0.);
    let gleis_links_unten: canvas::Y = gleis_links_oben + beschraenkung::<Z>();
    let radius_innen: canvas::Radius = canvas::Radius(0.) + radius - 0.5 * spurweite;
    let radius_aussen: canvas::Radius = radius_innen + spurweite;
    let radius_begrenzung_aussen: canvas::Abstand<canvas::Radius> =
        radius_aussen.to_abstand() + abstand::<Z>().as_radius();
    let radius_begrenzung_aussen_y: canvas::Abstand<canvas::Y> = radius_begrenzung_aussen.as_y();
    let begrenzung_x0: canvas::X = gleis_links + radius_begrenzung_aussen.as_x() * winkel.sin();
    let begrenzung_y0: canvas::Y =
        gleis_links_oben + radius_begrenzung_aussen_y * (1. - winkel.cos());
    let begrenzung_x1: canvas::X = begrenzung_x0 - beschraenkung::<Z>().as_x() * winkel.sin();
    let begrenzung_y1: canvas::Y = begrenzung_y0 + beschraenkung::<Z>() * winkel.cos();
    let bogen_zentrum_y: canvas::Y = gleis_links_oben + radius_begrenzung_aussen_y;
    // Beschränkungen
    if beschraenkungen.anfangs_beschraenkung() {
        path_builder.move_to(canvas::Point::new(gleis_links, gleis_links_oben).into());
        path_builder.line_to(canvas::Point::new(gleis_links, gleis_links_unten).into());
    }
    if beschraenkungen.end_beschraenkung() {
        path_builder.move_to(canvas::Point::new(begrenzung_x0, begrenzung_y0).into());
        path_builder.line_to(canvas::Point::new(begrenzung_x1, begrenzung_y1).into());
    }
    // Gleis
    path_builder.arc(
        canvas::Arc {
            center: canvas::Point::new(gleis_links, bogen_zentrum_y),
            radius: canvas::Radius(0.) + radius_aussen.to_abstand(),
            start: winkel_anfang,
            end: winkel_ende,
        }
        .into(),
    );
    path_builder.arc(
        canvas::Arc {
            center: canvas::Point::new(gleis_links, bogen_zentrum_y),
            radius: canvas::Radius(0.) + radius_innen.to_abstand(),
            start: winkel_anfang,
            end: winkel_ende,
        }
        .into(),
    );
}

pub(crate) fn fuelle<Z, P, A>(
    _zugtyp: PhantomData<*const Z>,
    radius: canvas::Abstand<canvas::Radius>,
    winkel: Angle,
    transformations: Vec<canvas::Transformation>,
    with_invert_axis: impl FnOnce(
        &mut canvas::PathBuilder<canvas::Point, canvas::Arc>,
        Box<dyn for<'s> FnOnce(&'s mut canvas::PathBuilder<P, A>)>,
    ),
) -> canvas::Path
where
    Z: Zugtyp,
    P: From<canvas::Point> + canvas::ToPoint,
    A: From<canvas::Arc> + canvas::ToArc,
{
    let mut path_builder = canvas::PathBuilder::new();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| fuelle_internal::<Z, P, A>(builder, radius, winkel)),
    );
    path_builder.build_under_transformations(transformations)
}

/// Geplant für canvas::PathType::EvenOdd
fn fuelle_internal<Z, P, A>(
    path_builder: &mut canvas::PathBuilder<P, A>,
    radius: canvas::Abstand<canvas::Radius>,
    winkel: Angle,
) where
    Z: Zugtyp,
    P: From<canvas::Point> + canvas::ToPoint,
    A: From<canvas::Arc> + canvas::ToArc,
{
    let spurweite = Z::SPURWEITE.to_abstand().as_radius();
    // Koordinaten für den Bogen
    let winkel_anfang: Angle = Angle::new(3. * PI / 2.);
    let winkel_ende: Angle = winkel_anfang + winkel;
    let radius_innen_abstand = radius - 0.5 * spurweite;
    let radius_innen: canvas::Radius = canvas::Radius(0.) + radius_innen_abstand;
    let radius_aussen_abstand = radius + 0.5 * spurweite;
    let radius_aussen: canvas::Radius = canvas::Radius(0.) + radius_aussen_abstand;
    let radius_aussen_abstand: canvas::Abstand<canvas::Radius> = radius_aussen.to_abstand();
    let bogen_zentrum_y: canvas::Y = canvas::Y(0.) + abstand::<Z>() + radius_aussen_abstand.as_y();
    // Koordinaten links
    let gleis_links: canvas::X = canvas::X(0.);
    let beschraenkung_oben: canvas::Y = canvas::Y(0.);
    let gleis_links_oben: canvas::Y = beschraenkung_oben + abstand::<Z>();
    let gleis_links_unten: canvas::Y = gleis_links_oben + Z::SPURWEITE.to_abstand();
    // Koordinaten rechts
    let gleis_rechts_oben: canvas::Point = canvas::Point::new(
        gleis_links + radius_aussen_abstand.as_x() * winkel.sin(),
        gleis_links_oben + radius_aussen_abstand.as_y() * (1. - winkel.cos()),
    );
    let gleis_rechts_unten: canvas::Point = canvas::Point::new(
        gleis_rechts_oben.x - spurweite.as_x() * winkel.sin(),
        gleis_rechts_oben.y + spurweite.as_y() * winkel.cos(),
    );
    // obere Kurve
    path_builder.arc(
        canvas::Arc {
            center: canvas::Point::new(gleis_links, bogen_zentrum_y),
            radius: radius_aussen,
            start: winkel_anfang,
            end: winkel_ende,
        }
        .into(),
    );
    path_builder.close();
    // untere Kurve
    path_builder.arc(
        canvas::Arc {
            center: canvas::Point::new(gleis_links, bogen_zentrum_y),
            radius: radius_innen,
            start: winkel_anfang,
            end: winkel_ende,
        }
        .into(),
    );
    path_builder.close();
    // Zwischen-Teil
    path_builder.move_to(canvas::Point::new(gleis_links, gleis_links_oben).into());
    path_builder.line_to(gleis_rechts_oben.into());
    path_builder.line_to(gleis_rechts_unten.into());
    path_builder.line_to(canvas::Point::new(gleis_links, gleis_links_unten).into());
    path_builder.close();
}
