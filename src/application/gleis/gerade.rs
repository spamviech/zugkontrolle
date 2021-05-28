//! Definition und zeichnen einer Gerade

use std::hash::Hash;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use super::anchor;
use super::typen::*;

/// Definition einer Gerade
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Gerade<Z> {
    pub zugtyp: PhantomData<Z>,
    pub länge: Skalar,
    pub beschreibung: Option<String>,
}
impl<Z> Gerade<Z> {
    pub const fn neu(länge: Länge) -> Self {
        Gerade { zugtyp: PhantomData, länge: länge.als_skalar(), beschreibung: None }
    }

    pub fn neu_mit_beschreibung(länge: Länge, beschreibung: impl Into<String>) -> Self {
        Gerade {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            beschreibung: Some(beschreibung.into()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Gerade<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn size(&self) -> Vektor {
        size::<Z>(self.länge)
    }

    fn zeichne(&self) -> Vec<Pfad> {
        vec![zeichne(self.zugtyp, self.länge, true, Vec::new(), pfad::Erbauer::with_normal_axis)]
    }

    fn fülle(&self) -> Vec<Pfad> {
        vec![fülle(self.zugtyp, self.länge, Vec::new(), pfad::Erbauer::with_normal_axis)]
    }

    fn beschreibung(&self) -> Option<(Position, &String)> {
        self.beschreibung.as_ref().map(|text| {
            (
                Position {
                    punkt: Vektor { x: self.länge.halbiert(), y: beschränkung::<Z>().halbiert() },
                    winkel: Winkel(0.),
                },
                text,
            )
        })
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        innerhalb::<Z>(self.länge, relative_position)
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        let gleis_links = Skalar(0.);
        let gleis_rechts = gleis_links + self.länge;
        let beschränkung_mitte = beschränkung::<Z>().halbiert();
        AnchorPoints {
            anfang: anchor::Anchor {
                position: Vektor { x: gleis_links, y: beschränkung_mitte },
                richtung: winkel::PI,
            },
            ende: anchor::Anchor {
                position: Vektor { x: gleis_rechts, y: beschränkung_mitte },
                richtung: winkel::ZERO,
            },
        }
    }
}

pub(crate) fn size<Z: Zugtyp>(länge: Skalar) -> Vektor {
    Vektor { x: länge, y: beschränkung::<Z>() }
}

pub(crate) fn zeichne<Z, P, A>(
    _zugtyp: PhantomData<Z>,
    länge: Skalar,
    beschränkungen: bool,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn for<'s> FnOnce(&'s mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| zeichne_internal::<Z, P, A>(builder, länge, beschränkungen)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn zeichne_internal<Z, P, A>(
    path_builder: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
    beschränkungen: bool,
) where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let beschränkung_unten = beschränkung_oben + beschränkung::<Z>();
    let gleis_oben = beschränkung_oben + abstand::<Z>();
    let gleis_unten = gleis_oben + spurweite::<Z>();
    // Beschränkungen
    if beschränkungen {
        path_builder.move_to(Vektor { x: gleis_links, y: beschränkung_oben }.into());
        path_builder.line_to(Vektor { x: gleis_links, y: beschränkung_unten }.into());
        path_builder.move_to(Vektor { x: gleis_rechts, y: beschränkung_oben }.into());
        path_builder.line_to(Vektor { x: gleis_rechts, y: beschränkung_unten }.into());
    }
    // Gleis
    path_builder.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    path_builder.move_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
}

pub(crate) fn fülle<Z, P, A>(
    _zugtyp: PhantomData<Z>,
    länge: Skalar,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn for<'s> FnOnce(&'s mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| fülle_internal::<Z, P, A>(builder, länge)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn fülle_internal<Z, P, A>(path_builder: &mut pfad::Erbauer<P, A>, länge: Skalar)
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let gleis_oben = beschränkung_oben + abstand::<Z>();
    let gleis_unten = gleis_oben + spurweite::<Z>();
    // Zeichne Umriss
    path_builder.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_oben }.into());
}

pub(crate) fn innerhalb<Z: Zugtyp>(länge: Skalar, relative_position: Vektor) -> bool {
    relative_position.x >= Skalar(0.)
        && relative_position.x <= länge
        && relative_position.y >= abstand::<Z>()
        && relative_position.y <= abstand::<Z>() + spurweite::<Z>()
}