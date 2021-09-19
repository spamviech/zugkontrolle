//! Definition und zeichnen einer Gerade

use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::alias_serialisiert_unit;

use crate::{
    application::{typen::*, verbindung},
    lookup::impl_lookup,
    steuerung::kontakt::{Kontakt, KontaktSerialisiert},
};

/// Definition einer Gerade
#[alias_serialisiert_unit(KontaktSerialisiert)]
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Gerade<Z, Anschluss = Option<Kontakt>> {
    pub zugtyp: PhantomData<fn() -> Z>,
    pub länge: Skalar,
    pub beschreibung: Option<String>,
    pub kontakt: Anschluss,
}
impl<Z> GeradeUnit<Z> {
    pub fn neu(länge: Länge) -> Self {
        GeradeUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            beschreibung: None,
            kontakt: (),
        }
    }

    pub fn neu_mit_beschreibung(länge: Länge, beschreibung: impl Into<String>) -> Self {
        GeradeUnit {
            zugtyp: PhantomData,
            länge: länge.als_skalar(),
            beschreibung: Some(beschreibung.into()),
            kontakt: (),
        }
    }
}

#[impl_lookup(verbindung::Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp, Anschluss: MitName> Zeichnen for Gerade<Z, Anschluss> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self) -> Rechteck {
        todo!()
        // size::<Z>(self.länge)
    }

    fn zeichne(&self) -> Vec<Pfad> {
        vec![zeichne(self.zugtyp, self.länge, true, Vec::new(), pfad::Erbauer::with_normal_axis)]
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
        vec![(
            fülle(self.zugtyp, self.länge, Vec::new(), pfad::Erbauer::with_normal_axis),
            Transparenz::Voll,
        )]
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        (
            Position {
                punkt: Vektor { x: self.länge.halbiert(), y: beschränkung::<Z>().halbiert() },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.kontakt.name(),
        )
    }

    fn innerhalb(&self, relative_position: Vektor) -> bool {
        innerhalb::<Z>(self.länge, relative_position)
    }

    fn verbindungen(&self) -> Self::Verbindungen {
        todo!()
        // let gleis_links = Skalar(0.);
        // let gleis_rechts = gleis_links + self.länge;
        // let beschränkung_mitte = beschränkung::<Z>().halbiert();
        // Verbindungen {
        //     anfang: verbindung::Verbindung {
        //         position: Vektor { x: gleis_links, y: beschränkung_mitte },
        //         richtung: winkel::PI,
        //     },
        //     ende: verbindung::Verbindung {
        //         position: Vektor { x: gleis_rechts, y: beschränkung_mitte },
        //         richtung: winkel::ZERO,
        //     },
        // }
    }
}

pub(crate) fn size<Z: Zugtyp>(länge: Skalar) -> Vektor {
    Vektor { x: länge, y: beschränkung::<Z>() }
}

pub(crate) fn zeichne<Z, P, A>(
    _zugtyp: PhantomData<fn() -> Z>,
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
    _zugtyp: PhantomData<fn() -> Z>,
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
