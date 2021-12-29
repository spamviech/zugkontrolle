//! Definition und zeichnen einer Gerade

use std::{fmt::Debug, hash::Hash};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::alias_serialisiert_unit;

use crate::{
    application::{typen::*, verbindung::Verbindung},
    lookup::impl_lookup,
    steuerung::kontakt::{Kontakt, KontaktSerialisiert},
};

/// Definition einer Gerade
#[alias_serialisiert_unit(KontaktSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Gerade<Anschluss = Option<Kontakt>> {
    pub länge: Skalar,
    pub beschreibung: Option<String>,
    pub kontakt: Anschluss,
}

impl GeradeUnit {
    pub const fn neu(länge: Länge) -> Self {
        GeradeUnit { länge: länge.als_skalar(), beschreibung: None, kontakt: () }
    }

    pub fn neu_mit_beschreibung(länge: Länge, beschreibung: impl Into<String>) -> Self {
        GeradeUnit {
            länge: länge.als_skalar(),
            beschreibung: Some(beschreibung.into()),
            kontakt: (),
        }
    }
}

#[impl_lookup(Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang,
    Ende,
}

impl<Anschluss: MitName> Zeichnen for Gerade<Anschluss> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self) -> Rechteck {
        rechteck(self.länge)
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
                punkt: Vektor { x: self.länge.halbiert(), y: beschränkung().halbiert() },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.kontakt.name(),
        )
    }

    fn innerhalb(&self, relative_position: Vektor, ungenauigkeit: Skalar) -> bool {
        innerhalb(self.länge, relative_position, ungenauigkeit)
    }

    fn verbindungen(&self) -> Self::Verbindungen {
        let gleis_links = Skalar(0.);
        let gleis_rechts = gleis_links + self.länge;
        let beschränkung_mitte = beschränkung().halbiert();
        Verbindungen {
            anfang: Verbindung {
                position: Vektor { x: gleis_links, y: beschränkung_mitte },
                richtung: winkel::PI,
            },
            ende: Verbindung {
                position: Vektor { x: gleis_rechts, y: beschränkung_mitte },
                richtung: winkel::ZERO,
            },
        }
    }
}

pub(crate) fn rechteck(länge: Skalar) -> Rechteck {
    Rechteck::mit_größe(Vektor { x: länge, y: beschränkung() })
}

pub(crate) fn zeichne<P, A>(
    länge: Skalar,
    beschränkungen: bool,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| zeichne_internal::<P, A>(builder, länge, beschränkungen)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn zeichne_internal<P, A>(
    path_builder: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
    beschränkungen: bool,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let beschränkung_unten = beschränkung_oben + beschränkung();
    let gleis_oben = beschränkung_oben + abstand();
    let gleis_unten = gleis_oben + spurweite();
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

pub(crate) fn fülle<P, A>(
    länge: Skalar,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| fülle_internal::<P, A>(builder, länge)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn fülle_internal<P, A>(path_builder: &mut pfad::Erbauer<P, A>, länge: Skalar)
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let gleis_oben = beschränkung_oben + abstand();
    let gleis_unten = gleis_oben + spurweite();
    // Zeichne Umriss
    path_builder.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_oben }.into());
}

pub(crate) fn innerhalb(länge: Skalar, relative_position: Vektor, ungenauigkeit: Skalar) -> bool {
    relative_position.x + ungenauigkeit >= Skalar(0.)
        && relative_position.x - ungenauigkeit <= länge
        && relative_position.y + ungenauigkeit >= abstand()
        && relative_position.y - ungenauigkeit <= abstand() + spurweite()
}
