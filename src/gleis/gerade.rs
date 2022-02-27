//! Definition und zeichnen einer Gerade

use std::{fmt::Debug, hash::Hash};

use serde::{Deserialize, Serialize};
use zugkontrolle_macros::alias_serialisiert_unit;

use crate::{
    gleis::verbindung::Verbindung,
    nachschlagen::impl_nachschlagen,
    steuerung::kontakt::{Kontakt, KontaktSerialisiert},
    typen::*,
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

#[impl_nachschlagen(Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang,
    Ende,
}

impl<Anschluss: MitName> Zeichnen for Gerade<Anschluss> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, spurweite: Spurweite) -> Rechteck {
        rechteck(spurweite, self.länge)
    }

    fn zeichne(&self, spurweite: Spurweite) -> Vec<Pfad> {
        vec![zeichne(spurweite, self.länge, true, Vec::new(), pfad::Erbauer::with_normal_axis)]
    }

    fn fülle(&self, spurweite: Spurweite) -> Vec<(Pfad, Transparenz)> {
        vec![(
            fülle(spurweite, self.länge, Vec::new(), pfad::Erbauer::with_normal_axis),
            Transparenz::Voll,
        )]
    }

    fn beschreibung_und_name(
        &self,
        spurweite: Spurweite,
    ) -> (Position, Option<&String>, Option<&String>) {
        (
            Position {
                punkt: Vektor {
                    x: self.länge.halbiert(), y: spurweite.beschränkung().halbiert()
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.kontakt.name(),
        )
    }

    fn innerhalb(
        &self,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> bool {
        innerhalb(spurweite, self.länge, relative_position, ungenauigkeit)
    }

    fn verbindungen(&self, spurweite: Spurweite) -> Self::Verbindungen {
        let gleis_links = Skalar(0.);
        let gleis_rechts = gleis_links + self.länge;
        let beschränkung_mitte = spurweite.beschränkung().halbiert();
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

pub(crate) fn rechteck(spurweite: Spurweite, länge: Skalar) -> Rechteck {
    Rechteck::mit_größe(Vektor { x: länge, y: spurweite.beschränkung() })
}

pub(crate) fn zeichne<P, A>(
    spurweite: Spurweite,
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
        Box::new(move |builder| {
            zeichne_internal::<P, A>(spurweite, builder, länge, beschränkungen)
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn zeichne_internal<P, A>(
    spurweite: Spurweite,
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
    let beschränkung_unten = beschränkung_oben + spurweite.beschränkung();
    let gleis_oben = beschränkung_oben + spurweite.abstand();
    let gleis_unten = gleis_oben + spurweite.spurweite();
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
    spurweite: Spurweite,
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
        Box::new(move |builder| fülle_internal::<P, A>(spurweite, builder, länge)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

fn fülle_internal<P, A>(
    spurweite: Spurweite,
    path_builder: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let gleis_oben = beschränkung_oben + spurweite.abstand();
    let gleis_unten = gleis_oben + spurweite.spurweite();
    // Zeichne Umriss
    path_builder.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
    path_builder.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    path_builder.line_to(Vektor { x: gleis_links, y: gleis_oben }.into());
}

pub(crate) fn innerhalb(
    spurweite: Spurweite,
    länge: Skalar,
    relative_position: Vektor,
    ungenauigkeit: Skalar,
) -> bool {
    relative_position.x + ungenauigkeit >= Skalar(0.)
        && relative_position.x - ungenauigkeit <= länge
        && relative_position.y + ungenauigkeit >= spurweite.abstand()
        && relative_position.y - ungenauigkeit <= spurweite.abstand() + spurweite.spurweite()
}
