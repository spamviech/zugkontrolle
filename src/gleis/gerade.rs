//! Definition und zeichnen einer Gerade.

use std::{fmt::Debug, hash::Hash};

use serde::{Deserialize, Serialize};
use zugkontrolle_macros::alias_serialisiert_unit;

use crate::{
    anschluss::{level::Level, trigger::Trigger},
    gleis::verbindung::Verbindung,
    steuerung::kontakt::{Kontakt, KontaktSerialisiert, MitKontakt},
    typen::{
        canvas::{
            pfad::{self, Bogen, Pfad, Transformation},
            Position,
        },
        farbe::{self, Farbe},
        mm::{Länge, Spurweite},
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Winkel},
        MitName, Transparenz, Zeichnen,
    },
    util::nachschlagen::impl_nachschlagen,
};

/// Definition einer Gerade.
#[alias_serialisiert_unit(KontaktSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Gerade<Anschluss = Option<Kontakt>> {
    /// Die Länge der Gerade auf dem [`Canvas`](iced::widget::canvas::Canvas).
    pub länge: Skalar,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [`Kontakt`] an der Schiene.
    pub kontakt: Anschluss,
}

impl GeradeUnit {
    /// Erstelle eine neue [`Gerade`].
    pub const fn neu(länge: Länge) -> Self {
        GeradeUnit { länge: länge.als_skalar(), beschreibung: None, kontakt: () }
    }

    /// Erstelle eine neue [`Gerade`] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    pub fn neu_mit_beschreibung(länge: Länge, beschreibung: impl Into<String>) -> Self {
        GeradeUnit {
            länge: länge.als_skalar(),
            beschreibung: Some(beschreibung.into()),
            kontakt: (),
        }
    }
}

#[impl_nachschlagen(Verbindung, Verbindungen, Debug)]
/// [Verbindungen](Verbindung) einer [`Geraden`](Gerade).
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    /// Das eine Ende der Gerade.
    Anfang,
    /// Das andere Ende der Gerade.
    Ende,
}

impl<Anschlüsse, Anschlüsse2: MitName + MitKontakt> Zeichnen<Anschlüsse2> for Gerade<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        rechteck(spurweite, self.länge)
    }

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        let level_und_trigger = anschlüsse.aktuelles_level_und_trigger();
        let mut pfade =
            vec![zeichne(spurweite, self.länge, true, Vec::new(), pfad::Erbauer::with_normal_axis)];
        if level_und_trigger.is_some() {
            pfade.push(zeichne_kontakt(
                spurweite,
                self.länge,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ))
        }
        pfade
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        let level_und_trigger = anschlüsse.aktuelles_level_und_trigger();
        let pfad = fülle(spurweite, self.länge, Vec::new(), pfad::Erbauer::with_normal_axis);
        let mut pfade = vec![(pfad, None, Transparenz::Voll)];
        if let Some((Some(level), trigger)) = level_und_trigger {
            let (pfad, farbe) = fülle_kontakt(
                spurweite,
                self.länge,
                level,
                trigger,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            );
            pfade.push((pfad, Some(farbe), Transparenz::Voll));
        }
        pfade
    }

    fn beschreibung_und_name<'s, 't>(
        &'s self,
        anschlüsse: &'t Anschlüsse2,
        spurweite: Spurweite,
    ) -> (Position, Option<&'s str>, Option<&'t str>) {
        (
            Position {
                punkt: Vektor {
                    x: self.länge.halbiert(), y: spurweite.beschränkung().halbiert()
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref().map(String::as_str),
            anschlüsse.name(),
        )
    }

    fn innerhalb(
        &self,
        _anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> bool {
        innerhalb(spurweite, self.länge, relative_position, ungenauigkeit)
    }

    fn verbindungen(
        &self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite
    ) -> Self::Verbindungen {
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
    transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut erbauer = pfad::Erbauer::neu();
    mit_invertierter_achse(
        &mut erbauer,
        Box::new(move |builder| zeichne_intern::<P, A>(spurweite, builder, länge, beschränkungen)),
    );
    erbauer.baue_unter_transformationen(transformationen)
}

fn zeichne_intern<P, A>(
    spurweite: Spurweite,
    erbauer: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
    beschränkungen: bool,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let beschränkung_unten = beschränkung_oben + spurweite.beschränkung();
    let gleis_oben = beschränkung_oben + spurweite.abstand();
    let gleis_unten = gleis_oben + spurweite.als_skalar();
    // Beschränkungen
    if beschränkungen {
        erbauer.move_to(Vektor { x: gleis_links, y: beschränkung_oben }.into());
        erbauer.line_to(Vektor { x: gleis_links, y: beschränkung_unten }.into());
        erbauer.move_to(Vektor { x: gleis_rechts, y: beschränkung_oben }.into());
        erbauer.line_to(Vektor { x: gleis_rechts, y: beschränkung_unten }.into());
    }
    // Gleis
    erbauer.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    erbauer.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    erbauer.move_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    erbauer.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
}

pub(crate) fn zeichne_kontakt<P, A>(
    spurweite: Spurweite,
    länge: Skalar,
    transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut erbauer = pfad::Erbauer::neu();
    mit_invertierter_achse(
        &mut erbauer,
        Box::new(move |builder| zeichne_kontakt_intern::<P, A>(spurweite, builder, länge)),
    );
    erbauer.baue_unter_transformationen(transformationen)
}

fn zeichne_kontakt_intern<P, A>(
    spurweite: Spurweite,
    erbauer: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let beschränkung_oben = Skalar(0.);
    let radius = (Skalar(0.5) * spurweite.abstand()).min(&(Skalar(0.25) * länge));
    let zentrum = Vektor { x: gleis_links + Skalar(3.) * radius, y: beschränkung_oben };
    // Kontakt
    erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU }.into());
}

pub(crate) fn fülle<P, A>(
    spurweite: Spurweite,
    länge: Skalar,
    transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut erbauer = pfad::Erbauer::neu();
    mit_invertierter_achse(
        &mut erbauer,
        Box::new(move |builder| fülle_intern::<P, A>(spurweite, builder, länge)),
    );
    // Rückgabewert
    erbauer.baue_unter_transformationen(transformationen)
}

fn fülle_intern<P, A>(spurweite: Spurweite, erbauer: &mut pfad::Erbauer<P, A>, länge: Skalar)
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let gleis_rechts = gleis_links + länge;
    let beschränkung_oben = Skalar(0.);
    let gleis_oben = beschränkung_oben + spurweite.abstand();
    let gleis_unten = gleis_oben + spurweite.als_skalar();
    // Zeichne Umriss
    erbauer.move_to(Vektor { x: gleis_links, y: gleis_oben }.into());
    erbauer.line_to(Vektor { x: gleis_links, y: gleis_unten }.into());
    erbauer.line_to(Vektor { x: gleis_rechts, y: gleis_unten }.into());
    erbauer.line_to(Vektor { x: gleis_rechts, y: gleis_oben }.into());
    erbauer.line_to(Vektor { x: gleis_links, y: gleis_oben }.into());
}

fn fülle_kontakt<P, A>(
    spurweite: Spurweite,
    länge: Skalar,
    level: Level,
    trigger: Trigger,
    transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>) -> Farbe>,
    ) -> Farbe,
) -> (Pfad, Farbe)
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut erbauer = pfad::Erbauer::neu();
    let farbe = mit_invertierter_achse(
        &mut erbauer,
        Box::new(move |builder| {
            fülle_kontakt_intern::<P, A>(spurweite, builder, länge, level, trigger)
        }),
    );
    // Rückgabewert
    (erbauer.baue_unter_transformationen(transformationen), farbe)
}

fn fülle_kontakt_intern<P, A>(
    spurweite: Spurweite,
    erbauer: &mut pfad::Erbauer<P, A>,
    länge: Skalar,
    level: Level,
    trigger: Trigger,
) -> Farbe
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Koordinaten
    let gleis_links = Skalar(0.);
    let beschränkung_oben = Skalar(0.);
    let radius = (Skalar(0.5) * spurweite.abstand()).min(&(Skalar(0.25) * länge));
    let zentrum = Vektor { x: gleis_links + Skalar(3.) * radius, y: beschränkung_oben };
    // Kontakt
    erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU }.into());
    // Anzeigefarbe
    match (level, trigger) {
        (Level::Low, Trigger::RisingEdge) => farbe::ROT,
        (Level::High, Trigger::RisingEdge) => farbe::GRÜN,
        (Level::Low, Trigger::FallingEdge) => farbe::GRÜN,
        (Level::High, Trigger::FallingEdge) => farbe::ROT,
        (Level::Low, _trigger) => farbe::BLAU,
        (Level::High, _trigger) => farbe::GRÜN,
    }
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
        && relative_position.y - ungenauigkeit <= spurweite.abstand() + spurweite.als_skalar()
}
