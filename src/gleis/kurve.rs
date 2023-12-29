//! Definition und zeichnen einer [Kurve].

use std::{f32::consts::PI, fmt::Debug};

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
        mm::{Radius, Spurweite},
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        MitName, Transparenz, Zeichnen,
    },
    util::nachschlagen::impl_nachschlagen,
};

/// Definition einer Kurve.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[alias_serialisiert_unit(KontaktSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Kurve<Anschluss = Option<Kontakt>> {
    /// Der Radius auf dem Canvas.
    pub radius: Skalar,
    /// Der Winkel der Kurve.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der Kurve, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [Kontakt] an der Schiene.
    pub kontakt: Anschluss,
}

impl KurveUnit {
    /// Erstelle eine neue [Kurve].
    pub const fn neu(radius: Radius, winkel: Winkel) -> Self {
        KurveUnit { radius: radius.als_skalar(), winkel, beschreibung: None, kontakt: () }
    }

    /// Erstelle eine neue [Kurve] mit einer allgemeinen Beschreibung, z.B. der Produktnummer.
    pub fn neu_mit_beschreibung(
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        KurveUnit {
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
            kontakt: (),
        }
    }
}

#[impl_nachschlagen(Verbindung, Verbindungen, Debug)]
/// [Verbindungen](Verbindung) einer [Kurve].
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    /// Das eine Ende der Kurve.
    Anfang,
    /// Das andere Ende der Kurve.
    Ende,
}

impl<Anschlüsse, Anschlüsse2: MitName + MitKontakt> Zeichnen<Anschlüsse2> for Kurve<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        rechteck(spurweite, self.radius, self.winkel)
    }

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        let level_und_trigger = anschlüsse.aktuelles_level_und_trigger();
        let mut pfade = vec![zeichne(
            spurweite,
            self.radius,
            self.winkel,
            Beschränkung::Alle,
            Vec::new(),
            pfad::Erbauer::with_normal_axis,
        )];
        if level_und_trigger.is_some() {
            pfade.push(zeichne_kontakt(
                spurweite,
                self.radius,
                self.winkel,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
        }
        pfade
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        let level_und_trigger = anschlüsse.aktuelles_level_und_trigger();
        let mut pfade = vec![(
            fülle(
                spurweite,
                self.radius,
                self.winkel,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ),
            None,
            Transparenz::Voll,
        )];
        if let Some((Some(level), trigger)) = level_und_trigger {
            let (pfad, farbe) = fülle_kontakt(
                spurweite,
                self.radius,
                self.winkel,
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
        let half_angle = 0.5 * self.winkel;
        (
            Position {
                punkt: Vektor {
                    x: self.radius * half_angle.sin(),
                    y: spurweite.beschränkung().halbiert()
                        + self.radius * (Skalar(1.) - half_angle.cos()),
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
        innerhalb(spurweite, self.radius, self.winkel, relative_position, ungenauigkeit)
    }

    fn verbindungen(
        &self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite
    ) -> Self::Verbindungen {
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        Verbindungen {
            anfang: Verbindung {
                position: Vektor { x: Skalar(0.), y: halbe_beschränkung },
                richtung: winkel::PI,
            },
            ende: Verbindung {
                position: Vektor {
                    x: self.radius * self.winkel.sin(),
                    y: halbe_beschränkung + self.radius * (Skalar(1.) - self.winkel.cos()),
                },
                richtung: self.winkel,
            },
        }
    }
}

pub(crate) fn rechteck(spurweite: Spurweite, radius: Skalar, winkel: Winkel) -> Rechteck {
    // Hilfswerte
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    let breite_faktor;
    let höhe_vergleich;
    let position_x_faktor;
    if winkel < winkel::FRAC_PI_2 {
        breite_faktor = winkel.sin();
        höhe_vergleich = radius_begrenzung_außen * (Skalar(1.) - winkel.cos())
            + spurweite.beschränkung() * winkel.cos();
        position_x_faktor = Skalar(0.);
    } else {
        breite_faktor = Skalar(1.);
        if winkel < winkel::PI {
            höhe_vergleich = radius_begrenzung_außen * (Skalar(1.) - winkel.cos());
            position_x_faktor = Skalar(0.);
        } else {
            höhe_vergleich = radius_begrenzung_außen;
            position_x_faktor = Skalar(1.) - winkel.cos();
        }
    }
    // Minimale Koordinaten
    let ecke_a = Vektor { x: position_x_faktor * radius_begrenzung_außen, y: Skalar(0.) };
    // Maximale Koordinaten
    let breite = radius_begrenzung_außen * breite_faktor;
    let höhe = spurweite.beschränkung().max(&höhe_vergleich);
    let ecke_b = Vektor { x: breite, y: höhe };
    // Rückgabewert
    Rechteck { ecke_a, ecke_b }
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

pub(crate) fn zeichne<P, A>(
    spurweite: Spurweite,
    radius: Skalar,
    winkel: Winkel,
    beschränkungen: Beschränkung,
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
            zeichne_internal::<P, A>(spurweite, builder, radius, winkel, beschränkungen)
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

// factor_y is expected to be -1 or +1, although other values should work as well
fn zeichne_internal<P, A>(
    spurweite: Spurweite,
    path_builder: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
    beschränkungen: Beschränkung,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Utility Größen
    let spurweite_skalar: Skalar = spurweite.als_skalar();
    let beschränkung: Skalar = spurweite.beschränkung();
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: beschränkung };
    let radius_begrenzung_außen: Skalar = spurweite.radius_begrenzung_außen(radius);
    let radius_außen = radius_begrenzung_außen - spurweite.abstand();
    let radius_innen = radius_außen - spurweite_skalar;
    let begrenzung0 = gleis_links_oben
        + radius_begrenzung_außen * Vektor { x: winkel.sin(), y: (Skalar(1.) - winkel.cos()) };
    let begrenzung1 = begrenzung0 + beschränkung * Vektor { x: -winkel.sin(), y: winkel.cos() };
    let bogen_zentrum = gleis_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Beschränkungen
    if beschränkungen.anfangs_beschränkung() {
        path_builder.move_to(gleis_links_oben.into());
        path_builder.line_to(gleis_links_unten.into());
    }
    if beschränkungen.end_beschränkung() {
        path_builder.move_to(begrenzung0.into());
        path_builder.line_to(begrenzung1.into());
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
    );
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_innen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
    );
}

pub(crate) fn zeichne_kontakt<P, A>(
    spurweite: Spurweite,
    radius: Skalar,
    winkel: Winkel,
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
        Box::new(move |builder| zeichne_kontakt_intern::<P, A>(spurweite, builder, radius, winkel)),
    );
    erbauer.baue_unter_transformationen(transformationen)
}

fn zeichne_kontakt_intern<P, A>(
    spurweite: Spurweite,
    erbauer: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Utility Größen
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    let radius_begrenzung_außen: Skalar = spurweite.radius_begrenzung_außen(radius);
    let radius = (Skalar(0.5) * spurweite.abstand())
        .min(&(Skalar(0.25) * radius_begrenzung_außen * Skalar(winkel.0)));
    let anzeige_winkel = Winkel(3. * radius.0 / radius_begrenzung_außen.0);
    let zentrum = gleis_links_oben
        + radius_begrenzung_außen
            * Vektor { x: anzeige_winkel.sin(), y: (Skalar(1.) - anzeige_winkel.cos()) };
    // Kontakt
    erbauer.arc(Bogen { zentrum, radius, anfang: winkel::ZERO, ende: winkel::TAU }.into());
}

pub(crate) fn fülle<P, A>(
    spurweite: Spurweite,
    radius: Skalar,
    winkel: Winkel,
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
        Box::new(move |builder| fülle_internal::<P, A>(spurweite, builder, radius, winkel)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

/// Geplant für canvas::PathType::EvenOdd
fn fülle_internal<P, A>(
    spurweite: Spurweite,
    path_builder: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
) where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let spurweite_skalar = spurweite.als_skalar();
    let abstand = spurweite.abstand();
    let beschränkung_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    // Koordinaten für den Bogen
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let radius_begrenzung_außen: Skalar = spurweite.radius_begrenzung_außen(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite_skalar;
    let bogen_zentrum =
        beschränkung_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Koordinaten links
    let gleis_links_oben = beschränkung_links_oben + Vektor { x: Skalar(0.), y: abstand };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: spurweite_skalar };
    // Koordinaten rechts
    let gleis_rechts_oben: Vektor = gleis_links_oben
        + radius_außen * Vektor { x: winkel.sin(), y: (Skalar(1.) - winkel.cos()) };
    let gleis_rechts_unten: Vektor = gleis_rechts_oben
        + Vektor { x: -spurweite_skalar * winkel.sin(), y: spurweite_skalar * winkel.cos() };
    // obere Kurve
    path_builder.arc(
        Bogen {
            zentrum: bogen_zentrum,
            radius: radius_außen,
            anfang: winkel_anfang,
            ende: winkel_ende,
        }
        .into(),
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
    );
    path_builder.close();
    // Zwischen-Teil
    path_builder.move_to(gleis_links_oben.into());
    path_builder.line_to(gleis_rechts_oben.into());
    path_builder.line_to(gleis_rechts_unten.into());
    path_builder.line_to(gleis_links_unten.into());
    path_builder.close();
}

fn fülle_kontakt<P, A>(
    spurweite: Spurweite,
    radius: Skalar,
    winkel: Winkel,
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
            fülle_kontakt_intern::<P, A>(spurweite, builder, radius, winkel, level, trigger)
        }),
    );
    // Rückgabewert
    (erbauer.baue_unter_transformationen(transformationen), farbe)
}

fn fülle_kontakt_intern<P, A>(
    spurweite: Spurweite,
    erbauer: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
    level: Level,
    trigger: Trigger,
) -> Farbe
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Utility Größen
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    let radius_begrenzung_außen: Skalar = spurweite.radius_begrenzung_außen(radius);
    let radius = (Skalar(0.5) * spurweite.abstand())
        .min(&(Skalar(0.25) * radius_begrenzung_außen * Skalar(winkel.0)));
    let anzeige_winkel = Winkel(3. * radius.0 / radius_begrenzung_außen.0);
    let zentrum = gleis_links_oben
        + radius_begrenzung_außen
            * Vektor { x: anzeige_winkel.sin(), y: (Skalar(1.) - anzeige_winkel.cos()) };
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
    radius: Skalar,
    winkel: Winkel,
    relative_position: Vektor,
    ungenauigkeit: Skalar,
) -> bool {
    let spurweite_skalar = spurweite.als_skalar();
    let abstand = spurweite.abstand();
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite_skalar;
    let bogen_zentrum = Vektor { x: Skalar(0.), y: abstand + radius_außen };
    let radius_vector = bogen_zentrum - relative_position;
    let länge = radius_vector.länge();
    if länge + ungenauigkeit > radius_innen && länge - ungenauigkeit < radius_außen {
        let acos = Winkel::acos(radius_vector.y / länge);
        let mut test_winkel: Winkel = if radius_vector.x > Skalar(0.) { -acos } else { acos };
        // normalisiere winkel
        while test_winkel < winkel::ZERO {
            test_winkel += winkel::TAU
        }
        if test_winkel < winkel {
            return true;
        }
    }
    false
}
