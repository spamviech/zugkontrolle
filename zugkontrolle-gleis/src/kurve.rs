//! Definition und zeichnen einer [`Kurve`].

use std::{f32::consts::PI, fmt::Debug};

use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::{level::Level, trigger::Trigger};
use zugkontrolle_macros::{alias_serialisiert_unit, impl_nachschlagen};
use zugkontrolle_typen::{
    canvas::{
        pfad::{self, Bogen, Pfad, Transformation},
        Position,
    },
    farbe::{self, Farbe},
    mm::{Radius, Spurweite},
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    verbindung::Verbindung,
    winkel::{self, Trigonometrie, Winkel},
    MitName, Transparenz, Zeichnen,
};

use crate::steuerung::kontakt::{Kontakt, KontaktSerialisiert, MitKontakt};

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
    /// Der Anschluss für einen [`Kontakt`] an der Schiene.
    pub kontakt: Anschluss,
}

impl KurveUnit {
    /// Erstelle eine neue [`Kurve`].
    #[must_use]
    pub const fn neu(radius: Radius, winkel: Winkel) -> Self {
        KurveUnit { radius: radius.als_skalar(), winkel, beschreibung: None, kontakt: () }
    }

    /// Erstelle eine neue [`Kurve`] mit einer allgemeinen Beschreibung, z.B. der Produktnummer.
    #[must_use]
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
/// [Verbindungen](Verbindung) einer [`Kurve`].
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
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let half_angle = 0.5 * self.winkel;
        (
            Position {
                punkt: Vektor {
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    x: self.radius * half_angle.sin(),
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    y: spurweite.beschränkung().halbiert()
                        + self.radius * (Skalar(1.) - half_angle.cos()),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_deref(),
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
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    x: self.radius * self.winkel.sin(),
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    y: halbe_beschränkung + self.radius * (Skalar(1.) - self.winkel.cos()),
                },
                richtung: self.winkel,
            },
        }
    }
}

/// Das kleinste, parallel zu den x-y-Achsen, einschließende Rechteck für eine [`Kurve`].
pub(crate) fn rechteck(spurweite: Spurweite, radius: Skalar, winkel: Winkel) -> Rechteck {
    // Hilfswerte
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    let breite_faktor;
    let höhe_vergleich;
    let position_x_faktor;
    if winkel < winkel::FRAC_PI_2 {
        breite_faktor = winkel.sin();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            höhe_vergleich = radius_begrenzung_außen * (Skalar(1.) - winkel.cos())
                + spurweite.beschränkung() * winkel.cos();
        }
        position_x_faktor = Skalar(0.);
    } else {
        breite_faktor = Skalar(1.);
        if winkel < winkel::PI {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                höhe_vergleich = radius_begrenzung_außen * (Skalar(1.) - winkel.cos());
            }
            position_x_faktor = Skalar(0.);
        } else {
            höhe_vergleich = radius_begrenzung_außen;
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                position_x_faktor = Skalar(1.) - winkel.cos();
            }
        }
    }
    // Minimale Koordinaten
    let ecke_a = Vektor {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        x: position_x_faktor * radius_begrenzung_außen,
        y: Skalar(0.),
    };
    // Maximale Koordinaten
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let breite = radius_begrenzung_außen * breite_faktor;
    let höhe = spurweite.beschränkung().max(&höhe_vergleich);
    let ecke_b = Vektor { x: breite, y: höhe };
    // Rückgabewert
    Rechteck { ecke_a, ecke_b }
}

/// Hilfs-Typ für [`zeichne`]: Welche Beschränkung sollen gezeichnet werden.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Beschränkung {
    /// Zeichne keine Beschränkung.
    Keine,
    /// Zeichne nur die End-Beschränkung.
    Ende,
    /// Zeichne alle Beschränkungen.
    Alle,
}

impl Beschränkung {
    /// Soll die Anfangs-Beschränkung gezeigt werden.
    fn anfangs_beschränkung(self) -> bool {
        match self {
            Beschränkung::Alle => true,
            Beschränkung::Keine | Beschränkung::Ende => false,
        }
    }

    /// Soll die End-Beschränkung gezeigt werden.
    fn end_beschränkung(self) -> bool {
        match self {
            Beschränkung::Ende | Beschränkung::Alle => true,
            Beschränkung::Keine => false,
        }
    }
}

/// Zeichne die Kontur einer [`Kurve`].
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
            zeichne_internal::<P, A>(spurweite, builder, radius, winkel, beschränkungen);
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

/// Hilfs-Funktion für [`zeichne`], parametrisiert über beliebiges invertieren der Achsen.
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
    let spurweite_skalar = spurweite.als_skalar();
    let beschränkung = spurweite.beschränkung();
    let winkel_anfang = Winkel(3. * PI / 2.);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let winkel_ende = winkel_anfang + winkel;
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: beschränkung };
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_außen = radius_begrenzung_außen - spurweite.abstand();
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_innen = radius_außen - spurweite_skalar;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let begrenzung0 = gleis_links_oben
        + radius_begrenzung_außen * Vektor { x: winkel.sin(), y: (Skalar(1.) - winkel.cos()) };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let begrenzung1 = begrenzung0 + beschränkung * Vektor { x: -winkel.sin(), y: winkel.cos() };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
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

/// Pfad für die Kontur des Kontaktes einer [`Kurve`].
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

/// Hilfs-Funktion für [`zeichne_kontakt`], parametrisiert über beliebiges invertieren der Achsen.
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
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let kontakt_radius = (Skalar(0.5) * spurweite.abstand())
        .min(&(Skalar(0.25) * radius_begrenzung_außen * Skalar(winkel.0)));
    let anzeige_winkel = Winkel(3. * kontakt_radius.0 / radius_begrenzung_außen.0);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let zentrum = gleis_links_oben
        + radius_begrenzung_außen
            * Vektor { x: anzeige_winkel.sin(), y: (Skalar(1.) - anzeige_winkel.cos()) };
    // Kontakt
    erbauer.arc(
        Bogen { zentrum, radius: kontakt_radius, anfang: winkel::ZERO, ende: winkel::TAU }.into(),
    );
}

/// Pfad für den Hintergrund einer [`Kurve`].
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

/// Hilfs-Funktion für [`fülle`], parametrisiert über beliebiges invertieren der Achsen.
///
/// Geplant für [`canvas::PathType::EvenOdd`].
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
    let winkel_anfang = Winkel(3. * PI / 2.);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let winkel_ende = winkel_anfang + winkel;
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_außen = radius_begrenzung_außen - abstand;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_innen = radius_außen - spurweite_skalar;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let bogen_zentrum =
        beschränkung_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Koordinaten links
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let gleis_links_oben = beschränkung_links_oben + Vektor { x: Skalar(0.), y: abstand };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: spurweite_skalar };
    // Koordinaten rechts
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let gleis_rechts_oben: Vektor = gleis_links_oben
        + radius_außen * Vektor { x: winkel.sin(), y: (Skalar(1.) - winkel.cos()) };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
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

/// Pfad für den Hintergrund des Kontaktes einer [`Kurve`].
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

/// Hilfs-Funktion für [`fülle_kontakt`], parametrisiert über beliebiges invertieren der Achsen.
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
    let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(radius);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_kontakt = (Skalar(0.5) * spurweite.abstand())
        .min(&(Skalar(0.25) * radius_begrenzung_außen * Skalar(winkel.0)));
    let anzeige_winkel = Winkel(3. * radius_kontakt.0 / radius_begrenzung_außen.0);
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let zentrum = gleis_links_oben
        + radius_begrenzung_außen
            * Vektor { x: anzeige_winkel.sin(), y: (Skalar(1.) - anzeige_winkel.cos()) };
    // Kontakt
    erbauer.arc(
        Bogen { zentrum, radius: radius_kontakt, anfang: winkel::ZERO, ende: winkel::TAU }.into(),
    );
    // Anzeigefarbe
    match (level, trigger) {
        (Level::Low, Trigger::RisingEdge) | (Level::High, Trigger::FallingEdge) => farbe::ROT,
        (Level::High, Trigger::RisingEdge) | (Level::Low, Trigger::FallingEdge) => farbe::GRÜN,
        (Level::Low, _trigger) => farbe::BLAU,
        (Level::High, _trigger) => farbe::GRÜN,
    }
}

/// Ist die `relative_position` innerhalb der Kontur einer [`Kurve`].
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
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_außen = radius_begrenzung_außen - abstand;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_innen = radius_außen - spurweite_skalar;
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let bogen_zentrum = Vektor { x: Skalar(0.), y: abstand + radius_außen };
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    let radius_vector = bogen_zentrum - relative_position;
    let länge = radius_vector.länge();
    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
    #[allow(clippy::arithmetic_side_effects)]
    if (länge + ungenauigkeit > radius_innen) && (länge - ungenauigkeit < radius_außen) {
        let acos = Winkel::acos(radius_vector.y / länge);
        let mut test_winkel: Winkel = if radius_vector.x > Skalar(0.) { -acos } else { acos };
        // normalisiere winkel
        while test_winkel < winkel::ZERO {
            test_winkel += winkel::TAU;
        }
        if test_winkel < winkel {
            return true;
        }
    }
    false
}
