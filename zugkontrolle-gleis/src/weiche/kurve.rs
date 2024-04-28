//! Definition und zeichnen einer [`KurvenWeiche`].

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use zugkontrolle_macros::{alias_serialisiert_unit, erstelle_richtung};
use zugkontrolle_typen::{
    canvas::{
        pfad::{self, Bogen, Pfad, Transformation},
        Position,
    },
    farbe::Farbe,
    mm::{Länge, Radius, Spurweite},
    nachschlagen::impl_nachschlagen,
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    verbindung::Verbindung,
    winkel::{self, Winkel},
    Innerhalb, MitName, Transparenz, Zeichnen,
};

use crate::{
    gerade, kurve,
    steuerung::{self, weiche::MitRichtung},
    weiche::orientierung::Orientierung,
};

/// Steuerung einer [`KurvenWeiche`].
type Steuerung = steuerung::weiche::Weiche<Richtung, RichtungAnschlüsse>;
/// Serialisierbare Darstellung der Steuerung einer [`KurvenWeiche`].
type AnschlüsseSerialisiert =
    steuerung::weiche::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Kurven-Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[alias_serialisiert_unit(AnschlüsseSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KurvenWeiche<Anschlüsse = Option<Steuerung>> {
    /// Die Länge der Geraden vor der äußeren Kurve.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Die Orientierung der KurvenWeiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der KurvenWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der KurvenWeiche.
    pub steuerung: Anschlüsse,
}

impl KurvenWeicheUnit {
    /// Erstelle eine neue [`KurvenWeiche`].
    #[must_use]
    pub const fn neu(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        orientierung: Orientierung,
    ) -> Self {
        KurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: None,
            steuerung: (),
        }
    }

    /// Erstelle eine neue [`KurvenWeiche`] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    #[must_use]
    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        orientierung: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        KurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}
#[erstelle_richtung]
#[impl_nachschlagen(Verbindung, Verbindungen, Debug, Clone)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
/// [Verbindungen](Verbindung) einer [`KurvenWeiche`].
pub enum VerbindungName {
    /// Das Ende, an dem sich beide Kurven treffen.
    Anfang,
    /// Das andere Ende der inneren Kurve.
    Innen,
    /// Das andere Ende der äußeren Kurve.
    Außen,
}

impl<Anschlüsse, Anschlüsse2: MitName + MitRichtung<Richtung>> Zeichnen<Anschlüsse2>
    for KurvenWeiche<Anschlüsse>
{
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        let KurvenWeiche { länge, radius, winkel, .. } = *self;
        let rechteck_gerade = gerade::rechteck(spurweite, länge);
        let rechteck_kurve = kurve::rechteck(spurweite, radius, winkel);
        let rechteck_kurve_verschoben =
            rechteck_kurve.clone().verschiebe_chain(&Vektor { x: länge, y: Skalar(0.) });
        rechteck_gerade.einschließend(&rechteck_kurve).einschließend(&rechteck_kurve_verschoben)
    }

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        let außen_transformation =
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        if self.orientierung == Orientierung::Links {
            let size = self.rechteck(anschlüsse, spurweite).ecke_max();
            let transformationen =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            zeichne(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                transformationen,
                außen_transformation,
                pfad::Erbauer::with_invert_y,
            )
        } else {
            zeichne(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                Vec::new(),
                außen_transformation,
                pfad::Erbauer::with_normal_axis,
            )
        }
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        let (innen_transparenz, außen_transparenz) = match anschlüsse.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Innen) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Außen) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        if self.orientierung == Orientierung::Links {
            let size = self.rechteck(anschlüsse, spurweite).ecke_max();
            let transformationen =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            fülle(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                innen_transparenz,
                außen_transparenz,
                transformationen,
                außen_transformation,
                pfad::Erbauer::with_invert_y,
            )
        } else {
            fülle(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                innen_transparenz,
                außen_transparenz,
                Vec::new(),
                außen_transformation,
                pfad::Erbauer::with_normal_axis,
            )
        }
    }

    fn beschreibung_und_name<'s, 't>(
        &'s self,
        anschlüsse: &'t Anschlüsse2,
        spurweite: Spurweite,
    ) -> (Position, Option<&'s str>, Option<&'t str>) {
        let start_height: Skalar;
        let multiplier: Skalar;
        let size = self.rechteck(anschlüsse, spurweite).ecke_max();
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                start_height = size.y;
                multiplier = Skalar(-1.);
            },
        };
        (
            Position {
                punkt: Vektor {
                    x: self.länge.min(&(size.y.halbiert())),
                    // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                    #[allow(clippy::arithmetic_side_effects)]
                    y: start_height + multiplier * spurweite.beschränkung().halbiert(),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_deref(),
            anschlüsse.name(),
        )
    }

    fn innerhalb(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> Innerhalb {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                let size = self.rechteck(anschlüsse, spurweite).ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            },
        };
        let start_vector = Vektor { x: Skalar(0.), y: start_height };
        // sub-checks
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let mut relative_vector = relative_position - start_vector;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            relative_vector.y *= multiplier;
        }
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let verschoben_vector = relative_vector - Vektor { x: self.länge, y: Skalar(0.) };
        gerade::innerhalb(spurweite, self.länge, relative_vector, ungenauigkeit)
            .oder(kurve::innerhalb(
                spurweite,
                self.radius,
                self.winkel,
                relative_vector,
                ungenauigkeit,
            ))
            .oder(kurve::innerhalb(
                spurweite,
                self.radius,
                self.winkel,
                verschoben_vector,
                ungenauigkeit,
            ))
    }

    fn verbindungen(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Self::Verbindungen {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                start_height = self.rechteck(anschlüsse, spurweite).ecke_max().y;
                multiplier = Skalar(-1.);
            },
        };
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let innen = anfang
            + Vektor {
                x: self.radius * self.winkel.sin(),
                y: multiplier * self.radius * (Skalar(1.) - self.winkel.cos()),
            };
        Verbindungen {
            anfang: Verbindung { position: anfang, richtung: winkel::PI },
            innen: Verbindung {
                position: innen,
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                richtung: multiplier.0 * self.winkel,
            },
            außen: Verbindung {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                position: innen + Vektor { x: self.länge, y: Skalar(0.) },
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                richtung: multiplier.0 * self.winkel,
            },
        }
    }
}

/// Pfade für die Kontur einer [`KurvenWeiche`].
fn zeichne<P, A>(
    spurweite: Spurweite,
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    mut transformationen: Vec<Transformation>,
    außen_transformation: Transformation,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Vec<Pfad>
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut pfade = Vec::new();
    // Innere Kurve
    pfade.push(kurve::zeichne(
        spurweite,
        radius,
        winkel,
        kurve::Beschränkung::Alle,
        transformationen.clone(),
        &mit_invertierter_achse,
    ));
    // Gerade vor äußerer Kurve
    pfade.push(gerade::zeichne(
        spurweite,
        länge,
        false,
        transformationen.clone(),
        &mit_invertierter_achse,
    ));
    // Äußere Kurve
    transformationen.push(außen_transformation);
    pfade.push(kurve::zeichne(
        spurweite,
        radius,
        winkel,
        kurve::Beschränkung::Ende,
        transformationen,
        mit_invertierter_achse,
    ));
    // Rückgabewert
    pfade
}

// Alle Argumente benötigt.
#[allow(clippy::too_many_arguments)]
/// Pfade für den Hintergrund einer [`KurvenWeiche`].
fn fülle<P, A>(
    spurweite: Spurweite,
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    innen_transparenz: Transparenz,
    außen_transparenz: Transparenz,
    mut transformationen: Vec<Transformation>,
    außen_transformation: Transformation,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Vec<(Pfad, Option<Farbe>, Transparenz)>
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut pfade = Vec::new();
    // Innere Kurve
    pfade.push((
        kurve::fülle(spurweite, radius, winkel, transformationen.clone(), &mit_invertierter_achse),
        None,
        innen_transparenz,
    ));
    // Gerade vor äußerer Kurve
    pfade.push((
        gerade::fülle(spurweite, länge, transformationen.clone(), &mit_invertierter_achse),
        None,
        außen_transparenz,
    ));
    // Äußere Kurve
    transformationen.push(außen_transformation);
    pfade.push((
        kurve::fülle(spurweite, radius, winkel, transformationen, mit_invertierter_achse),
        None,
        außen_transparenz,
    ));
    // Rückgabewert
    pfade
}
