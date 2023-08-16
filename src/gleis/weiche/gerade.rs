//! Definition und zeichnen einer [Weiche].

use std::fmt::Debug;

use serde::{Deserialize, Serialize};
use zugkontrolle_macros::{alias_serialisiert_unit, erstelle_richtung};

use crate::{
    gleis::{gerade, kurve, verbindung::Verbindung},
    nachschlagen::impl_nachschlagen,
    steuerung::{self, weiche::MitRichtung},
    typen::{
        canvas::{
            pfad::{self, Pfad, Transformation},
            Position,
        },
        mm::{Länge, Radius, Spurweite},
        rechteck::Rechteck,
        skalar::Skalar,
        vektor::Vektor,
        winkel::{self, Trigonometrie, Winkel},
        MitName, Transparenz, Zeichnen,
    },
};

type AnschlüsseSerialisiert =
    steuerung::weiche::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;
type Anschlüsse = steuerung::weiche::Weiche<Richtung, RichtungAnschlüsse>;

/// Definition einer Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[alias_serialisiert_unit(AnschlüsseSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Weiche<Anschlüsse = Option<self::Anschlüsse>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Radius der Kurve.
    pub radius: Skalar,
    /// Der Winkel der Kurve.
    pub winkel: Winkel,
    /// Die Orientierung der Weiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der Weiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der Weiche.
    pub steuerung: Anschlüsse,
}

impl WeicheUnit {
    /// Erstelle eine neue [Weiche].
    pub const fn neu(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        orientierung: Orientierung,
    ) -> Self {
        WeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: None,
            steuerung: (),
        }
    }

    /// Erstelle eine neue [Weiche] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        orientierung: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        WeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            orientierung,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

/// Die Orientierung einer [Weiche], in welche Richtung geht die Kurve.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientierung {
    /// Die Kurve geht nach links.
    Links,
    /// Die Kurve geht nach rechts.
    Rechts,
}

#[erstelle_richtung]
#[impl_nachschlagen(Verbindung, Verbindungen, Debug, Clone)]
/// [Verbindungen](Verbindung) einer [Weiche].
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    /// Das Ende an dem sich Gerade und Kurve treffen.
    Anfang,
    /// Das andere Ende der Gerade.
    Gerade,
    /// Das andere Ende der Kurve.
    Kurve,
}

impl<Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for Weiche<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, spurweite: Spurweite) -> Rechteck {
        let Weiche { länge, radius, winkel, .. } = *self;
        let rechteck_gerade = gerade::rechteck(spurweite, länge);
        let rechteck_kurve = kurve::rechteck(spurweite, radius, winkel);
        rechteck_gerade.einschließend(rechteck_kurve)
    }

    fn zeichne(&self, spurweite: Spurweite) -> Vec<Pfad> {
        let Weiche { länge, radius, winkel, orientierung, .. } = *self;
        if orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(spurweite).ecke_max();
            let transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            vec![
                gerade::zeichne(
                    spurweite,
                    länge,
                    true,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                kurve::zeichne(
                    spurweite,
                    radius,
                    winkel,
                    kurve::Beschränkung::Ende,
                    transformations,
                    pfad::Erbauer::with_invert_y,
                ),
            ]
        } else {
            vec![
                gerade::zeichne(
                    spurweite,
                    länge,
                    true,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
                kurve::zeichne(
                    spurweite,
                    radius,
                    winkel,
                    kurve::Beschränkung::Ende,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
            ]
        }
    }

    fn fülle(&self, spurweite: Spurweite) -> Vec<(Pfad, Transparenz)> {
        let Weiche { länge, radius, winkel, orientierung, .. } = *self;
        let (gerade_transparenz, kurve_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        if orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(spurweite).ecke_max();
            let transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            vec![
                (
                    gerade::fülle(
                        spurweite,
                        länge,
                        transformations.clone(),
                        pfad::Erbauer::with_invert_y,
                    ),
                    gerade_transparenz,
                ),
                (
                    kurve::fülle(
                        spurweite,
                        radius,
                        winkel,
                        transformations,
                        pfad::Erbauer::with_invert_y,
                    ),
                    kurve_transparenz,
                ),
            ]
        } else {
            vec![
                (
                    gerade::fülle(spurweite, länge, Vec::new(), pfad::Erbauer::with_normal_axis),
                    gerade_transparenz,
                ),
                (
                    kurve::fülle(
                        spurweite,
                        radius,
                        winkel,
                        Vec::new(),
                        pfad::Erbauer::with_normal_axis,
                    ),
                    kurve_transparenz,
                ),
            ]
        }
    }

    fn beschreibung_und_name(
        &self,
        spurweite: Spurweite,
    ) -> (Position, Option<&str>, Option<&str>) {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                let size: Vektor = self.rechteck(spurweite).ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            },
        };
        (
            Position {
                punkt: Vektor {
                    x: self.länge.halbiert(),
                    y: start_height + multiplier * spurweite.beschränkung().halbiert(),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref().map(String::as_str),
            self.steuerung.name(),
        )
    }

    fn innerhalb(
        &self,
        spurweite: Spurweite,
        relative_position: Vektor,
        ungenauigkeit: Skalar,
    ) -> bool {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                let size: Vektor = self.rechteck(spurweite).ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            },
        };
        let start = Vektor { x: Skalar(0.), y: start_height };
        // sub-checks
        let mut relative_vector = relative_position - start;
        relative_vector.y *= multiplier;
        gerade::innerhalb(spurweite, self.länge, relative_vector, ungenauigkeit)
            || kurve::innerhalb(spurweite, self.radius, self.winkel, relative_vector, ungenauigkeit)
    }

    fn verbindungen(&self, spurweite: Spurweite) -> Self::Verbindungen {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                start_height = self.rechteck(spurweite).ecke_max().y;
                multiplier = Skalar(-1.);
            },
        };
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        Verbindungen {
            anfang: Verbindung { position: anfang, richtung: winkel::PI },
            gerade: Verbindung {
                position: anfang + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            kurve: Verbindung {
                position: anfang
                    + Vektor {
                        x: self.winkel.sin() * self.radius,
                        y: multiplier * self.radius * (Skalar(1.) - self.winkel.cos()),
                    },
                richtung: multiplier.0 * self.winkel,
            },
        }
    }
}
