//! Definition und zeichnen einer Weiche

use std::fmt::Debug;

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::{alias_serialisiert_unit, create_richtung};

use crate::{
    application::{
        gleis::{gerade, kurve, verbindung::Verbindung, weiche::gerade::Orientierung},
        typen::*,
    },
    nachschlagen::impl_nachschlagen,
    steuerung,
};

/// Definition einer Kurven-Weiche
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[alias_serialisiert_unit(steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KurvenWeiche<Anschlüsse = Option<steuerung::Weiche<Richtung, RichtungAnschlüsse>>> {
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub orientierung: Orientierung,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}

impl KurvenWeicheUnit {
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
#[create_richtung]
#[impl_nachschlagen(Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang,
    Innen,
    Außen,
}

impl<Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for KurvenWeiche<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, spurweite: Spurweite) -> Rechteck {
        let KurvenWeiche { länge, radius, winkel, .. } = *self;
        let rechteck_gerade = gerade::rechteck(spurweite, länge);
        let rechteck_kurve = kurve::rechteck(spurweite, radius, winkel);
        let rechteck_kurve_verschoben =
            rechteck_kurve.clone().verschiebe_chain(&Vektor { x: länge, y: Skalar(0.) });
        rechteck_gerade.einschließend(rechteck_kurve).einschließend(rechteck_kurve_verschoben)
    }

    fn zeichne(&self, spurweite: Spurweite) -> Vec<Pfad> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(spurweite).ecke_max();
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            // Innere Kurve
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                spurweite,
                self.länge,
                false,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                transformations,
                pfad::Erbauer::with_invert_y,
            ));
        } else {
            // Innere Kurve
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Alle,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Gerade vor äußerer Kurve
            paths.push(gerade::zeichne(
                spurweite,
                self.länge,
                false,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Äußere Kurve
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Ende,
                vec![außen_transformation],
                pfad::Erbauer::with_normal_axis,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self, spurweite: Spurweite) -> Vec<(Pfad, Transparenz)> {
        // utility sizes
        let außen_transformation =
            Transformation::Translation(Vektor { x: self.länge, y: Skalar(0.) });
        let (innen_transparenz, außen_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Innen) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Außen) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(spurweite).ecke_max();
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            // Innere Kurve
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    self.winkel,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                innen_transparenz,
            ));
            // Gerade vor äußerer Kurve
            paths.push((
                gerade::fülle(
                    spurweite,
                    self.länge,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                außen_transparenz,
            ));
            // Äußere Kurve
            transformations.push(außen_transformation);
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    self.winkel,
                    transformations,
                    pfad::Erbauer::with_invert_y,
                ),
                außen_transparenz,
            ));
        } else {
            // Innere Kurve
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    self.winkel,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
                innen_transparenz,
            ));
            // Gerade vor äußerer Kurve
            paths.push((
                gerade::fülle(spurweite, self.länge, Vec::new(), pfad::Erbauer::with_normal_axis),
                außen_transparenz,
            ));
            // Äußere Kurve
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    self.winkel,
                    vec![außen_transformation],
                    pfad::Erbauer::with_normal_axis,
                ),
                außen_transparenz,
            ));
        }
        // return value
        paths
    }

    fn beschreibung_und_name(
        &self,
        spurweite: Spurweite,
    ) -> (Position, Option<&String>, Option<&String>) {
        let start_height: Skalar;
        let multiplier: Skalar;
        let size: Vektor = self.rechteck(spurweite).ecke_max();
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
                    y: start_height + multiplier * spurweite.beschränkung().halbiert(),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
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
        let start_vector = Vektor { x: Skalar(0.), y: start_height };
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.y *= multiplier;
        let verschoben_vector = relative_vector - Vektor { x: self.länge, y: Skalar(0.) };
        gerade::innerhalb(spurweite, self.länge, relative_vector, ungenauigkeit)
            || kurve::innerhalb(spurweite, self.radius, self.winkel, relative_vector, ungenauigkeit)
            || kurve::innerhalb(
                spurweite,
                self.radius,
                self.winkel,
                verschoben_vector,
                ungenauigkeit,
            )
    }

    fn verbindungen(&self, spurweite: Spurweite) -> Self::Verbindungen {
        // utility sizes
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
        let halbe_beschränkung: Skalar = spurweite.beschränkung().halbiert();
        let anfang = Vektor { x: Skalar(0.), y: start_height + multiplier * halbe_beschränkung };
        let innen = anfang
            + Vektor {
                x: self.radius * self.winkel.sin(),
                y: multiplier * self.radius * (Skalar(1.) - self.winkel.cos()),
            };
        Verbindungen {
            anfang: Verbindung { position: anfang, richtung: winkel::PI },
            innen: Verbindung { position: innen, richtung: multiplier.0 * self.winkel },
            außen: Verbindung {
                position: innen + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: multiplier.0 * self.winkel,
            },
        }
    }
}
