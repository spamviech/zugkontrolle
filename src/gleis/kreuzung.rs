//! Definition und zeichnen einer Kreuzung

use std::fmt::Debug;

use serde::{Deserialize, Serialize};
use zugkontrolle_macros::alias_serialisiert_unit;

pub use crate::gleis::weiche::gerade::{
    Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert,
};
use crate::{
    gleis::{gerade, kurve, verbindung::Verbindung},
    nachschlagen::impl_nachschlagen,
    steuerung,
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
        MitName, MitRichtung, Transparenz, Zeichnen,
    },
};

/// Definition einer Kreuzung
#[alias_serialisiert_unit(steuerung::weiche::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Kreuzung
<Anschlüsse = Option<steuerung::weiche::Weiche<Richtung, RichtungAnschlüsse>>> {
    pub länge: Skalar,
    pub radius: Skalar,
    pub variante: Variante,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    MitKurve,
    OhneKurve,
}

impl KreuzungUnit {
    pub const fn neu(länge: Länge, radius: Radius, variante: Variante) -> Self {
        KreuzungUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: None,
            steuerung: (),
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        variante: Variante,
        beschreibung: impl Into<String>,
    ) -> Self {
        KreuzungUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

impl<Anschlüsse> Kreuzung<Anschlüsse> {
    fn winkel(&self) -> Winkel {
        // winkel solves the formula `x = L/2 * (1 + sin(alpha)) = R * cos(alpha)`
        // https://www.wolframalpha.com/input/?i=sin%28alpha%29-C*cos%28alpha%29%3DC
        // länge=0 gives winkel=0, but is not properly defined,
        // since it violates the formula above (pi/2 required)
        // pi/2 doesn't work either, since it violates the formula
        // `y = L/2 * sin(alpha) = R * (1 - cos(alpha))`
        // only for radius=0 as well both formulas are satisfied by any winkel
        Winkel(2. * (0.5 * (self.länge / self.radius).0).atan())
    }
}

#[impl_nachschlagen(Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang0,
    Ende0,
    Anfang1,
    Ende1,
}

impl<Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for Kreuzung<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, spurweite: Spurweite) -> Rechteck {
        let winkel = self.winkel();
        let rechteck_kurve = kurve::rechteck(spurweite, self.radius, winkel);
        let rechteck_gerade = gerade::rechteck(spurweite, self.länge);
        let beschränkung = spurweite.beschränkung();
        let höhe_verschoben = rechteck_kurve.ecke_max().y - beschränkung;
        let verschieben = Vektor { x: Skalar(0.), y: höhe_verschoben };
        let rechteck_gerade_verschoben = rechteck_gerade.clone().verschiebe_chain(&verschieben);
        let gerade_zentrum = Skalar(0.5) * rechteck_gerade.ecke_max();
        let rechteck_gerade_gedreht = rechteck_gerade
            .verschiebe_chain(&-gerade_zentrum)
            .respektiere_rotation_chain(&winkel)
            .verschiebe_chain(&gerade_zentrum);
        let rechteck_geraden = rechteck_gerade_verschoben.einschließend(rechteck_gerade_gedreht);
        if self.variante == Variante::MitKurve {
            let rechteck_kurve_verschoben = rechteck_kurve.clone().verschiebe_chain(&verschieben);
            let rechteck_kurve_gespiegelt =
                Rechteck { ecke_a: -rechteck_kurve.ecke_a, ecke_b: -rechteck_kurve.ecke_b }
                    .verschiebe_chain(&Vektor { x: self.länge, y: rechteck_kurve.ecke_max().y });
            rechteck_geraden
                .einschließend(rechteck_kurve_verschoben)
                .einschließend(rechteck_kurve_gespiegelt)
        } else {
            rechteck_geraden
        }
    }

    fn zeichne(&self, spurweite: Spurweite) -> Vec<Pfad> {
        // utility sizes
        let Vektor { x: width, y: height } = self.rechteck(spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - spurweite.beschränkung().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor { x: start.x, y: -start.y };
        let zentrum_invert_y = Vektor { x: zentrum.x, y: -zentrum.y };
        let winkel = self.winkel();
        let mut paths = Vec::new();
        // Transformationen
        let horizontal_transformations = vec![Transformation::Translation(start)];
        let gedreht_transformations = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformations with assumed inverted y-Axis
            Transformation::Translation(-zentrum_invert_y),
            Transformation::Translation(start_invert_y),
        ];
        // Geraden
        paths.push(gerade::zeichne(
            spurweite,
            self.länge,
            true,
            horizontal_transformations.clone(),
            pfad::Erbauer::with_normal_axis,
        ));
        paths.push(gerade::zeichne(
            spurweite,
            self.länge,
            true,
            gedreht_transformations.clone(),
            pfad::Erbauer::with_invert_y,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                horizontal_transformations,
                pfad::Erbauer::with_normal_axis,
            ));
            paths.push(kurve::zeichne(
                spurweite,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                gedreht_transformations,
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self, spurweite: Spurweite) -> Vec<(Pfad, Transparenz)> {
        // utility sizes
        let Vektor { x: width, y: height } = self.rechteck(spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - spurweite.beschränkung().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor { x: start.x, y: -start.y };
        let zentrum_invert_y = Vektor { x: zentrum.x, y: -zentrum.y };
        let winkel = self.winkel();
        let mut paths = Vec::new();
        // Transformationen
        let horizontal_transformations = vec![Transformation::Translation(start)];
        let gedreht_transformations = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformations with assumed inverted y-Axis
            Transformation::Translation(-zentrum_invert_y),
            Transformation::Translation(start_invert_y),
        ];
        let (gerade_transparenz, kurve_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        // Geraden
        paths.push((
            gerade::fülle(
                spurweite,
                self.länge,
                horizontal_transformations.clone(),
                pfad::Erbauer::with_normal_axis,
            ),
            gerade_transparenz,
        ));
        paths.push((
            gerade::fülle(
                spurweite,
                self.länge,
                gedreht_transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ),
            gerade_transparenz,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    winkel,
                    horizontal_transformations,
                    pfad::Erbauer::with_normal_axis,
                ),
                kurve_transparenz,
            ));
            paths.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    winkel,
                    gedreht_transformations,
                    pfad::Erbauer::with_invert_y,
                ),
                kurve_transparenz,
            ));
        }
        // return value
        paths
    }

    fn beschreibung_und_name(
        &self,
        spurweite: Spurweite,
    ) -> (Position, Option<&String>, Option<&String>) {
        // utility sizes
        let size: Vektor = self.rechteck(spurweite).ecke_max();
        let half_height = size.y.halbiert();
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
        (
            Position {
                punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
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
        let Vektor { x: width, y: height } = self.rechteck(spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor { x: Skalar(0.), y: half_height - spurweite.beschränkung().halbiert() };
        let zentrum = Vektor { x: half_width, y: half_height };
        let winkel = self.winkel();
        // sub-checks
        let horizontal_vector = relative_position - start;
        let mut gedreht_vector = (relative_position - zentrum).rotiert(-winkel);
        gedreht_vector.y = -gedreht_vector.y;
        gedreht_vector += zentrum - start;
        gerade::innerhalb(spurweite, self.länge, horizontal_vector, ungenauigkeit)
            || gerade::innerhalb(spurweite, self.länge, gedreht_vector, ungenauigkeit)
            || (self.variante == Variante::MitKurve
                && (kurve::innerhalb(
                    spurweite,
                    self.radius,
                    winkel,
                    horizontal_vector,
                    ungenauigkeit,
                ) || kurve::innerhalb(
                    spurweite,
                    self.radius,
                    winkel,
                    gedreht_vector,
                    ungenauigkeit,
                )))
    }

    fn verbindungen(&self, spurweite: Spurweite) -> Self::Verbindungen {
        let Vektor { x: _, y: height } = self.rechteck(spurweite).ecke_max();
        let half_height = height.halbiert();
        let anfang0 = Vektor { x: Skalar(0.), y: half_height };
        let ende0 = anfang0 + Vektor { x: self.länge, y: Skalar(0.) };
        let winkel = self.winkel();
        let kurve = self.radius * Vektor { x: winkel.sin(), y: Skalar(1.) - winkel.cos() };
        let anfang1 = ende0 - kurve;
        let ende1 = anfang0 + kurve;
        Verbindungen {
            anfang_0: Verbindung { position: anfang0, richtung: winkel::PI },
            ende_0: Verbindung { position: ende0, richtung: winkel::ZERO },
            anfang_1: Verbindung { position: anfang1, richtung: winkel::PI + winkel },
            ende_1: Verbindung { position: ende1, richtung: winkel },
        }
    }
}
