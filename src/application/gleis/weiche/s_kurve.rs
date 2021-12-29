//! Definition und zeichnen einer Weiche

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::alias_serialisiert_unit;

pub use crate::application::gleis::weiche::gerade::{
    Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert,
};
use crate::{
    application::{
        gleis::{
            gerade, kurve,
            verbindung::Verbindung,
            weiche::gerade::{Orientierung, VerbindungName, Verbindungen},
        },
        typen::*,
    },
    steuerung,
};

/// Definition einer Weiche mit S-Kurve
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
/// Zeichnen::height berücksichtigt nur positive y-Werte.
#[alias_serialisiert_unit(steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SKurvenWeiche<Anschlüsse = Option<steuerung::Weiche<Richtung, RichtungAnschlüsse>>> {
    pub länge: Skalar,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub radius_reverse: Skalar,
    pub winkel_reverse: Winkel,
    pub orientierung: Orientierung,
    pub beschreibung: Option<String>,
    pub steuerung: Anschlüsse,
}

impl SKurvenWeicheUnit {
    pub const fn neu(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_reverse: Radius,
        angle_reverse: Winkel,
        direction: Orientierung,
    ) -> Self {
        SKurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            radius_reverse: radius_reverse.als_skalar(),
            winkel_reverse: angle_reverse,
            orientierung: direction,
            beschreibung: None,
            steuerung: (),
        }
    }

    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_reverse: Radius,
        angle_reverse: Winkel,
        direction: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        SKurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            radius_reverse: radius_reverse.als_skalar(),
            winkel_reverse: angle_reverse,
            orientierung: direction,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

impl<Anschlüsse: MitName + MitRichtung<Richtung>> Zeichnen for SKurvenWeiche<Anschlüsse> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self) -> Rechteck {
        let SKurvenWeiche { länge, radius, winkel, radius_reverse, winkel_reverse, .. } = *self;
        let rechteck_gerade = gerade::rechteck(länge);
        let rechteck_kurve = kurve::rechteck(radius, winkel);
        let rechteck_kurve_reverse = kurve::rechteck(radius_reverse, winkel_reverse);
        let radius_außen = radius_begrenzung_außen(radius);
        let radius_innen = radius_begrenzung_innen(radius);
        let winkel_sin = winkel.sin();
        let eins_minus_winkel_cos = Skalar(1.) - winkel.cos();
        let verschieben = Vektor {
            x: (winkel_sin * radius_außen).min(&(winkel_sin * radius_innen)),
            y: (eins_minus_winkel_cos * radius_außen).min(&(eins_minus_winkel_cos * radius_innen)),
        };
        let rechteck_kurve_reverse_verschoben = rechteck_kurve_reverse
            .respektiere_rotation_chain(&winkel)
            .verschiebe_chain(&verschieben);
        rechteck_gerade
            .einschließend(rechteck_kurve)
            .einschließend(rechteck_kurve_reverse_verschoben)
    }

    fn zeichne(&self) -> Vec<Pfad> {
        // utility sizes
        let radius_begrenzung_außen = radius_begrenzung_außen(self.radius);
        let s_kurve_transformations = |multiplier: Skalar| {
            let winkel = multiplier.0 * self.winkel;
            vec![
                Transformation::Translation(Vektor {
                    x: multiplier * radius_begrenzung_außen * winkel.sin(),
                    y: multiplier * radius_begrenzung_außen * (Skalar(1.) - winkel.cos()),
                }),
                Transformation::Rotation(winkel),
                Transformation::Translation(Vektor {
                    x: Skalar(0.),
                    y: multiplier * beschränkung(),
                }),
            ]
        };
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck().ecke_max();
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                true,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Keine,
                transformations.clone(),
                pfad::Erbauer::with_invert_y,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations(Skalar(-1.)));
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschränkung::Ende,
                transformations,
                pfad::Erbauer::with_normal_axis,
            ));
        } else {
            // Gerade
            paths.push(gerade::zeichne(
                self.zugtyp,
                self.länge,
                true,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach außen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius,
                self.winkel,
                kurve::Beschränkung::Keine,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ));
            // Kurve nach innen
            paths.push(kurve::zeichne(
                self.zugtyp,
                self.radius_reverse,
                self.winkel_reverse,
                kurve::Beschränkung::Ende,
                s_kurve_transformations(Skalar(1.)),
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        paths
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
        // utility sizes
        let radius_begrenzung_außen = radius_begrenzung_außen(self.radius);
        let s_kurve_transformations = |multiplier: Skalar| {
            let winkel = multiplier.0 * self.winkel;
            vec![
                Transformation::Translation(Vektor {
                    x: multiplier * radius_begrenzung_außen * winkel.sin(),
                    y: multiplier * radius_begrenzung_außen * (Skalar(1.) - winkel.cos()),
                }),
                Transformation::Rotation(winkel),
                Transformation::Translation(Vektor {
                    x: Skalar(0.),
                    y: multiplier * beschränkung(),
                }),
            ]
        };
        let (gerade_transparenz, kurve_transparenz) = match self.steuerung.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        // Zeichne Pfad
        let mut paths = Vec::new();
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck().ecke_max();
            let mut transformations =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            // Gerade
            paths.push((
                gerade::fülle(
                    self.zugtyp,
                    self.länge,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                gerade_transparenz,
            ));
            // Kurve nach außen
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius,
                    self.winkel,
                    transformations.clone(),
                    pfad::Erbauer::with_invert_y,
                ),
                kurve_transparenz,
            ));
            // Kurve nach innen
            transformations.extend(s_kurve_transformations(Skalar(-1.)));
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius_reverse,
                    self.winkel_reverse,
                    transformations,
                    pfad::Erbauer::with_normal_axis,
                ),
                kurve_transparenz,
            ));
        } else {
            // Gerade
            paths.push((
                gerade::fülle(
                    self.zugtyp,
                    self.länge,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
                gerade_transparenz,
            ));
            // Kurve nach außen
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius,
                    self.winkel,
                    Vec::new(),
                    pfad::Erbauer::with_normal_axis,
                ),
                kurve_transparenz,
            ));
            // Kurve nach innen
            paths.push((
                kurve::fülle(
                    self.zugtyp,
                    self.radius_reverse,
                    self.winkel_reverse,
                    s_kurve_transformations(Skalar(1.)),
                    pfad::Erbauer::with_invert_y,
                ),
                kurve_transparenz,
            ));
        }
        // return value
        paths
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                let size: Vektor = self.rechteck().ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            }
        };
        (
            Position {
                punkt: Vektor {
                    x: self.länge.halbiert(),
                    y: start_height + multiplier * beschränkung().halbiert(),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.steuerung.name(),
        )
    }

    fn innerhalb(&self, relative_position: Vektor, ungenauigkeit: Skalar) -> bool {
        // utility sizes
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                let size: Vektor = self.rechteck().ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            }
        };
        let start_vector = Vektor { x: Skalar(0.), y: start_height };
        let radius_begrenzung_außen = radius_begrenzung_außen(self.radius);
        let multiplied_winkel = multiplier.0 * self.winkel;
        let s_kurve_start_vector = Vektor {
            x: multiplier * radius_begrenzung_außen * multiplied_winkel.sin(),
            y: radius_begrenzung_außen * (Skalar(1.) - multiplied_winkel.cos()),
        };
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.y *= multiplier;
        let mut s_kurve_vector = (relative_vector - s_kurve_start_vector).rotiert(-self.winkel);
        s_kurve_vector -= Vektor { x: Skalar(0.), y: beschränkung() };
        s_kurve_vector.y = -s_kurve_vector.y;
        gerade::innerhalb(self.länge, relative_vector, ungenauigkeit)
            || kurve::innerhalb(self.radius, self.winkel, relative_vector, ungenauigkeit)
            || kurve::innerhalb(
                self.radius_reverse,
                self.winkel_reverse,
                s_kurve_vector,
                ungenauigkeit,
            )
    }

    fn verbindungen(&self) -> Self::Verbindungen {
        let start_height: Skalar;
        let multiplier: Skalar;
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            }
            Orientierung::Links => {
                start_height = self.rechteck().ecke_max().y;
                multiplier = Skalar(-1.);
            }
        };
        let angle_difference = self.winkel - self.winkel_reverse;
        let anfang =
            Vektor { x: Skalar(0.), y: start_height + multiplier * beschränkung().halbiert() };
        Verbindungen {
            anfang: Verbindung { position: anfang, richtung: winkel::PI },
            gerade: Verbindung {
                position: anfang + Vektor { x: self.länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            kurve: Verbindung {
                position: anfang
                    + Vektor {
                        x: self.radius * self.winkel.sin()
                            + self.radius_reverse * (self.winkel.sin() - angle_difference.sin()),
                        y: multiplier
                            * (self.radius * (Skalar(1.) - self.winkel.cos())
                                + self.radius_reverse
                                    * (angle_difference.cos() - self.winkel.cos())),
                    },
                richtung: multiplier.0 * angle_difference,
            },
        }
    }
}
