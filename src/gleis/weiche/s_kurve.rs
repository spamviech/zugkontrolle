//! Definition und zeichnen einer [Weiche mit S-Kurve](SKurvenWeiche).

use serde::{Deserialize, Serialize};
use zugkontrolle_macros::alias_serialisiert_unit;

pub use crate::gleis::weiche::gerade::{
    Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert,
};
use crate::{
    gleis::{
        gerade, kurve,
        verbindung::Verbindung,
        weiche::{
            gerade::{VerbindungName, Verbindungen},
            orientierung::Orientierung,
        },
    },
    steuerung::{self, weiche::MitRichtung},
    typen::{
        canvas::{
            pfad::{self, Bogen, Pfad, Transformation},
            Position,
        },
        farbe::Farbe,
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
type Steuerung = steuerung::weiche::Weiche<Richtung, RichtungAnschlüsse>;

/// Definition einer Weiche mit S-Kurve.
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
#[alias_serialisiert_unit(AnschlüsseSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SKurvenWeiche<Anschlüsse = Option<Steuerung>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Radius der Kurve nach außen.
    pub radius: Skalar,
    /// Der Winkel der Kurve nach außen.
    pub winkel: Winkel,
    /// Der Radius der Kurve nach innen.
    pub radius_kurve_nach_innen: Skalar,
    /// Der Winkel der Kurve nach innen.
    pub winkel_kurve_nach_innen: Winkel,
    /// Die Orientierung der SKurvenWeiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der SKurvenWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der SKurvenWeiche.
    pub steuerung: Anschlüsse,
}

impl SKurvenWeicheUnit {
    /// Erstelle eine neue [SKurvenWeiche].
    pub const fn neu(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_kurve_nach_innen: Radius,
        winkel_kurve_nach_innen: Winkel,
        orientierung: Orientierung,
    ) -> Self {
        SKurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            radius_kurve_nach_innen: radius_kurve_nach_innen.als_skalar(),
            winkel_kurve_nach_innen,
            orientierung,
            beschreibung: None,
            steuerung: (),
        }
    }

    /// Erstelle eine neue [SKurvenWeiche] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        radius_kurve_nach_innen: Radius,
        winkel_kurve_nach_innen: Winkel,
        orientierung: Orientierung,
        beschreibung: impl Into<String>,
    ) -> Self {
        SKurvenWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            radius_kurve_nach_innen: radius_kurve_nach_innen.als_skalar(),
            winkel_kurve_nach_innen,
            orientierung,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

impl<Anschlüsse, Anschlüsse2: MitName + MitRichtung<Richtung>> Zeichnen<Anschlüsse2>
    for SKurvenWeiche<Anschlüsse>
{
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        let SKurvenWeiche {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            ..
        } = *self;
        let rechteck_gerade = gerade::rechteck(spurweite, länge);
        let rechteck_kurve = kurve::rechteck(spurweite, radius, winkel);
        let rechteck_kurve_reverse =
            kurve::rechteck(spurweite, radius_kurve_nach_innen, winkel_kurve_nach_innen);
        let radius_außen = spurweite.radius_begrenzung_außen(radius);
        let radius_innen = spurweite.radius_begrenzung_innen(radius);
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

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        // utility sizes
        let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(self.radius);
        let s_kurve_transformationen = |multiplier: Skalar| {
            let winkel = multiplier.0 * self.winkel;
            vec![
                Transformation::Translation(Vektor {
                    x: multiplier * radius_begrenzung_außen * winkel.sin(),
                    y: multiplier * radius_begrenzung_außen * (Skalar(1.) - winkel.cos()),
                }),
                Transformation::Rotation(winkel),
                Transformation::Translation(Vektor {
                    x: Skalar(0.),
                    y: multiplier * spurweite.beschränkung(),
                }),
            ]
        };
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(anschlüsse, spurweite).ecke_max();
            let transformationen =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            zeichne(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                self.radius_kurve_nach_innen,
                self.winkel_kurve_nach_innen,
                transformationen,
                s_kurve_transformationen(Skalar(-1.)),
                pfad::Erbauer::with_invert_y,
                pfad::Erbauer::with_normal_axis,
            )
        } else {
            zeichne(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                self.radius_kurve_nach_innen,
                self.winkel_kurve_nach_innen,
                Vec::new(),
                s_kurve_transformationen(Skalar(1.)),
                pfad::Erbauer::with_normal_axis,
                pfad::Erbauer::with_invert_y,
            )
        }
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        // utility sizes
        let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(self.radius);
        let s_kurve_transformationen = |multiplier: Skalar| {
            let winkel = multiplier.0 * self.winkel;
            vec![
                Transformation::Translation(Vektor {
                    x: multiplier * radius_begrenzung_außen * winkel.sin(),
                    y: multiplier * radius_begrenzung_außen * (Skalar(1.) - winkel.cos()),
                }),
                Transformation::Rotation(winkel),
                Transformation::Translation(Vektor {
                    x: Skalar(0.),
                    y: multiplier * spurweite.beschränkung(),
                }),
            ]
        };
        let (gerade_transparenz, kurve_transparenz) = match anschlüsse.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        if self.orientierung == Orientierung::Links {
            let size: Vektor = self.rechteck(anschlüsse, spurweite).ecke_max();
            let transformationen =
                vec![Transformation::Translation(Vektor { x: Skalar(0.), y: size.y })];
            fülle(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                self.radius_kurve_nach_innen,
                self.winkel_kurve_nach_innen,
                gerade_transparenz,
                kurve_transparenz,
                transformationen,
                s_kurve_transformationen(Skalar(-1.)),
                pfad::Erbauer::with_invert_y,
                pfad::Erbauer::with_normal_axis,
            )
        } else {
            fülle(
                spurweite,
                self.länge,
                self.radius,
                self.winkel,
                self.radius_kurve_nach_innen,
                self.winkel_kurve_nach_innen,
                gerade_transparenz,
                kurve_transparenz,
                Vec::new(),
                s_kurve_transformationen(Skalar(1.)),
                pfad::Erbauer::with_normal_axis,
                pfad::Erbauer::with_invert_y,
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
        match self.orientierung {
            Orientierung::Rechts => {
                start_height = Skalar(0.);
                multiplier = Skalar(1.);
            },
            Orientierung::Links => {
                let size: Vektor = self.rechteck(anschlüsse, spurweite).ecke_max();
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
            anschlüsse.name(),
        )
    }

    fn innerhalb(
        &self,
        anschlüsse: &Anschlüsse2,
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
                let size: Vektor = self.rechteck(anschlüsse, spurweite).ecke_max();
                start_height = size.y;
                multiplier = Skalar(-1.);
            },
        };
        let start_vector = Vektor { x: Skalar(0.), y: start_height };
        let radius_begrenzung_außen = spurweite.radius_begrenzung_außen(self.radius);
        let multiplied_winkel = multiplier.0 * self.winkel;
        let s_kurve_start_vector = Vektor {
            x: multiplier * radius_begrenzung_außen * multiplied_winkel.sin(),
            y: radius_begrenzung_außen * (Skalar(1.) - multiplied_winkel.cos()),
        };
        // sub-checks
        let mut relative_vector = relative_position - start_vector;
        relative_vector.y *= multiplier;
        let mut s_kurve_vector = (relative_vector - s_kurve_start_vector).rotiert(-self.winkel);
        s_kurve_vector -= Vektor { x: Skalar(0.), y: spurweite.beschränkung() };
        s_kurve_vector.y = -s_kurve_vector.y;
        gerade::innerhalb(spurweite, self.länge, relative_vector, ungenauigkeit)
            || kurve::innerhalb(spurweite, self.radius, self.winkel, relative_vector, ungenauigkeit)
            || kurve::innerhalb(
                spurweite,
                self.radius_kurve_nach_innen,
                self.winkel_kurve_nach_innen,
                s_kurve_vector,
                ungenauigkeit,
            )
    }

    fn verbindungen(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Self::Verbindungen {
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
        let angle_difference = self.winkel - self.winkel_kurve_nach_innen;
        let anfang = Vektor {
            x: Skalar(0.),
            y: start_height + multiplier * spurweite.beschränkung().halbiert(),
        };
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
                            + self.radius_kurve_nach_innen
                                * (self.winkel.sin() - angle_difference.sin()),
                        y: multiplier
                            * (self.radius * (Skalar(1.) - self.winkel.cos())
                                + self.radius_kurve_nach_innen
                                    * (angle_difference.cos() - self.winkel.cos())),
                    },
                richtung: multiplier.0 * angle_difference,
            },
        }
    }
}

fn zeichne<P, A, PInnen, AInnen>(
    spurweite: Spurweite,
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    radius_kurve_nach_innen: Skalar,
    winkel_kurve_nach_innen: Winkel,
    mut transformationen: Vec<Transformation>,
    s_kurve_transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
    mit_invertierter_achse_kurve_nach_innen: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<PInnen, AInnen>)>,
    ),
) -> Vec<Pfad>
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    PInnen: From<Vektor> + Into<Vektor>,
    AInnen: From<Bogen> + Into<Bogen>,
{
    let mut pfade = Vec::new();
    // Gerade
    pfade.push(gerade::zeichne(
        spurweite,
        länge,
        true,
        transformationen.clone(),
        &mit_invertierter_achse,
    ));
    // Kurve nach außen
    pfade.push(kurve::zeichne(
        spurweite,
        radius,
        winkel,
        kurve::Beschränkung::Keine,
        transformationen.clone(),
        mit_invertierter_achse,
    ));
    // Kurve nach innen
    transformationen.extend(s_kurve_transformationen);
    pfade.push(kurve::zeichne(
        spurweite,
        radius_kurve_nach_innen,
        winkel_kurve_nach_innen,
        kurve::Beschränkung::Ende,
        transformationen,
        mit_invertierter_achse_kurve_nach_innen,
    ));
    // Rückgabewert
    pfade
}

fn fülle<P, A, PInnen, AInnen>(
    spurweite: Spurweite,
    länge: Skalar,
    radius: Skalar,
    winkel: Winkel,
    radius_kurve_nach_innen: Skalar,
    winkel_kurve_nach_innen: Winkel,
    gerade_transparenz: Transparenz,
    kurve_transparenz: Transparenz,
    mut transformationen: Vec<Transformation>,
    s_kurve_transformationen: Vec<Transformation>,
    mit_invertierter_achse: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
    mit_invertierter_achse_kurve_nach_innen: impl Fn(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<PInnen, AInnen>)>,
    ),
) -> Vec<(Pfad, Option<Farbe>, Transparenz)>
where
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
    PInnen: From<Vektor> + Into<Vektor>,
    AInnen: From<Bogen> + Into<Bogen>,
{
    let mut pfade = Vec::new();
    // Gerade
    pfade.push((
        gerade::fülle(spurweite, länge, transformationen.clone(), &mit_invertierter_achse),
        None,
        gerade_transparenz,
    ));
    // Kurve nach außen
    pfade.push((
        kurve::fülle(spurweite, radius, winkel, transformationen.clone(), mit_invertierter_achse),
        None,
        kurve_transparenz,
    ));
    // Kurve nach innen
    transformationen.extend(s_kurve_transformationen);
    pfade.push((
        kurve::fülle(
            spurweite,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            transformationen,
            mit_invertierter_achse_kurve_nach_innen,
        ),
        None,
        kurve_transparenz,
    ));
    // Rückgabewert
    pfade
}
