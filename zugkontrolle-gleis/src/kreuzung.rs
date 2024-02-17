//! Definition und zeichnen einer [`Kreuzung`].

// Wiederverwenden von public Items [`Richtung`], [`RichtungAnschlüsse`], [`RichtungAnschlüsseSerialisiert`]
#![allow(clippy::pub_use)]

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use zugkontrolle_macros::{alias_serialisiert_unit, impl_nachschlagen};
use zugkontrolle_typen::{
    canvas::{
        pfad::{self, Pfad, Transformation},
        Position,
    },
    farbe::Farbe,
    mm::{Länge, Radius, Spurweite},
    rechteck::Rechteck,
    skalar::Skalar,
    vektor::Vektor,
    verbindung::Verbindung,
    winkel::{self, Trigonometrie, Winkel},
    MitName, Transparenz, Zeichnen,
};

use crate::{
    gerade, kurve,
    steuerung::{self, weiche::MitRichtung},
};

pub use crate::weiche::gerade::{Richtung, RichtungAnschlüsse, RichtungAnschlüsseSerialisiert};

/// Die Steuerung einer [`Kreuzung`].
type Steuerung = steuerung::weiche::Weiche<Richtung, RichtungAnschlüsse>;
/// Serialisierbare Darstellung der Steuerung einer [`Kreuzung`].
type AnschlüsseSerialisiert =
    steuerung::weiche::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Kreuzung.
#[alias_serialisiert_unit(AnschlüsseSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Kreuzung<Anschlüsse = Option<Steuerung>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Kurvenradius; legt automatisch den Winkel fest.
    pub radius: Skalar,
    /// Werden die Kurven gezeichnet, oder nur die Geraden.
    pub variante: Variante,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der Kreuzung.
    pub steuerung: Anschlüsse,
}

/// Werden die Kurven gezeichnet, oder nur die Geraden.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    /// Zeichne die Kurven und die Geraden.
    MitKurve,
    /// Zeichne nur die Geraden.
    OhneKurve,
}

impl KreuzungUnit {
    /// Erstelle eine neue [`Kreuzung`].
    #[must_use]
    pub const fn neu(länge: Länge, radius: Radius, variante: Variante) -> Self {
        KreuzungUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            variante,
            beschreibung: None,
            steuerung: (),
        }
    }

    /// Erstelle eine neue [`Kreuzung`] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    #[must_use]
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
    /// Berechne den Winkel der [`Kreuzung`], ausgehend von der Gleichung
    /// `x = L/2 * (1 + sin(alpha)) = R * cos(alpha)`.
    ///
    /// <https://www.wolframalpha.com/input/?i=sin%28alpha%29-C*cos%28alpha%29%3DC>
    /// `länge=0` gibt `winkel=0` zurück, ist aber nicht wohldefiniert,
    /// da es die obige Gleichung verletzt (benötigt winkel=pi/2).
    /// `winkel=pi/2` funktioniert ebenfalls nicht, da es die Gleichung
    /// `y = L/2 * sin(alpha) = R * (1 - cos(alpha))` verletzt.
    /// Nur wenn gleichzeitig `radius=0` gilt sind beide Gleichungen bei allen Winkeln erfüllt.
    pub fn winkel(&self) -> Winkel {
        // Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        Winkel(2. * (0.5 * (self.länge / self.radius).0).atan())
    }
}

#[impl_nachschlagen(Verbindung, Verbindungen, Debug)]
/// [Verbindungen](Verbindung) einer [`Kreuzung`].
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    /// Ein Ende der ersten Geraden.
    Anfang0,
    /// Das andere Ende der ersten Geraden.
    Ende0,
    /// Ein Ende der zweiten Geraden.
    Anfang1,
    /// Das andere Ende der zweiten Geraden.
    Ende1,
}

impl<Anschlüsse, Anschlüsse2: MitName + MitRichtung<Richtung>> Zeichnen<Anschlüsse2>
    for Kreuzung<Anschlüsse>
{
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        let winkel = self.winkel();
        let rechteck_kurve = kurve::rechteck(spurweite, self.radius, winkel);
        let rechteck_gerade = gerade::rechteck(spurweite, self.länge);
        let beschränkung = spurweite.beschränkung();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let höhe_verschoben = rechteck_kurve.ecke_max().y - beschränkung;
        let verschieben = Vektor { x: Skalar(0.), y: höhe_verschoben };
        let rechteck_gerade_verschoben = rechteck_gerade.clone().verschiebe_chain(&verschieben);
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let gerade_zentrum = Skalar(0.5) * rechteck_gerade.ecke_max();
        let rechteck_gerade_gedreht = rechteck_gerade
            .verschiebe_chain(
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                {
                    &-gerade_zentrum
                },
            )
            .respektiere_rotation_chain(&winkel)
            .verschiebe_chain(&gerade_zentrum);
        let rechteck_geraden = rechteck_gerade_verschoben.einschließend(rechteck_gerade_gedreht);
        if self.variante == Variante::MitKurve {
            let rechteck_kurve_verschoben = rechteck_kurve.clone().verschiebe_chain(&verschieben);
            let rechteck_kurve_gespiegelt = Rechteck {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                ecke_a: -rechteck_kurve.ecke_a,
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                ecke_b: -rechteck_kurve.ecke_b,
            }
            .verschiebe_chain(&Vektor { x: self.länge, y: rechteck_kurve.ecke_max().y });
            rechteck_geraden
                .einschließend(rechteck_kurve_verschoben)
                .einschließend(rechteck_kurve_gespiegelt)
        } else {
            rechteck_geraden
        }
    }

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        // utility sizes
        let Vektor { x: width, y: height } = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor {
            x: Skalar(0.),
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: half_height - spurweite.beschränkung().halbiert(),
        };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor {
            x: start.x,
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: -start.y,
        };
        let zentrum_invert_y = Vektor {
            x: zentrum.x,
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: -zentrum.y,
        };
        let winkel = self.winkel();
        let mut pfade = Vec::new();
        // Transformationen
        let horizontal_transformationen = vec![Transformation::Translation(start)];
        let gedreht_transformationen = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformationen with assumed inverted y-Axis
            Transformation::Translation(
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                {
                    -zentrum_invert_y
                },
            ),
            Transformation::Translation(start_invert_y),
        ];
        // Geraden
        pfade.push(gerade::zeichne(
            spurweite,
            self.länge,
            true,
            horizontal_transformationen.clone(),
            pfad::Erbauer::with_normal_axis,
        ));
        pfade.push(gerade::zeichne(
            spurweite,
            self.länge,
            true,
            gedreht_transformationen.clone(),
            pfad::Erbauer::with_invert_y,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            pfade.push(kurve::zeichne(
                spurweite,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                horizontal_transformationen,
                pfad::Erbauer::with_normal_axis,
            ));
            pfade.push(kurve::zeichne(
                spurweite,
                self.radius,
                winkel,
                kurve::Beschränkung::Keine,
                gedreht_transformationen,
                pfad::Erbauer::with_invert_y,
            ));
        }
        // return value
        pfade
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        // utility sizes
        let Vektor { x: width, y: height } = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor {
            x: Skalar(0.),
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: half_height - spurweite.beschränkung().halbiert(),
        };
        let zentrum = Vektor { x: half_width, y: half_height };
        let start_invert_y = Vektor {
            x: start.x,
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: -start.y,
        };
        let zentrum_invert_y = Vektor {
            x: zentrum.x,
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: -zentrum.y,
        };
        let winkel = self.winkel();
        // Transformationen
        let horizontal_transformationen = vec![Transformation::Translation(start)];
        let gedreht_transformationen = vec![
            Transformation::Translation(zentrum),
            Transformation::Rotation(winkel),
            // transformationen with assumed inverted y-Axis
            Transformation::Translation(
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                {
                    -zentrum_invert_y
                },
            ),
            Transformation::Translation(start_invert_y),
        ];
        let (gerade_transparenz, kurve_transparenz) = match anschlüsse.aktuelle_richtung() {
            None => (Transparenz::Voll, Transparenz::Voll),
            Some(Richtung::Gerade) => (Transparenz::Voll, Transparenz::Reduziert),
            Some(Richtung::Kurve) => (Transparenz::Reduziert, Transparenz::Voll),
        };
        let mut pfade = Vec::new();
        // Geraden
        pfade.push((
            gerade::fülle(
                spurweite,
                self.länge,
                horizontal_transformationen.clone(),
                pfad::Erbauer::with_normal_axis,
            ),
            None,
            gerade_transparenz,
        ));
        pfade.push((
            gerade::fülle(
                spurweite,
                self.länge,
                gedreht_transformationen.clone(),
                pfad::Erbauer::with_invert_y,
            ),
            None,
            gerade_transparenz,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            pfade.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    winkel,
                    horizontal_transformationen,
                    pfad::Erbauer::with_normal_axis,
                ),
                None,
                kurve_transparenz,
            ));
            pfade.push((
                kurve::fülle(
                    spurweite,
                    self.radius,
                    winkel,
                    gedreht_transformationen,
                    pfad::Erbauer::with_invert_y,
                ),
                None,
                kurve_transparenz,
            ));
        }
        // Rückgabewert
        pfade
    }

    fn beschreibung_und_name<'s, 't>(
        &'s self,
        anschlüsse: &'t Anschlüsse2,
        spurweite: Spurweite,
    ) -> (Position, Option<&'s str>, Option<&'t str>) {
        // utility sizes
        let size = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = size.y.halbiert();
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        let start = Vektor {
            x: Skalar(0.),
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: half_height - halbe_beschränkung,
        };
        (
            Position {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
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
    ) -> bool {
        // utility sizes
        let Vektor { x: width, y: height } = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_width = width.halbiert();
        let half_height = height.halbiert();
        let start = Vektor {
            x: Skalar(0.),
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            y: half_height - spurweite.beschränkung().halbiert(),
        };
        let zentrum = Vektor { x: half_width, y: half_height };
        let winkel = self.winkel();
        // sub-checks
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let horizontal_vector = relative_position - start;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let mut gedreht_vector = (relative_position - zentrum).rotiert(&(-winkel));
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            gedreht_vector.y = -gedreht_vector.y;
            gedreht_vector += zentrum - start;
        }
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

    fn verbindungen(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Self::Verbindungen {
        let Vektor { x: _, y: height } = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = height.halbiert();
        let anfang0 = Vektor { x: Skalar(0.), y: half_height };
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende0 = anfang0 + Vektor { x: self.länge, y: Skalar(0.) };
        let winkel = self.winkel();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let kurve = self.radius * Vektor { x: winkel.sin(), y: Skalar(1.) - winkel.cos() };
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let anfang1 = ende0 - kurve;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let ende1 = anfang0 + kurve;
        Verbindungen {
            anfang0: Verbindung { position: anfang0, richtung: winkel::PI },
            ende0: Verbindung { position: ende0, richtung: winkel::ZERO },
            anfang1: Verbindung {
                position: anfang1,
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                richtung: winkel::PI + winkel,
            },
            ende1: Verbindung { position: ende1, richtung: winkel },
        }
    }
}
