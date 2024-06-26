//! Definition und zeichnen einer [`DreiwegeWeiche`].

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use zugkontrolle_macros::{alias_serialisiert_unit, erstelle_richtung};
use zugkontrolle_typen::{
    canvas::{
        pfad::{self, Pfad, Transformation},
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
    steuerung::{
        self,
        weiche::{self, MitRichtung},
    },
};

/// Die Steuerung einer [`DreiwegeWeiche`].
type Steuerung = steuerung::weiche::Weiche<RichtungInformation, RichtungAnschlüsse>;
/// Serialisierbare Darstellung der Steuerung einer [`DreiwegeWeiche`].
type AnschlüsseSerialisiert =
    steuerung::weiche::WeicheSerialisiert<RichtungInformation, RichtungAnschlüsseSerialisiert>;

// Soll unqualifiziert verwendet werden.
#[allow(clippy::module_name_repetitions)]
/// Definition einer Dreiwege-Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[alias_serialisiert_unit(AnschlüsseSerialisiert)]
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DreiwegeWeiche<Anschlüsse = Option<Steuerung>> {
    /// Die Länge der Gerade.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der DreiwegeWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der DreiwegeWeiche.
    pub steuerung: Anschlüsse,
}

impl DreiwegeWeicheUnit {
    /// Erstelle eine neue [`DreiwegeWeiche`].
    #[must_use]
    pub const fn neu(länge: Länge, radius: Radius, winkel: Winkel) -> Self {
        DreiwegeWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: None,
            steuerung: (),
        }
    }

    /// Erstelle eine neue [`DreiwegeWeiche`] mit allgemeiner Beschreibung, z.B. der Produktnummer.
    #[must_use]
    pub fn neu_mit_beschreibung(
        länge: Länge,
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        DreiwegeWeicheUnit {
            länge: länge.als_skalar(),
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
            steuerung: (),
        }
    }
}

#[erstelle_richtung]
#[impl_nachschlagen(Verbindung, Verbindungen, Debug, Clone)]
/// [Verbindungen](Verbindung) einer [`DreiwegeWeiche`].
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    /// Das Ende, an dem sich die Gerade und beide Kurven treffen.
    Anfang,
    /// Das andere Ende der Gerade.
    Gerade,
    /// Das andere Ende der linken Kurve.
    Links,
    /// Das andere Ende der rechten Kurve.
    Rechts,
}

/// Die aktuelle und letzte [Richtung] einer [`DreiwegeWeiche`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RichtungInformation {
    /// Die aktuelle [Richtung] der [`DreiwegeWeiche`].
    pub aktuelle_richtung: Richtung,
    /// Die [Richtung] vor der aktuellen [`Richtung`].
    pub letzte_richtung: Richtung,
}

impl Default for RichtungInformation {
    fn default() -> Self {
        RichtungInformation {
            aktuelle_richtung: Richtung::Gerade,
            letzte_richtung: Richtung::Rechts,
        }
    }
}

impl MitRichtung<Richtung> for RichtungInformation {
    fn aktuelle_richtung(&self) -> Option<Richtung> {
        Some(self.aktuelle_richtung)
    }
}

impl weiche::Steuerung<Richtung> for RichtungInformation {
    type Zurücksetzen = Richtung;

    fn einstellen(&mut self, neue_richtung: Richtung) -> Self::Zurücksetzen {
        let zurücksetzen = self.letzte_richtung;
        self.letzte_richtung = self.aktuelle_richtung;
        self.aktuelle_richtung = neue_richtung;
        zurücksetzen
    }

    fn zurücksetzen(&mut self, zurücksetzen: Self::Zurücksetzen) {
        self.aktuelle_richtung = self.letzte_richtung;
        self.letzte_richtung = zurücksetzen;
    }
}

impl<Anschlüsse, Anschlüsse2: MitName + MitRichtung<Richtung>> Zeichnen<Anschlüsse2>
    for DreiwegeWeiche<Anschlüsse>
{
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self, _anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Rechteck {
        let DreiwegeWeiche { länge, radius, winkel, .. } = *self;
        let rechteck_gerade = gerade::rechteck(spurweite, länge);
        let rechteck_kurve = kurve::rechteck(spurweite, radius, winkel);
        let beschränkung = spurweite.beschränkung();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let höhe_verschoben = rechteck_kurve.ecke_max().y - beschränkung;
        let verschieben = Vektor { x: Skalar(0.), y: höhe_verschoben };
        let rechteck_gerade_verschoben = rechteck_gerade.verschiebe_chain(&verschieben);
        let rechteck_kurve_verschoben = rechteck_kurve.clone().verschiebe_chain(&verschieben);
        rechteck_gerade_verschoben
            .einschließend(&rechteck_kurve)
            .einschließend(&rechteck_kurve_verschoben)
    }

    fn zeichne(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Vec<Pfad> {
        // utility sizes
        let size = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = size.y.halbiert();
        let beschränkung = spurweite.beschränkung();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        let rechts_transformationen = vec![Transformation::Translation(start)];
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_transformationen =
            vec![Transformation::Translation(start + Vektor { x: Skalar(0.), y: beschränkung })];
        // Pfade
        let gerade = gerade::zeichne(
            spurweite,
            self.länge,
            true,
            rechts_transformationen.clone(),
            pfad::Erbauer::with_normal_axis,
        );
        let links = kurve::zeichne(
            spurweite,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            links_transformationen,
            pfad::Erbauer::with_invert_y,
        );
        let rechts = kurve::zeichne(
            spurweite,
            self.radius,
            self.winkel,
            kurve::Beschränkung::Ende,
            rechts_transformationen,
            pfad::Erbauer::with_normal_axis,
        );
        // Rückgabewert
        vec![gerade, links, rechts]
    }

    fn fülle(
        &self,
        anschlüsse: &Anschlüsse2,
        spurweite: Spurweite,
    ) -> Vec<(Pfad, Option<Farbe>, Transparenz)> {
        // utility sizes
        let size = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = size.y.halbiert();
        let beschränkung = spurweite.beschränkung();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        let rechts_transformationen = vec![Transformation::Translation(start)];
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let links_transformationen =
            vec![Transformation::Translation(start + Vektor { x: Skalar(0.), y: beschränkung })];
        let (gerade_transparenz, links_transparenz, rechts_transparenz) =
            match anschlüsse.aktuelle_richtung() {
                None => (Transparenz::Voll, Transparenz::Voll, Transparenz::Voll),
                Some(Richtung::Gerade) => {
                    (Transparenz::Voll, Transparenz::Reduziert, Transparenz::Reduziert)
                },
                Some(Richtung::Links) => {
                    (Transparenz::Reduziert, Transparenz::Voll, Transparenz::Reduziert)
                },
                Some(Richtung::Rechts) => {
                    (Transparenz::Reduziert, Transparenz::Reduziert, Transparenz::Voll)
                },
            };
        // Pfade
        let gerade = (
            gerade::fülle(
                spurweite,
                self.länge,
                rechts_transformationen.clone(),
                pfad::Erbauer::with_normal_axis,
            ),
            None,
            gerade_transparenz,
        );
        let links = (
            kurve::fülle(
                spurweite,
                self.radius,
                self.winkel,
                links_transformationen,
                pfad::Erbauer::with_invert_y,
            ),
            None,
            links_transparenz,
        );
        let rechts = (
            kurve::fülle(
                spurweite,
                self.radius,
                self.winkel,
                rechts_transformationen,
                pfad::Erbauer::with_normal_axis,
            ),
            None,
            rechts_transparenz,
        );
        // Rückgabewert
        vec![gerade, links, rechts]
    }

    fn beschreibung_und_name<'s, 't>(
        &'s self,
        anschlüsse: &'t Anschlüsse2,
        spurweite: Spurweite,
    ) -> (Position, Option<&'s str>, Option<&'t str>) {
        let size = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = size.y.halbiert();
        let halbe_beschränkung = spurweite.beschränkung().halbiert();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let start = Vektor { x: Skalar(0.), y: half_height - halbe_beschränkung };
        (
            Position {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                punkt: start + Vektor { x: self.länge.halbiert(), y: halbe_beschränkung },
                winkel: winkel::ZERO,
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
        let Vektor { x: _, y: height } = self.rechteck(anschlüsse, spurweite).ecke_max();
        let half_height = height.halbiert();
        let beschränkung = spurweite.beschränkung();
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let start = Vektor { x: Skalar(0.), y: half_height - beschränkung.halbiert() };
        // sub-checks
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let relative_vector = relative_position - start;
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        let inverted_vector = Vektor { x: relative_vector.x, y: beschränkung - relative_vector.y };
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
                inverted_vector,
                ungenauigkeit,
            ))
    }

    fn verbindungen(&self, anschlüsse: &Anschlüsse2, spurweite: Spurweite) -> Self::Verbindungen {
        let height: Skalar = self.rechteck(anschlüsse, spurweite).ecke_max().y;
        let half_height = height.halbiert();
        let länge: Skalar = self.länge;
        let radius: Skalar = self.radius;
        let anfang = Vektor { x: Skalar(0.), y: half_height };
        Verbindungen {
            anfang: Verbindung { position: anfang, richtung: winkel::PI },
            gerade: Verbindung {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                position: anfang + Vektor { x: länge, y: Skalar(0.) },
                richtung: winkel::ZERO,
            },
            links: Verbindung {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                position: anfang
                    + Vektor {
                        x: radius * self.winkel.sin(),
                        y: radius * (Skalar(1.) - self.winkel.cos()),
                    },
                richtung: self.winkel,
            },
            rechts: Verbindung {
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                position: anfang
                    + Vektor {
                        x: radius * self.winkel.sin(),
                        y: -radius * (Skalar(1.) - self.winkel.cos()),
                    },
                // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
                #[allow(clippy::arithmetic_side_effects)]
                richtung: -self.winkel,
            },
        }
    }
}
