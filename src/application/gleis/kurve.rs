//! Definition und zeichnen einer Kurve

use std::{f32::consts::PI, fmt::Debug, marker::PhantomData};

use serde::{Deserialize, Serialize};
use zugkontrolle_derive::alias_serialisiert_unit;

use crate::{
    application::{typen::*, verbindung::Verbindung},
    lookup::impl_lookup,
    steuerung::kontakt::{Kontakt, KontaktSerialisiert},
};

/// Definition einer Kurve
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
/// Zeichnen::width berücksichtigt nur positive x-Werte.
#[alias_serialisiert_unit(KontaktSerialisiert)]
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct Kurve<Z, Anschluss = Option<Kontakt>> {
    pub zugtyp: PhantomData<fn() -> Z>,
    pub radius: Skalar,
    pub winkel: Winkel,
    pub beschreibung: Option<String>,
    pub kontakt: Anschluss,
}
impl<Z> KurveUnit<Z> {
    pub fn neu(radius: Radius, winkel: Winkel) -> Self {
        KurveUnit {
            zugtyp: PhantomData,
            radius: radius.als_skalar(),
            winkel,
            beschreibung: None,
            kontakt: (),
        }
    }

    pub fn neu_mit_beschreibung(
        radius: Radius,
        winkel: Winkel,
        beschreibung: impl Into<String>,
    ) -> Self {
        KurveUnit {
            zugtyp: PhantomData,
            radius: radius.als_skalar(),
            winkel,
            beschreibung: Some(beschreibung.into()),
            kontakt: (),
        }
    }
}

#[impl_lookup(Verbindung, en, Debug)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VerbindungName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp, Anschluss: MitName> Zeichnen for Kurve<Z, Anschluss> {
    type VerbindungName = VerbindungName;
    type Verbindungen = Verbindungen;

    fn rechteck(&self) -> Rechteck {
        rechteck::<Z>(self.radius, self.winkel)
    }

    fn zeichne(&self) -> Vec<Pfad> {
        vec![zeichne(
            self.zugtyp,
            self.radius,
            self.winkel,
            Beschränkung::Alle,
            Vec::new(),
            pfad::Erbauer::with_normal_axis,
        )]
    }

    fn fülle(&self) -> Vec<(Pfad, Transparenz)> {
        vec![(
            fülle(
                self.zugtyp,
                self.radius,
                self.winkel,
                Vec::new(),
                pfad::Erbauer::with_normal_axis,
            ),
            Transparenz::Voll,
        )]
    }

    fn beschreibung_und_name(&self) -> (Position, Option<&String>, Option<&String>) {
        let half_angle = 0.5 * self.winkel;
        (
            Position {
                punkt: Vektor {
                    x: self.radius * half_angle.sin(),
                    y: beschränkung::<Z>().halbiert()
                        + self.radius * (Skalar(1.) - half_angle.cos()),
                },
                winkel: Winkel(0.),
            },
            self.beschreibung.as_ref(),
            self.kontakt.name(),
        )
    }

    fn innerhalb(&self, relative_position: Vektor, ungenauigkeit: Skalar) -> bool {
        innerhalb::<Z>(self.radius, self.winkel, relative_position, ungenauigkeit)
    }

    fn verbindungen(&self) -> Self::Verbindungen {
        let halbe_beschränkung = beschränkung::<Z>().halbiert();
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

pub(crate) fn rechteck<Z: Zugtyp>(radius: Skalar, winkel: Winkel) -> Rechteck {
    // Hilfswerte
    let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(radius);
    let breite_faktor;
    let höhe_vergleich;
    let position_x_faktor;
    if winkel < winkel::FRAC_PI_2 {
        breite_faktor = winkel.sin();
        höhe_vergleich = radius_begrenzung_außen * (Skalar(1.) - winkel.cos())
            + beschränkung::<Z>() * winkel.cos();
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
    let höhe = beschränkung::<Z>().max(&höhe_vergleich);
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

pub(crate) fn zeichne<Z, P, A>(
    _zugtyp: PhantomData<fn() -> Z>,
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
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| {
            zeichne_internal::<Z, P, A>(builder, radius, winkel, beschränkungen)
        }),
    );
    path_builder.baue_unter_transformationen(transformations)
}

// factor_y is expected to be -1 or +1, although other values should work as well
fn zeichne_internal<Z, P, A>(
    path_builder: &mut pfad::Erbauer<P, A>,
    radius: Skalar,
    winkel: Winkel,
    beschränkungen: Beschränkung,
) where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    // Utility Größen
    let spurweite: Skalar = spurweite::<Z>();
    let beschränkung: Skalar = beschränkung::<Z>();
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let gleis_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: beschränkung };
    let radius_begrenzung_außen: Skalar = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand::<Z>();
    let radius_innen = radius_außen - spurweite;
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

pub(crate) fn fülle<Z, P, A>(
    _zugtyp: PhantomData<fn() -> Z>,
    radius: Skalar,
    winkel: Winkel,
    transformations: Vec<Transformation>,
    with_invert_axis: impl FnOnce(
        &mut pfad::Erbauer<Vektor, Bogen>,
        Box<dyn FnOnce(&mut pfad::Erbauer<P, A>)>,
    ),
) -> Pfad
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let mut path_builder = pfad::Erbauer::neu();
    with_invert_axis(
        &mut path_builder,
        Box::new(move |builder| fülle_internal::<Z, P, A>(builder, radius, winkel)),
    );
    path_builder.baue_unter_transformationen(transformations)
}

/// Geplant für canvas::PathType::EvenOdd
fn fülle_internal<Z, P, A>(path_builder: &mut pfad::Erbauer<P, A>, radius: Skalar, winkel: Winkel)
where
    Z: Zugtyp,
    P: From<Vektor> + Into<Vektor>,
    A: From<Bogen> + Into<Bogen>,
{
    let spurweite = spurweite::<Z>();
    let abstand = abstand::<Z>();
    let beschränkung_links_oben = Vektor { x: Skalar(0.), y: Skalar(0.) };
    // Koordinaten für den Bogen
    let winkel_anfang: Winkel = Winkel(3. * PI / 2.);
    let winkel_ende: Winkel = winkel_anfang + winkel;
    let radius_begrenzung_außen: Skalar = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite;
    let bogen_zentrum =
        beschränkung_links_oben + Vektor { x: Skalar(0.), y: radius_begrenzung_außen };
    // Koordinaten links
    let gleis_links_oben = beschränkung_links_oben + Vektor { x: Skalar(0.), y: abstand };
    let gleis_links_unten = gleis_links_oben + Vektor { x: Skalar(0.), y: spurweite };
    // Koordinaten rechts
    let gleis_rechts_oben: Vektor = gleis_links_oben
        + radius_außen * Vektor { x: winkel.sin(), y: (Skalar(1.) - winkel.cos()) };
    let gleis_rechts_unten: Vektor =
        gleis_rechts_oben + Vektor { x: -spurweite * winkel.sin(), y: spurweite * winkel.cos() };
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

#[allow(unused_qualifications)]
pub(crate) fn innerhalb<Z: Zugtyp>(
    radius: Skalar,
    winkel: Winkel,
    relative_position: Vektor,
    ungenauigkeit: Skalar,
) -> bool {
    let spurweite = spurweite::<Z>();
    let abstand = abstand::<Z>();
    let radius_begrenzung_außen = radius_begrenzung_außen::<Z>(radius);
    let radius_außen = radius_begrenzung_außen - abstand;
    let radius_innen = radius_außen - spurweite;
    let bogen_zentrum = Vektor { x: Skalar(0.), y: abstand + radius_außen };
    let radius_vector = bogen_zentrum - relative_position;
    let länge = radius_vector.länge();
    if länge + ungenauigkeit > radius_innen && länge - ungenauigkeit < radius_außen {
        let acos = Winkel::acos(radius_vector.y / länge);
        let mut test_winkel: Winkel = if radius_vector.x > Skalar(0.) { -acos } else { acos };
        // normalisiere winkel
        while test_winkel < Winkel(0.) {
            test_winkel += Winkel(2. * std::f32::consts::PI)
        }
        if test_winkel < winkel {
            return true;
        }
    }
    false
}
