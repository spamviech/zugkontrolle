//! Dieses Modul definiert alle Lego (9V) Gleise, die ich zur Verfügung habe.

use std::{f32::consts::PI, marker::PhantomData, time::Duration};

use once_cell::sync::Lazy;

use zugkontrolle_typen::{
    mm::{Länge, Radius, Spurweite},
    winkel::Winkel,
};
use zugkontrolle_util::eingeschränkt::NichtNegativ;

use crate::{
    gleis::{
        gerade::{Gerade, GeradeUnit},
        kreuzung::{self, Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            dreiwege::DreiwegeWeiche,
            gerade::Weiche,
            kurve::KurvenWeiche,
            orientierung::Orientierung,
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    gleise::daten::de_serialisieren::erzeuge_zugtyp_maps,
    steuerung::geschwindigkeit::Zweileiter,
    zugtyp::Zugtyp,
};

/// Alle bekannten Gleise und Eigenschaften für eine Lego-Eisenbahn.
static LEGO: Lazy<Zugtyp<Zweileiter>> = Lazy::new(|| {
    let geraden = [gerade()];
    let kurven = [kurve()];
    let weichen = [];
    let dreiwege_weichen = [];
    let kurven_weichen = [];
    let s_kurven_weichen = [weiche(Orientierung::Links), weiche(Orientierung::Rechts)];
    let kreuzungen = [kreuzung()];
    erzeuge_zugtyp_maps!(
        geraden: Gerade | "Anzahl der Geraden kann man an den Händen abzählen.",
        kurven: Kurve | "Anzahl der Kurven kann man an den Händen abzählen.",
        weichen: Weiche | "Anzahl der Weichen kann man an den Händen abzählen.",
        dreiwege_weichen: DreiwegeWeiche | "Anzahl der Dreiwege-Weichen kann man an den Händen abzählen.",
        kurven_weichen: KurvenWeiche | "Anzahl der Kurven-Weichen kann man an den Händen abzählen.",
        s_kurven_weichen: SKurvenWeiche | "Anzahl der S-Kurven-Weichen kann man an den Händen abzählen.",
        kreuzungen: Kreuzung | "Anzahl der Kreuzungen kann man an den Händen abzählen.",
    );
    Zugtyp {
        name: String::from("Lego"),
        leiter: PhantomData,
        spurweite: Spurweite::neu(38.),
        geraden,
        kurven,
        weichen,
        dreiwege_weichen,
        kurven_weichen,
        s_kurven_weichen,
        kreuzungen,
        pwm_frequenz: NichtNegativ::neu_unchecked(50.),
        verhältnis_fahrspannung_überspannung: PhantomData,
        stopp_zeit: Duration::from_millis(500),
        umdrehen_zeit: PhantomData,
        schalten_zeit: Duration::from_millis(400),
    }
});

impl Zugtyp<Zweileiter> {
    /// Lego
    #[must_use]
    pub fn lego() -> &'static Zugtyp<Zweileiter> {
        &LEGO
    }
}

/*
https://blaulicht-schiene.jimdofree.com/projekte/lego-daten/

Der Maßstab liegt zwischen 1:35 - 1:49.
Spurbreite
    Innen: ca. 3,81 cm (38,1 mm)
    Außen: ca. 4,20 cm (42,0 mm)
Gerade (Straight)
    Länge: ca. 17 Noppen / 13,6 cm
    Breite: ca. 8 Noppen / 6,4 cm
    Höhe: ca. 1 1/3 Noppen (1 Stein-Höhe) / 1,0 cm
Kurve (Curve)
    Länge Außen: ca. 17 1/2 Noppen (14 1/3 Legosteine hoch) / 13,7 cm
    Länge Innen: ca. 15 1/6 Noppen (12 2/3 Legosteine hoch) / 12,1 cm
    Breite: 8 Noppen / 6,4 cm
Kreis-Aufbau mit PF-Kurven
Ein Kreis benötigt 16 Lego-PF-Kurven.
    Kreis-Durchmesser Außen: ca. 88 Noppen / 70 cm
    Kreis-Durchmesser Innen: ca. 72 Noppen / 57 cm
    Kreis-Radius (halber Kreis, Außen bis Kreismittelpunkt): ca. 44 Noppen / 35 cm
Lego Spurweite: 38mm
*/

/// Die Länge einer [`Gerade`] in mm.
const LENGTH_VALUE: f32 = 128.;
/// [`LENGTH_VALUE`] als [`Länge`].
const LENGTH: Länge = Länge::neu(LENGTH_VALUE);
/// Der Radius einer Kurve in mm.
const RADIUS_VALUE: f32 = 320.;
/// [`RADIUS_VALUE`] als [`Radius`].
const RADIUS: Radius = Radius::neu(RADIUS_VALUE);
/// Der Winkel einer [`Kurve`] im Bogenmaß.
const ANGLE_VALUE_DEGREE: f32 = 22.5;
/// [`ANGLE_VALUE_DEGREE`] im Gradmaß.
const ANGLE_VALUE: f32 = ANGLE_VALUE_DEGREE * PI / 180.;
/// [`ANGLE_VALUE`] als [`Winkel`].
const ANGLE: Winkel = Winkel(ANGLE_VALUE);

/// Eine Lego-Gerade mit `12.8cm` Länge.
#[must_use]
pub fn gerade() -> GeradeUnit {
    Gerade::neu(LENGTH)
}

/// Eine Lego-Kurve mit `32cm` Kurvenradius und `22.5°` Winkel.
#[must_use]
pub fn kurve() -> KurveUnit {
    Kurve::neu(RADIUS, ANGLE)
}

/*
Eine leichte S-Kurve: 6.5 Lücken rechts, dann 2.5 Lücken links; insgesamt 22.5°
Normale Kurve (22.5°) hat 4 Lücken
Nach 1 Gerade/Kurve sind Haupt- und Parallelgleis auf der selben Höhe
const ANGLE_DIFFERENCE: Winkel = ANGLE_OUTWARDS - ANGLE_INWARDS;
ANGLE_OUTWARDS = (1.5 * LENGTH / RADIUS).asin();
ANGLE_INWARDS = ANGLE_OUTWARDS - ANGLE;
-------------------------------------------------------
Die Geometrie der LEGO Eisenbahnweichen hat sich mit Einführung des 9 V Systems verändert.
Das Parallelgleis nach der Weiche ist nun 8 Noppen vom Hauptgleis entfernt.
Beim 4,5 V/12V System führte das Parallelgleis direkt am Hauptgleis entlang.
-------------------------------------------------------
1 Noppe ist rund 0,8 cm (genauer: 0,79675... cm)
1,00 cm sind rund 1,25 Noppen (genauer: 1,255...)
*/
/// Die Länge einer [`SKurvenWeiche`] (doppelte Länge einer [`Gerade`]).
const DOUBLE_LENGTH: Länge = Länge::neu(2. * LENGTH_VALUE);
/// Der Winkel der nach außen gehenden Kurve im Bogenmaß.
// const ANGLE_OUTWARDS_VALUE: f32 = (1.5 * LENGTH_VALUE / RADIUS_VALUE).asin();
// https://www.wolframalpha.com/input/?i=asin%281.5+*+128%2F+320%29
const ANGLE_OUTWARDS_VALUE: f32 = 0.643_501_1;
/// [`ANGLE_OUTWARDS_VALUE`] als [`Winkel`].
const ANGLE_OUTWARDS: Winkel = Winkel(ANGLE_OUTWARDS_VALUE);
/// Der Winkel der nach innen gehenden Kurve als [`Winkel`].
const ANGLE_INWARDS: Winkel = Winkel(ANGLE_OUTWARDS_VALUE - ANGLE_VALUE);

/// Eine Lego-Weiche. Nach einer zusätzlichen [Gerade]/[`Kurve`] sind beide Gleise auf der selben Höhe.
#[must_use]
pub fn weiche(orientierung: Orientierung) -> SKurvenWeicheUnit {
    SKurvenWeiche::neu(DOUBLE_LENGTH, RADIUS, ANGLE_OUTWARDS, RADIUS, ANGLE_INWARDS, orientierung)
}

/// Der Radius einer [Kreuzung] als [`Radius`].
const HALF_LENGTH_RADIUS: Radius = Radius::neu(0.5 * LENGTH_VALUE);

/// Eine Lego-Kreuzung: `90°`-Winkel ohne Schaltmöglichkeit, Länge `13.6cm`.
#[must_use]
pub fn kreuzung() -> KreuzungUnit {
    Kreuzung::neu(LENGTH, HALF_LENGTH_RADIUS, kreuzung::Variante::OhneKurve)
}
