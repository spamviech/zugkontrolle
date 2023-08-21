//! Dieses Modul definiert alle Lego (9V) Gleise, die ich zur Verfügung habe.

use std::{f32::consts::PI, marker::PhantomData, time::Duration};

use crate::{
    gleis::{
        gerade::{Gerade, GeradeUnit},
        kreuzung::{self, Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            orientierung::Orientierung,
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::geschwindigkeit::Zweileiter,
    typen::{
        mm::{Länge, Radius, Spurweite},
        winkel::Winkel,
    },
    util::eingeschränkt::NichtNegativ,
    zugtyp::Zugtyp,
};

impl Zugtyp<Zweileiter> {
    /// Lego
    pub fn lego() -> Zugtyp<Zweileiter> {
        Zugtyp {
            name: "Lego".to_string(),
            leiter: PhantomData,
            spurweite: Spurweite::neu(38.),
            geraden: vec![gerade()],
            kurven: vec![kurve()],
            weichen: vec![],
            dreiwege_weichen: vec![],
            kurven_weichen: vec![],
            s_kurven_weichen: vec![weiche(Orientierung::Links), weiche(Orientierung::Rechts)],
            kreuzungen: vec![kreuzung()],
            pwm_frequenz: NichtNegativ::neu_unchecked(50.),
            verhältnis_fahrspannung_überspannung: PhantomData,
            stopp_zeit: Duration::from_millis(500),
            umdrehen_zeit: PhantomData,
            schalten_zeit: Duration::from_millis(400),
        }
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
const LENGTH_VALUE: f32 = 128.;
const LENGTH: Länge = Länge::neu(LENGTH_VALUE);
const RADIUS_VALUE: f32 = 320.;
const RADIUS: Radius = Radius::neu(RADIUS_VALUE);
const ANGLE_VALUE_DEGREE: f32 = 22.5;
const ANGLE_VALUE: f32 = ANGLE_VALUE_DEGREE * PI / 180.;

/// Eine Lego-Gerade mit `13.6cm` Länge.
pub fn gerade() -> GeradeUnit {
    Gerade::neu(LENGTH)
}

const ANGLE: Winkel = Winkel(ANGLE_VALUE);

/// Eine Lego-Kurve mit `32cm` Kurvenradius und `22.5°` Winkel.
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
const DOUBLE_LENGTH: Länge = Länge::neu(2. * LENGTH_VALUE);
// const ANGLE_OUTWARDS_VALUE: f32 = (1.5 * LENGTH_VALUE / RADIUS_VALUE).asin();
// https://www.wolframalpha.com/input/?i=asin%281.5+*+128%2F+320%29
const ANGLE_OUTWARDS_VALUE: f32 = 0.6435011087932843868028092287173226380415105911153123828656;
const ANGLE_OUTWARDS: Winkel = Winkel(ANGLE_OUTWARDS_VALUE);
const ANGLE_INWARDS: Winkel = Winkel(ANGLE_OUTWARDS_VALUE - ANGLE_VALUE);

/// Eine Lego-Weiche, nach einer zusätzlichen Gerade/Kurve sind beide Gleise auf der selben Höhe.
pub fn weiche(orientierung: Orientierung) -> SKurvenWeicheUnit {
    SKurvenWeiche::neu(DOUBLE_LENGTH, RADIUS, ANGLE_OUTWARDS, RADIUS, ANGLE_INWARDS, orientierung)
}

const HALF_LENGTH_RADIUS: Radius = Radius::neu(0.5 * LENGTH_VALUE);

/// Eine Lego-Kreuzung: `90°`-Winkel ohne Schaltmöglichkeit, Länge `13.6cm`.
pub fn kreuzung() -> KreuzungUnit {
    Kreuzung::neu(LENGTH, HALF_LENGTH_RADIUS, kreuzung::Variante::OhneKurve)
}
