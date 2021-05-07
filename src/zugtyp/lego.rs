//! This modules defines all Lego (9V) rails I have access to.

use std::f32::consts::PI;

use crate::gleis::gerade::Gerade;
use crate::gleis::kreuzung::{self, Kreuzung};
use crate::gleis::kurve::Kurve;
use crate::gleis::typen::*;
use crate::gleis::weiche::{self, DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lego;
impl Zugtyp for Lego {
    const SPURWEITE: Spurweite = Spurweite(38.);

    fn geraden() -> Vec<Gerade<Self>> {
        vec![GERADE]
    }

    fn kurven() -> Vec<Kurve<Self>> {
        vec![KURVE]
    }

    fn weichen() -> Vec<Weiche<Self>> {
        vec![]
    }

    fn dreiwege_weichen() -> Vec<DreiwegeWeiche<Self>> {
        vec![]
    }

    fn kurven_weichen() -> Vec<KurvenWeiche<Self>> {
        vec![]
    }

    fn s_kurven_weichen() -> Vec<SKurvenWeiche<Self>> {
        vec![WEICHE_LINKS, WEICHE_RECHTS]
    }

    fn kreuzungen() -> Vec<Kreuzung<Self>> {
        vec![KREUZUNG]
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
const LENGTH: Länge = Länge::new(LENGTH_VALUE);
const RADIUS: Radius = Radius::new(320.);
const ANGLE_VALUE_DEGREE: f32 = 22.5;
const ANGLE_VALUE: f32 = ANGLE_VALUE_DEGREE * PI / 180.;

pub const GERADE: Gerade<Lego> = Gerade::new(LENGTH);

const ANGLE: Winkel = Winkel::new(ANGLE_VALUE);
pub const KURVE: Kurve<Lego> = Kurve::new(RADIUS, ANGLE);

/*
Eine leichte S-Kurve: 6.5 Lücken rechts, dann 2.5 Lücken links; insgesamt 22.5°
Normale Kurve (22.5°) hat 4 Lücken
Nach 1 Gerade/Kurve sind Haupt- und Parallelgleis auf der selben Höhe
-------------------------------------------------------
Die Geometrie der LEGO Eisenbahnweichen hat sich mit Einführung des 9 V Systems verändert.
Das Parallelgleis nach der Weiche ist nun 8 Noppen vom Hauptgleis entfernt.
Beim 4,5 V/12V System führte das Parallelgleis direkt am Hauptgleis entlang.
-------------------------------------------------------
1 Noppe ist rund 0,8 cm (genauer: 0,79675... cm)
1,00 cm sind rund 1,25 Noppen (genauer: 1,255...)
*/
const DOUBLE_LENGTH: Länge = Länge::new(2. * LENGTH_VALUE);
const ANGLE_OUTWARDS: Winkel = Winkel::new(1.625 * ANGLE_VALUE);
const ANGLE_INWARDS: Winkel = Winkel::new(0.625 * ANGLE_VALUE);
pub const fn weiche(richtung: weiche::Richtung) -> SKurvenWeiche<Lego> {
    SKurvenWeiche::new(DOUBLE_LENGTH, RADIUS, ANGLE_OUTWARDS, RADIUS, ANGLE_INWARDS, richtung)
}
pub const WEICHE_RECHTS: SKurvenWeiche<Lego> = weiche(weiche::Richtung::Rechts);
pub const WEICHE_LINKS: SKurvenWeiche<Lego> = weiche(weiche::Richtung::Links);

const HALF_LENGTH_RADIUS: Radius = Radius::new(0.5 * LENGTH_VALUE);
pub const KREUZUNG: Kreuzung<Lego> =
    Kreuzung::new(LENGTH, HALF_LENGTH_RADIUS, kreuzung::Variante::OhneKurve);
