//! This modules defines all Lego (9V) rails I have access to.

use std::marker::PhantomData;

use super::gerade::Gerade;
use super::kreuzung::{self, Kreuzung};
use super::kurve::Kurve;
use super::types::*;
use super::weiche::{self, SKurvenWeiche};
use crate::zugtyp::Lego;

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
const LENGTH_VALUE: f64 = 128.;
const LENGTH: Length = Length::new(LENGTH_VALUE);
const RADIUS: Radius = Radius::new(320.);
const ANGLE_VALUE: f64 = 22.5;
const ZUGTYP: PhantomData<*const Lego> = PhantomData;

pub const GERADE: Gerade<Lego> = Gerade { zugtyp: ZUGTYP, length: LENGTH };

const ANGLE: AngleDegrees = AngleDegrees::new(ANGLE_VALUE);
pub const KURVE: Kurve<Lego> = Kurve { zugtyp: ZUGTYP, radius: RADIUS, angle: ANGLE };

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
const DOUBLE_LENGTH: Length = Length::new(2. * LENGTH_VALUE);
const ANGLE_OUTWARDS: AngleDegrees = AngleDegrees::new(1.625 * ANGLE_VALUE);
const ANGLE_INWARDS: AngleDegrees = AngleDegrees::new(0.625 * ANGLE_VALUE);
pub const fn weiche(richtung: weiche::Richtung) -> SKurvenWeiche<Lego> {
    SKurvenWeiche {
        zugtyp: ZUGTYP,
        length: DOUBLE_LENGTH,
        radius: RADIUS,
        angle: ANGLE_OUTWARDS,
        radius_reverse: RADIUS,
        angle_reverse: ANGLE_INWARDS,
        direction: richtung,
    }
}
pub const WEICHE_RECHTS: SKurvenWeiche<Lego> = weiche(weiche::Richtung::Rechts);
pub const WEICHE_LINKS: SKurvenWeiche<Lego> = weiche(weiche::Richtung::Links);

const HALF_LENGTH_RADIUS: Radius = Radius::new(0.5 * LENGTH_VALUE);
pub const KREUZUNG: Kreuzung<Lego> = Kreuzung {
    zugtyp: ZUGTYP,
    length: LENGTH,
    radius: HALF_LENGTH_RADIUS,
    variante: kreuzung::Variante::OhneKurve,
};
