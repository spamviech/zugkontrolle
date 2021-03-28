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
#[allow(non_upper_case_globals)]
const length: Length = Length::new(128.);
#[allow(non_upper_case_globals)]
const double_length: Length = Length::new(256.);
#[allow(non_upper_case_globals)]
const radius: Radius = Radius::new(320.);
#[allow(non_upper_case_globals)]
const half_length_radius: Radius = Radius::new(64.);
#[allow(non_upper_case_globals)]
const angle: AngleDegrees = AngleDegrees::new(22.5);
#[allow(non_upper_case_globals)]
const double_angle: AngleDegrees = AngleDegrees::new(45.);
#[allow(non_upper_case_globals)]
const zugtyp: PhantomData<*const Lego> = PhantomData;

pub const GERADE: Gerade<Lego> = Gerade { zugtyp, length };

pub const KURVE: Kurve<Lego> = Kurve { zugtyp, radius, angle };

/*
Eine leichte S-Kurve: 6 rechts, dann 3 links; insgesamt 22.5°
Nach 1 Gerade/Kurve sind Haupt- und Parallelgleis auf der selben Höhe
-------------------------------------------------------
Die Geometrie der LEGO Eisenbahnweichen hat sich mit Einführung des 9 V Systems verändert.
Das Parallelgleis nach der Weiche ist nun 8 Noppen vom Hauptgleis entfernt.
Beim 4,5 V/12V System führte das Parallelgleis direkt am Hauptgleis entlang.
-------------------------------------------------------
1 Noppe ist rund 0,8 cm (genauer: 0,79675... cm)
1,00 cm sind rund 1,25 Noppen (genauer: 1,255...)
-------------------------------------------------------
*/
pub const fn weiche(richtung: weiche::Richtung) -> SKurvenWeiche<Lego> {
    SKurvenWeiche {
        zugtyp,
        length: double_length,
        radius,
        angle: double_angle,
        radius_reverse: radius,
        angle_reverse: angle,
        direction: richtung,
    }
}

pub const KREUZUNG: Kreuzung<Lego> = Kreuzung {
    zugtyp,
    length,
    radius: half_length_radius,
    variante: kreuzung::Variante::OhneKurve,
};
