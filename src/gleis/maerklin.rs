//! This modules defines all Märklin rails I have access to.

use std::f32::consts::PI;

use super::gerade::Gerade;
use super::kreuzung::{self, Kreuzung};
use super::kurve::Kurve;
use super::types::*;
use super::weiche::{self, DreiwegeWeiche, KurvenWeiche, Weiche};
use crate::zugtyp::Maerklin;

// Märklin Kurven-Radien
const RADIUS_INDUSTRIE: Radius = Radius::new(286.);
const RADIUS_R1: Radius = Radius::new(360.);
const RADIUS_R2: Radius = Radius::new(437.4);

// floating point semantics aren't supported in const functions
macro_rules! as_radians {
    ($degrees:expr) => {
        $degrees * PI / 180.
    };
}

/*
H0 Spurweite: 16.5mm
Gerade
    5106: L180mm
    5107: L90mm
    5129: L70mm
    5108: L45mm
    5109: L33.5mm
    5110: L22.5mm
    5210: L16mm
    5208: L8mm
*/
pub const GERADE_5106: Gerade<Maerklin> = Gerade::new_with_description(Length::new(180.), "5106");
pub const GERADE_5107: Gerade<Maerklin> = Gerade::new_with_description(Length::new(90.), "5107");
pub const GERADE_5129: Gerade<Maerklin> = Gerade::new_with_description(Length::new(70.), "5129");
pub const GERADE_5108: Gerade<Maerklin> = Gerade::new_with_description(Length::new(45.), "5108");
pub const GERADE_5109: Gerade<Maerklin> = Gerade::new_with_description(Length::new(33.5), "5109");
pub const GERADE_5110: Gerade<Maerklin> = Gerade::new_with_description(Length::new(22.5), "5110");
pub const GERADE_5210: Gerade<Maerklin> = Gerade::new_with_description(Length::new(16.), "5210");
pub const GERADE_5208: Gerade<Maerklin> = Gerade::new_with_description(Length::new(8.), "5208");

/*
Kurve
    5120: 45°, R286mm
    5100: 30°, R360mm
    5101: 15°, R360mm
    5102: 7.5°, R360mm
    5200: 30°, R437.4mm
    5206: 24.28°, R437.4mm
    5201: 15°, R437.4mm
    5205: 5.72°, R437.4mm
*/
pub const KURVE_5120: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_INDUSTRIE, Angle::new(as_radians!(45.)), "5120");
pub const KURVE_5100: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(30.)), "5100");
pub const KURVE_5101: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(15.)), "5101");
pub const KURVE_5102: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(7.5)), "5102");
pub const KURVE_5200: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(30.)), "5200");
pub const KURVE_5206: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(24.28)), "5206");
pub const KURVE_5201: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(15.)), "5201");
pub const KURVE_5205: Kurve<Maerklin> =
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(5.72)), "5205");

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
const ANGLE_5117: Angle = Angle::new(as_radians!(30.));
pub const fn weiche_5117(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5117L",
        weiche::Richtung::Rechts => "5117R",
    };
    Weiche::new_with_description(Length::new(180.), RADIUS_R2, ANGLE_5117, richtung, beschreibung)
}
pub const WEICHE_5117_RECHTS: Weiche<Maerklin> = weiche_5117(weiche::Richtung::Rechts);
pub const WEICHE_5117_LINKS: Weiche<Maerklin> = weiche_5117(weiche::Richtung::Links);
const ANGLE_5137: Angle = Angle::new(as_radians!(22.5));
pub const fn weiche_5137(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5137L",
        weiche::Richtung::Rechts => "5137R",
    };
    Weiche::new_with_description(Length::new(180.), RADIUS_R2, ANGLE_5137, richtung, beschreibung)
}
pub const WEICHE_5137_RECHTS: Weiche<Maerklin> = weiche_5137(weiche::Richtung::Rechts);
pub const WEICHE_5137_LINKS: Weiche<Maerklin> = weiche_5137(weiche::Richtung::Links);
const ANGLE_5202: Angle = Angle::new(as_radians!(24.28));
pub const fn weiche_5202(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5202L",
        weiche::Richtung::Rechts => "5202R",
    };
    Weiche::new_with_description(Length::new(180.), RADIUS_R2, ANGLE_5202, richtung, beschreibung)
}
pub const WEICHE_5202_RECHTS: Weiche<Maerklin> = weiche_5202(weiche::Richtung::Rechts);
pub const WEICHE_5202_LINKS: Weiche<Maerklin> = weiche_5202(weiche::Richtung::Links);

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
pub const DREIWEGE_WEICHE_5214: DreiwegeWeiche<Maerklin> = DreiwegeWeiche::new_with_description(
    Length::new(180.),
    RADIUS_R2,
    Angle::new(as_radians!(24.28)),
    "5214",
);

/*
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
*/
const ANGLE_5140: Angle = Angle(as_radians!(30.));
pub const fn kurven_weiche_5140(richtung: weiche::Richtung) -> KurvenWeiche<Maerklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5140L",
        weiche::Richtung::Rechts => "5140R",
    };
    KurvenWeiche::new_with_description(
        Length::new(77.3),
        RADIUS_R1,
        ANGLE_5140,
        richtung,
        beschreibung,
    )
}
pub const KURVEN_WEICHE_5140_RECHTS: KurvenWeiche<Maerklin> =
    kurven_weiche_5140(weiche::Richtung::Rechts);
pub const KURVEN_WEICHE_5140_LINKS: KurvenWeiche<Maerklin> =
    kurven_weiche_5140(weiche::Richtung::Links);

/*
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
*/
pub const KREUZUNG_5128: Kreuzung<Maerklin> = Kreuzung::new_with_description(
    Length::new(193.),
    RADIUS_R1,
    kreuzung::Variante::MitKurve,
    "5128",
);
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
pub const KREUZUNG_5207: Kreuzung<Maerklin> = Kreuzung::new_with_description(
    Length::new(180.),
    RADIUS_R2,
    kreuzung::Variante::MitKurve,
    "5207",
);

// TODO
/*
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
*/
