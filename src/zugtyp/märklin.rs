//! This modules defines all Märklin rails I have access to.

use std::f32::consts::PI;

use crate::gleis::gerade::Gerade;
use crate::gleis::kreuzung::{self, Kreuzung};
use crate::gleis::kurve::Kurve;
use crate::gleis::types::*;
use crate::gleis::weiche::{self, DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche};

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Märklin;
impl Zugtyp for Märklin {
    const SPURWEITE: Spurweite = Spurweite(16.5);

    fn geraden() -> Vec<Gerade<Self>> {
        vec![
            gerade_5106(),
            gerade_5107(),
            gerade_5108(),
            gerade_5109(),
            gerade_5110(),
            gerade_5129(),
            gerade_5208(),
            gerade_5210(),
        ]
    }

    fn kurven() -> Vec<Kurve<Self>> {
        vec![
            kurve_5100(),
            kurve_5101(),
            kurve_5102(),
            kurve_5120(),
            kurve_5200(),
            kurve_5201(),
            kurve_5205(),
            kurve_5206(),
        ]
    }

    fn weichen() -> Vec<Weiche<Self>> {
        vec![
            weiche_5117_links(),
            weiche_5117_rechts(),
            weiche_5137_links(),
            weiche_5137_rechts(),
            weiche_5202_links(),
            weiche_5202_rechts(),
        ]
    }

    fn dreiwege_weichen() -> Vec<DreiwegeWeiche<Self>> {
        vec![dreiwege_weiche_5214()]
    }

    fn kurven_weichen() -> Vec<KurvenWeiche<Self>> {
        vec![kurven_weiche_5140_links(), kurven_weiche_5140_rechts()]
    }

    fn s_kurven_weichen() -> Vec<SKurvenWeiche<Self>> {
        vec![]
    }

    fn kreuzungen() -> Vec<Kreuzung<Self>> {
        vec![kreuzung_5128(), kreuzung_5207()]
    }
}

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
pub fn gerade_5106() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(180.), "5106")
}
pub fn gerade_5107() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(90.), "5107")
}
pub fn gerade_5129() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(70.), "5129")
}
pub fn gerade_5108() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(45.), "5108")
}
pub fn gerade_5109() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(33.5), "5109")
}
pub fn gerade_5110() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(22.5), "5110")
}
pub fn gerade_5210() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(16.), "5210")
}
pub fn gerade_5208() -> Gerade<Märklin> {
    Gerade::new_with_description(Länge::new(8.), "5208")
}

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
pub fn kurve_5120() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_INDUSTRIE, Angle::new(as_radians!(45.)), "5120")
}
pub fn kurve_5100() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(30.)), "5100")
}
pub fn kurve_5101() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(15.)), "5101")
}
pub fn kurve_5102() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R1, Angle::new(as_radians!(7.5)), "5102")
}
pub fn kurve_5200() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(30.)), "5200")
}
pub fn kurve_5206() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(24.28)), "5206")
}
pub fn kurve_5201() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(15.)), "5201")
}
pub fn kurve_5205() -> Kurve<Märklin> {
    Kurve::new_with_description(RADIUS_R2, Angle::new(as_radians!(5.72)), "5205")
}

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
const ANGLE_5117: Angle = Angle::new(as_radians!(30.));
pub fn weiche_5117(richtung: weiche::Richtung) -> Weiche<Märklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5117L",
        weiche::Richtung::Rechts => "5117R",
    };
    Weiche::new_with_description(Länge::new(180.), RADIUS_R2, ANGLE_5117, richtung, beschreibung)
}
pub fn weiche_5117_rechts() -> Weiche<Märklin> {
    weiche_5117(weiche::Richtung::Rechts)
}
pub fn weiche_5117_links() -> Weiche<Märklin> {
    weiche_5117(weiche::Richtung::Links)
}
const ANGLE_5137: Angle = Angle::new(as_radians!(22.5));
pub fn weiche_5137(richtung: weiche::Richtung) -> Weiche<Märklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5137L",
        weiche::Richtung::Rechts => "5137R",
    };
    Weiche::new_with_description(Länge::new(180.), RADIUS_R2, ANGLE_5137, richtung, beschreibung)
}
pub fn weiche_5137_rechts() -> Weiche<Märklin> {
    weiche_5137(weiche::Richtung::Rechts)
}
pub fn weiche_5137_links() -> Weiche<Märklin> {
    weiche_5137(weiche::Richtung::Links)
}
const ANGLE_5202: Angle = Angle::new(as_radians!(24.28));
pub fn weiche_5202(richtung: weiche::Richtung) -> Weiche<Märklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5202L",
        weiche::Richtung::Rechts => "5202R",
    };
    Weiche::new_with_description(Länge::new(180.), RADIUS_R2, ANGLE_5202, richtung, beschreibung)
}
pub fn weiche_5202_rechts() -> Weiche<Märklin> {
    weiche_5202(weiche::Richtung::Rechts)
}
pub fn weiche_5202_links() -> Weiche<Märklin> {
    weiche_5202(weiche::Richtung::Links)
}

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
pub fn dreiwege_weiche_5214() -> DreiwegeWeiche<Märklin> {
    DreiwegeWeiche::new_with_description(
        Länge::new(180.),
        RADIUS_R2,
        Angle::new(as_radians!(24.28)),
        "5214",
    )
}

/*
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
*/
const ANGLE_5140: Angle = Angle(as_radians!(30.));
pub fn kurven_weiche_5140(richtung: weiche::Richtung) -> KurvenWeiche<Märklin> {
    let beschreibung = match richtung {
        weiche::Richtung::Links => "5140L",
        weiche::Richtung::Rechts => "5140R",
    };
    KurvenWeiche::new_with_description(
        Länge::new(77.3),
        RADIUS_R1,
        ANGLE_5140,
        richtung,
        beschreibung,
    )
}
pub fn kurven_weiche_5140_rechts() -> KurvenWeiche<Märklin> {
    kurven_weiche_5140(weiche::Richtung::Rechts)
}
pub fn kurven_weiche_5140_links() -> KurvenWeiche<Märklin> {
    kurven_weiche_5140(weiche::Richtung::Links)
}

/*
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
*/
pub fn kreuzung_5128() -> Kreuzung<Märklin> {
    Kreuzung::new_with_description(
        Länge::new(193.),
        RADIUS_R1,
        kreuzung::Variante::MitKurve,
        "5128",
    )
}
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
pub fn kreuzung_5207() -> Kreuzung<Märklin> {
    Kreuzung::new_with_description(
        Länge::new(180.),
        RADIUS_R2,
        kreuzung::Variante::MitKurve,
        "5207",
    )
}

// TODO
/*
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
*/
