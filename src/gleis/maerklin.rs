//! This modules defines all Märklin rails I have access to.

use std::f32::consts::PI;
use std::marker::PhantomData;

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
const ZUGTYP: PhantomData<*const Maerklin> = PhantomData;

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
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_INDUSTRIE, angle: Angle::new(as_radians!(45.)) };
pub const KURVE_5100: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R1, angle: Angle::new(as_radians!(30.)) };
pub const KURVE_5101: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R1, angle: Angle::new(as_radians!(15.)) };
pub const KURVE_5102: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R1, angle: Angle::new(as_radians!(7.5)) };
pub const KURVE_5200: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R2, angle: Angle::new(as_radians!(30.)) };
pub const KURVE_5206: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R2, angle: Angle::new(as_radians!(24.28)) };
pub const KURVE_5201: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R2, angle: Angle::new(as_radians!(15.)) };
pub const KURVE_5205: Kurve<Maerklin> =
    Kurve { zugtyp: ZUGTYP, radius: RADIUS_R2, angle: Angle::new(as_radians!(5.72)) };

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
const ANGLE_5117: Angle = Angle::new(as_radians!(30.));
pub const fn weiche_5117(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: ZUGTYP,
        length: Length::new(180.),
        radius: RADIUS_R2,
        angle: ANGLE_5117,
        direction: richtung,
    }
}
pub const WEICHE_5117_RECHTS: Weiche<Maerklin> = weiche_5117(weiche::Richtung::Rechts);
pub const WEICHE_5117_LINKS: Weiche<Maerklin> = weiche_5117(weiche::Richtung::Links);
const ANGLE_5137: Angle = Angle::new(as_radians!(22.5));
pub const fn weiche_5137(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: ZUGTYP,
        length: Length::new(180.),
        radius: RADIUS_R2,
        angle: ANGLE_5137,
        direction: richtung,
    }
}
pub const WEICHE_5137_RECHTS: Weiche<Maerklin> = weiche_5137(weiche::Richtung::Rechts);
pub const WEICHE_5137_LINKS: Weiche<Maerklin> = weiche_5137(weiche::Richtung::Links);
const ANGLE_5202: Angle = Angle::new(as_radians!(24.28));
pub const fn weiche_5202(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: ZUGTYP,
        length: Length::new(180.),
        radius: RADIUS_R2,
        angle: ANGLE_5202,
        direction: richtung,
    }
}
pub const WEICHE_5202_RECHTS: Weiche<Maerklin> = weiche_5202(weiche::Richtung::Rechts);
pub const WEICHE_5202_LINKS: Weiche<Maerklin> = weiche_5202(weiche::Richtung::Links);

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
pub const DREIWEGE_WEICHE_5214: DreiwegeWeiche<Maerklin> = DreiwegeWeiche {
    zugtyp: ZUGTYP,
    length: Length::new(180.),
    radius: RADIUS_R2,
    angle: Angle::new(as_radians!(24.28)),
};

/*
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
*/
const ANGLE_5140: Angle = Angle(as_radians!(30.));
pub const fn kurven_weiche_5140(richtung: weiche::Richtung) -> KurvenWeiche<Maerklin> {
    KurvenWeiche {
        zugtyp: ZUGTYP,
        length: Length::new(77.3),
        radius: RADIUS_R1,
        angle: ANGLE_5140,
        direction: richtung,
    }
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
pub const KREUZUNG_5128: Kreuzung<Maerklin> = Kreuzung {
    zugtyp: ZUGTYP,
    length: Length::new(193.),
    radius: RADIUS_R1,
    variante: kreuzung::Variante::MitKurve,
};
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
pub const KREUZUNG_5207: Kreuzung<Maerklin> = Kreuzung {
    zugtyp: ZUGTYP,
    length: Length::new(180.),
    radius: RADIUS_R2,
    variante: kreuzung::Variante::MitKurve,
};

// TODO
/*
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
*/
