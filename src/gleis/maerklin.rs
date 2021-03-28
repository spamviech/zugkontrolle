//! This modules defines all Märklin rails I have access to.

use std::marker::PhantomData;

use super::gerade::Gerade;
use super::kreuzung::{self, Kreuzung};
use super::kurve::Kurve;
use super::types::*;
use super::weiche::{self, DreiwegeWeiche, KurvenWeiche, Weiche};
use crate::zugtyp::Maerklin;

// Märklin Kurven-Radien
const MAERKLIN_R_INDUSTRIE: Radius = Radius::new(286.);
const MAERKLIN_R1: Radius = Radius::new(360.);
const MAERKLIN_R2: Radius = Radius::new(437.4);

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
pub const MAERKLIN_GERADE_5106: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(180.) };
pub const MAERKLIN_GERADE_5107: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(90.) };
pub const MAERKLIN_GERADE_5129: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(70.) };
pub const MAERKLIN_GERADE_5108: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(45.) };
pub const MAERKLIN_GERADE_5109: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(33.5) };
pub const MAERKLIN_GERADE_5110: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(22.5) };
pub const MAERKLIN_GERADE_5210: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(16.) };
pub const MAERKLIN_GERADE_5208: Gerade<Maerklin> =
    Gerade { zugtyp: PhantomData, length: Length::new(8.) };

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
pub const MAERKLIN_KURVE_5120: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R_INDUSTRIE, angle: AngleDegrees::new(45.) };
pub const MAERKLIN_KURVE_5100: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R1, angle: AngleDegrees::new(30.) };
pub const MAERKLIN_KURVE_5101: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R1, angle: AngleDegrees::new(15.) };
pub const MAERKLIN_KURVE_5102: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R1, angle: AngleDegrees::new(7.5) };
pub const MAERKLIN_KURVE_5200: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R2, angle: AngleDegrees::new(30.) };
pub const MAERKLIN_KURVE_5206: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R2, angle: AngleDegrees::new(24.28) };
pub const MAERKLIN_KURVE_5201: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R2, angle: AngleDegrees::new(15.) };
pub const MAERKLIN_KURVE_5205: Kurve<Maerklin> =
    Kurve { zugtyp: PhantomData, radius: MAERKLIN_R2, angle: AngleDegrees::new(5.72) };

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
pub const fn maerklin_weiche_5117(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: PhantomData,
        length: Length::new(180.),
        radius: MAERKLIN_R2,
        angle: AngleDegrees::new(30.),
        direction: richtung,
    }
}
pub const fn maerklin_weiche_5137(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: PhantomData,
        length: Length::new(180.),
        radius: MAERKLIN_R2,
        angle: AngleDegrees::new(22.5),
        direction: richtung,
    }
}
pub const fn maerklin_weiche_5202(richtung: weiche::Richtung) -> Weiche<Maerklin> {
    Weiche {
        zugtyp: PhantomData,
        length: Length::new(180.),
        radius: MAERKLIN_R2,
        angle: AngleDegrees::new(24.28),
        direction: richtung,
    }
}

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
pub const MAERKLIN_DREIWEGE_WEICHE_5214: DreiwegeWeiche<Maerklin> = DreiwegeWeiche {
    zugtyp: PhantomData,
    length: Length::new(180.),
    radius: MAERKLIN_R2,
    angle: AngleDegrees::new(24.28),
};

/*
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
*/
pub const fn maerklin_kurven_weiche_5140(richtung: weiche::Richtung) -> KurvenWeiche<Maerklin> {
    KurvenWeiche {
        zugtyp: PhantomData,
        length: Length::new(77.3),
        radius: MAERKLIN_R1,
        angle: AngleDegrees::new(30.),
        direction: richtung,
    }
}

/*
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
*/
pub const MAERKLIN_KREUZUNG_5128: Kreuzung<Maerklin> = Kreuzung {
    zugtyp: PhantomData,
    length: Length::new(193.),
    radius: MAERKLIN_R1,
    variante: kreuzung::Variante::MitKurve,
};
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
pub const MAERKLIN_KREUZUNG_5207: Kreuzung<Maerklin> = Kreuzung {
    zugtyp: PhantomData,
    length: Length::new(180.),
    radius: MAERKLIN_R2,
    variante: kreuzung::Variante::MitKurve,
};

// TODO
/*
Prellbock:
    7190: 70mm
Kupplungsgleis:
    5112 U: 90mm
*/
