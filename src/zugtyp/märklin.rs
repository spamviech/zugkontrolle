//! This modules defines all Märklin rails I have access to.
use serde::{Deserialize, Serialize};

use crate::application::gleis::{
    gerade::Gerade,
    kreuzung::{self, *},
    kurve::Kurve,
    weiche::{self, *},
};
use crate::application::typen::*;
use crate::steuerung::geschwindigkeit::Mittelleiter;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Märklin;
impl Zugtyp for Märklin {
    type Leiter = Mittelleiter;

    const NAME: &'static str = "Märklin";
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

    fn weichen() -> Vec<WeicheUnit<Self>> {
        vec![
            weiche_5117_links(),
            weiche_5117_rechts(),
            weiche_5137_links(),
            weiche_5137_rechts(),
            weiche_5202_links(),
            weiche_5202_rechts(),
        ]
    }

    fn dreiwege_weichen() -> Vec<DreiwegeWeicheUnit<Self>> {
        vec![dreiwege_weiche_5214()]
    }

    fn kurven_weichen() -> Vec<KurvenWeicheUnit<Self>> {
        vec![kurven_weiche_5140_links(), kurven_weiche_5140_rechts()]
    }

    fn s_kurven_weichen() -> Vec<SKurvenWeicheUnit<Self>> {
        vec![]
    }

    fn kreuzungen() -> Vec<KreuzungUnit<Self>> {
        vec![kreuzung_5128(), kreuzung_5207()]
    }
}

// Märklin Kurven-Radien
const RADIUS_INDUSTRIE: Radius = Radius::neu(286.);
const RADIUS_R1: Radius = Radius::neu(360.);
const RADIUS_R2: Radius = Radius::neu(437.4);

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
    Gerade::neu_mit_beschreibung(Länge::neu(180.), "5106")
}
pub fn gerade_5107() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(90.), "5107")
}
pub fn gerade_5129() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(70.), "5129")
}
pub fn gerade_5108() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(45.), "5108")
}
pub fn gerade_5109() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(33.5), "5109")
}
pub fn gerade_5110() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(22.5), "5110")
}
pub fn gerade_5210() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(16.), "5210")
}
pub fn gerade_5208() -> Gerade<Märklin> {
    Gerade::neu_mit_beschreibung(Länge::neu(8.), "5208")
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
    Kurve::neu_mit_beschreibung(RADIUS_INDUSTRIE, WinkelGradmaß::neu(45.).into(), "5120")
}
pub fn kurve_5100() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(30.).into(), "5100")
}
pub fn kurve_5101() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(15.).into(), "5101")
}
pub fn kurve_5102() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(7.5).into(), "5102")
}
pub fn kurve_5200() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(30.).into(), "5200")
}
pub fn kurve_5206() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(24.28).into(), "5206")
}
pub fn kurve_5201() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(15.).into(), "5201")
}
pub fn kurve_5205() -> Kurve<Märklin> {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(5.72).into(), "5205")
}

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
pub fn weiche_5117(richtung: weiche::Orientierung) -> WeicheUnit<Märklin> {
    let beschreibung = match richtung {
        weiche::Orientierung::Links => "5117L",
        weiche::Orientierung::Rechts => "5117R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(30.).into(),
        richtung,
        beschreibung,
    )
}
pub fn weiche_5117_rechts() -> WeicheUnit<Märklin> {
    weiche_5117(weiche::Orientierung::Rechts)
}
pub fn weiche_5117_links() -> WeicheUnit<Märklin> {
    weiche_5117(weiche::Orientierung::Links)
}
pub fn weiche_5137(richtung: weiche::Orientierung) -> WeicheUnit<Märklin> {
    let beschreibung = match richtung {
        weiche::Orientierung::Links => "5137L",
        weiche::Orientierung::Rechts => "5137R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(22.5).into(),
        richtung,
        beschreibung,
    )
}
pub fn weiche_5137_rechts() -> WeicheUnit<Märklin> {
    weiche_5137(weiche::Orientierung::Rechts)
}
pub fn weiche_5137_links() -> WeicheUnit<Märklin> {
    weiche_5137(weiche::Orientierung::Links)
}
pub fn weiche_5202(richtung: weiche::Orientierung) -> WeicheUnit<Märklin> {
    let beschreibung = match richtung {
        weiche::Orientierung::Links => "5202L",
        weiche::Orientierung::Rechts => "5202R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(24.28).into(),
        richtung,
        beschreibung,
    )
}
pub fn weiche_5202_rechts() -> WeicheUnit<Märklin> {
    weiche_5202(weiche::Orientierung::Rechts)
}
pub fn weiche_5202_links() -> WeicheUnit<Märklin> {
    weiche_5202(weiche::Orientierung::Links)
}

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
pub fn dreiwege_weiche_5214() -> DreiwegeWeicheUnit<Märklin> {
    DreiwegeWeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(24.28).into(),
        "5214",
    )
}

/*
Kurven-Weiche
    5140 L/R: 30°, Rin360mm, Rout360mm @ 77.4mm (Gerade vor Bogen)
*/
pub fn kurven_weiche_5140(richtung: weiche::Orientierung) -> KurvenWeicheUnit<Märklin> {
    let beschreibung = match richtung {
        weiche::Orientierung::Links => "5140L",
        weiche::Orientierung::Rechts => "5140R",
    };
    KurvenWeiche::neu_mit_beschreibung(
        Länge::neu(77.3),
        RADIUS_R1,
        WinkelGradmaß::neu(30.).into(),
        richtung,
        beschreibung,
    )
}
pub fn kurven_weiche_5140_rechts() -> KurvenWeicheUnit<Märklin> {
    kurven_weiche_5140(weiche::Orientierung::Rechts)
}
pub fn kurven_weiche_5140_links() -> KurvenWeicheUnit<Märklin> {
    kurven_weiche_5140(weiche::Orientierung::Links)
}

/*
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
*/
pub fn kreuzung_5128() -> KreuzungUnit<Märklin> {
    Kreuzung::neu_mit_beschreibung(
        Länge::neu(193.),
        RADIUS_R1,
        kreuzung::Variante::MitKurve,
        "5128",
    )
}
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
pub fn kreuzung_5207() -> KreuzungUnit<Märklin> {
    Kreuzung::neu_mit_beschreibung(
        Länge::neu(180.),
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
