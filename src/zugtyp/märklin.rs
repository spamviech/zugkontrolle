//! Dieses Modul definiert alle Märklin-Gleise, die mir zur Verfügung stehen.

use std::{marker::PhantomData, time::Duration};

use crate::{
    eingeschränkt::{NichtNegativ, NullBisEins},
    gleis::{
        gerade::{Gerade, GeradeUnit},
        kreuzung::{self, Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            dreiwege::DreiwegeWeicheUnit,
            gerade::WeicheUnit,
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            orientierung::Orientierung,
        },
    },
    steuerung::geschwindigkeit::Mittelleiter,
    typen::{
        mm::{Länge, Radius, Spurweite},
        winkel::WinkelGradmaß,
    },
    zugtyp::Zugtyp,
};

impl Zugtyp<Mittelleiter> {
    /// Märklin
    pub fn märklin() -> Zugtyp<Mittelleiter> {
        Zugtyp {
            name: "Märklin".to_string(),
            leiter: PhantomData,
            spurweite: Spurweite::neu(16.5),
            geraden: vec![
                gerade_5106(),
                gerade_5107(),
                gerade_5108(),
                gerade_5109(),
                gerade_5110(),
                gerade_5129(),
                gerade_5208(),
                gerade_5210(),
            ],
            kurven: vec![
                kurve_5100(),
                kurve_5101(),
                kurve_5102(),
                kurve_5120(),
                kurve_5200(),
                kurve_5201(),
                kurve_5205(),
                kurve_5206(),
            ],
            weichen: vec![
                weiche_5117_links(),
                weiche_5117_rechts(),
                weiche_5137_links(),
                weiche_5137_rechts(),
                weiche_5202_links(),
                weiche_5202_rechts(),
            ],
            dreiwege_weichen: vec![dreiwege_weiche_5214()],
            kurven_weichen: vec![kurven_weiche_5140_links(), kurven_weiche_5140_rechts()],
            s_kurven_weichen: vec![],
            kreuzungen: vec![kreuzung_5128(), kreuzung_5207()],
            pwm_frequenz: NichtNegativ::neu_unchecked(50.),
            verhältnis_fahrspannung_überspannung: NullBisEins::neu_unchecked(16. / 25.),
            stopp_zeit: Duration::from_millis(500),
            umdrehen_zeit: Duration::from_millis(500),
            schalten_zeit: Duration::from_millis(400),
        }
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
/// Eine Gerade der Länge `180mm`.
pub fn gerade_5106() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(180.), "5106")
}
/// Eine Gerade der Länge `90mm`.
pub fn gerade_5107() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(90.), "5107")
}
/// Eine Gerade der Länge `70mm`.
pub fn gerade_5129() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(70.), "5129")
}
/// Eine Gerade der Länge `45mm`.
pub fn gerade_5108() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(45.), "5108")
}
/// Eine Gerade der Länge `33.5mm`.
pub fn gerade_5109() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(33.5), "5109")
}
/// Eine Gerade der Länge `22.5mm`.
pub fn gerade_5110() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(22.5), "5110")
}
/// Eine Gerade der Länge `16mm`.
pub fn gerade_5210() -> GeradeUnit {
    Gerade::neu_mit_beschreibung(Länge::neu(16.), "5210")
}
/// Eine Gerade der Länge `8mm`.
pub fn gerade_5208() -> GeradeUnit {
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
/// Eine Kurve mit Winkel `45°` und Radius `286mm`.
pub fn kurve_5120() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_INDUSTRIE, WinkelGradmaß::neu(45.).into(), "5120")
}
/// Eine Kurve mit Winkel `30°` und Radius `360mm` (R1).
pub fn kurve_5100() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(30.).into(), "5100")
}
/// Eine Kurve mit Winkel `15°` und Radius `360mm` (R1).
pub fn kurve_5101() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(15.).into(), "5101")
}
/// Eine Kurve mit Winkel `7.5°` und Radius `360mm` (R1).
pub fn kurve_5102() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R1, WinkelGradmaß::neu(7.5).into(), "5102")
}
/// Eine Kurve mit Winkel `30°` und Radius `437.4mm` (R2).
pub fn kurve_5200() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(30.).into(), "5200")
}
/// Eine Kurve mit Winkel `24.28°` und Radius `437.4mm` (R2).
pub fn kurve_5206() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(24.28).into(), "5206")
}
/// Eine Kurve mit Winkel `15°` und Radius `437.4mm` (R2).
pub fn kurve_5201() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(15.).into(), "5201")
}
/// Eine Kurve mit Winkel `5.72°` und Radius `437.4mm` (R2).
pub fn kurve_5205() -> KurveUnit {
    Kurve::neu_mit_beschreibung(RADIUS_R2, WinkelGradmaß::neu(5.72).into(), "5205")
}

/*
Weiche
    5117 L/R: L180mm, 30°, R437.4mm
    5137 L/R: L180mm, 22.5°, R437.4mm
    5202 L/R: L180mm, 24.28°, R437.4mm
*/
/// Eine Weiche mit Länge `180mm`, Winkel `30°` und Radius `437.4mm` (R2).
pub fn weiche_5117(orientierung: Orientierung) -> WeicheUnit {
    let beschreibung = match orientierung {
        Orientierung::Links => "5117L",
        Orientierung::Rechts => "5117R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(30.).into(),
        orientierung,
        beschreibung,
    )
}
/// Eine Weiche mit Länge `180mm`, Winkel `30°` und Radius `437.4mm` (R2).
pub fn weiche_5117_rechts() -> WeicheUnit {
    weiche_5117(Orientierung::Rechts)
}
/// Eine Weiche mit Länge `180mm`, Winkel `30°` und Radius `437.4mm` (R2).
pub fn weiche_5117_links() -> WeicheUnit {
    weiche_5117(Orientierung::Links)
}
/// Eine Weiche mit Länge `180mm`, Winkel `22.5°` und Radius `437.4mm` (R2).
pub fn weiche_5137(richtung: Orientierung) -> WeicheUnit {
    let beschreibung = match richtung {
        Orientierung::Links => "5137L",
        Orientierung::Rechts => "5137R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(22.5).into(),
        richtung,
        beschreibung,
    )
}
/// Eine Weiche mit Länge `180mm`, Winkel `22.5°` und Radius `437.4mm` (R2).
pub fn weiche_5137_rechts() -> WeicheUnit {
    weiche_5137(Orientierung::Rechts)
}
/// Eine Weiche mit Länge `180mm`, Winkel `22.5°` und Radius `437.4mm` (R2).
pub fn weiche_5137_links() -> WeicheUnit {
    weiche_5137(Orientierung::Links)
}
/// Eine Weiche mit Länge `180mm`, Winkel `24.28°` und Radius `437.4mm` (R2).
pub fn weiche_5202(richtung: Orientierung) -> WeicheUnit {
    let beschreibung = match richtung {
        Orientierung::Links => "5202L",
        Orientierung::Rechts => "5202R",
    };
    WeicheUnit::neu_mit_beschreibung(
        Länge::neu(180.),
        RADIUS_R2,
        WinkelGradmaß::neu(24.28).into(),
        richtung,
        beschreibung,
    )
}
/// Eine Weiche mit Länge `180mm`, Winkel `24.28°` und Radius `437.4mm` (R2).
pub fn weiche_5202_rechts() -> WeicheUnit {
    weiche_5202(Orientierung::Rechts)
}
/// Eine Weiche mit Länge `180mm`, Winkel `24.28°` und Radius `437.4mm` (R2).
pub fn weiche_5202_links() -> WeicheUnit {
    weiche_5202(Orientierung::Links)
}

/*
Dreiwege-Weiche
    5214: L180mm, 24,28°, R437.4mm
*/
/// Eine Dreiwege-Weiche mit Länge `180mm`, Winkel `24.28°` und Radius `437.4mm` (R2).
pub fn dreiwege_weiche_5214() -> DreiwegeWeicheUnit {
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
/// Eine Kurven-Weiche mit Länge `77.4mm`, Winkel `30°` und Radius `360mm` (R1).
pub fn kurven_weiche_5140(orientierung: Orientierung) -> KurvenWeicheUnit {
    let beschreibung = match orientierung {
        Orientierung::Links => "5140L",
        Orientierung::Rechts => "5140R",
    };
    KurvenWeiche::neu_mit_beschreibung(
        Länge::neu(77.3),
        RADIUS_R1,
        WinkelGradmaß::neu(30.).into(),
        orientierung,
        beschreibung,
    )
}
/// Eine Kurven-Weiche mit Länge `77.4mm`, Winkel `30°` und Radius `360mm` (R1).
pub fn kurven_weiche_5140_rechts() -> KurvenWeicheUnit {
    kurven_weiche_5140(Orientierung::Rechts)
}
/// Eine Kurven-Weiche mit Länge `77.4mm`, Winkel `30°` und Radius `360mm` (R1).
pub fn kurven_weiche_5140_links() -> KurvenWeicheUnit {
    kurven_weiche_5140(Orientierung::Links)
}

/*
Kreuzung
    5128: L193mm, 30°, R360mm
    5207: L180mm, 24.28°, R437.4mm
*/
/// Eine schaltbare Kreuzung mit Länge `193mm`, Winkel `30°` und Radius `360mm` (R1).
pub fn kreuzung_5128() -> KreuzungUnit {
    Kreuzung::neu_mit_beschreibung(
        Länge::neu(193.),
        RADIUS_R1,
        kreuzung::Variante::MitKurve,
        "5128",
    )
}
// Länge/Winkel 24.28 passt nicht!
// https://www.stummiforum.de/viewtopic.php?t=29741#p309938
/// Eine schaltbare Kreuzung mit Länge `180mm`, Winkel `23,254°` und Radius `437.4mm` (R2).
pub fn kreuzung_5207() -> KreuzungUnit {
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
