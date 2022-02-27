//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{polarität::Fließend, trigger::Trigger},
    gleis::weiche,
    steuerung::{
        geschwindigkeit::{
            Fahrtrichtung, Geschwindigkeit, GeschwindigkeitSerialisiert, Mittelleiter, Zweileiter,
        },
        kontakt::{Kontakt, KontaktSerialisiert},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
        weiche::{Weiche, WeicheSerialisiert},
    },
};

/// Plan für einen automatischen Fahrplan.
#[derive(Debug, Serialize, Deserialize)]
pub struct Plan<Aktion = self::Aktion> {
    pub aktionen: Vec<Aktion>,
    pub endlosschleife: bool,
}

/// Plan für einen automatischen Fahrplan.
pub type PlanSerialisiert = Plan<AktionSerialisiert>;

/// Eine Aktionen in einem Fahrplan.
#[derive(Debug, Clone)]
pub enum Aktion {
    Geschwindigkeit(AktionGeschwindigkeit),
    Streckenabschnitt(AktionStreckenabschnitt),
    Schalten(AktionSchalten),
    Warten(AktionWarten),
}

/// Eine Aktionen in einem Fahrplan.
#[derive(Debug, Serialize, Deserialize)]
pub enum AktionSerialisiert {
    Geschwindigkeit(AktionGeschwindigkeitSerialisiert),
    Streckenabschnitt(AktionStreckenabschnittSerialisiert),
    Schalten(AktionSchaltenSerialisiert),
    Warten(AktionWartenSerialisiert),
}

/// Eine Aktion mit einer Geschwindigkeit.
#[derive(Debug, Clone)]
pub enum AktionGeschwindigkeit {
    GeschwindigkeitMittelleiter { leiter: Geschwindigkeit<Mittelleiter>, wert: u8 },
    GeschwindigkeitZweileiter { leiter: Geschwindigkeit<Zweileiter>, wert: u8 },
    UmdrehenMittelleiter { leiter: Geschwindigkeit<Mittelleiter> },
    FahrtrichtungZweileiter { leiter: Geschwindigkeit<Zweileiter>, fahrtrichtung: Fahrtrichtung },
}

/// Eine Aktion mit einer Geschwindigkeit.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionGeschwindigkeitSerialisiert {
    GeschwindigkeitMittelleiter {
        leiter: GeschwindigkeitSerialisiert<Mittelleiter>,
        wert: u8,
    },
    GeschwindigkeitZweileiter {
        leiter: GeschwindigkeitSerialisiert<Zweileiter>,
        wert: u8,
    },
    UmdrehenMittelleiter {
        leiter: GeschwindigkeitSerialisiert<Mittelleiter>,
    },
    FahrtrichtungZweileiter {
        leiter: GeschwindigkeitSerialisiert<Zweileiter>,
        fahrtrichtung: Fahrtrichtung,
    },
}

/// Eine Aktion mit einem Streckenabschnitt.
#[derive(Debug, Clone)]
pub enum AktionStreckenabschnitt {
    Streckenabschnitt { streckenabschnitt: Streckenabschnitt, fließend: Fließend },
}

/// Eine Aktion mit einem Streckenabschnitt.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionStreckenabschnittSerialisiert {
    Streckenabschnitt { streckenabschnitt: StreckenabschnittSerialisiert, fließend: Fließend },
}

/// Eine Aktion mit einer Weiche.
#[derive(Debug, Clone)]
pub enum AktionSchalten {
    SchalteGerade {
        weiche: Weiche<weiche::gerade::Richtung, weiche::gerade::RichtungAnschlüsse>,
        richtung: weiche::gerade::Richtung,
    },
    SchalteKurve {
        weiche: Weiche<weiche::kurve::Richtung, weiche::kurve::RichtungAnschlüsse>,
        richtung: weiche::gerade::Richtung,
    },
    SchalteDreiwege {
        weiche: Weiche<weiche::dreiwege::Richtung, weiche::dreiwege::RichtungAnschlüsse>,
        richtung: weiche::gerade::Richtung,
    },
}

/// Eine Aktion mit einer Weiche.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionSchaltenSerialisiert {
    SchalteGerade {
        weiche: WeicheSerialisiert<
            weiche::gerade::Richtung,
            weiche::gerade::RichtungAnschlüsseSerialisiert,
        >,
        richtung: weiche::gerade::Richtung,
    },
    SchalteKurve {
        weiche: WeicheSerialisiert<
            weiche::kurve::Richtung,
            weiche::kurve::RichtungAnschlüsseSerialisiert,
        >,
        richtung: weiche::gerade::Richtung,
    },
    SchalteDreiwege {
        weiche: WeicheSerialisiert<
            weiche::dreiwege::Richtung,
            weiche::dreiwege::RichtungAnschlüsseSerialisiert,
        >,
        richtung: weiche::gerade::Richtung,
    },
}

/// Eine Warte-Aktion.
#[derive(Debug, Clone)]
pub enum AktionWarten {
    WartenAuf { kontakt: Kontakt, trigger: Trigger },
    WartenFür { zeit: Duration },
}

/// Eine Warte-Aktion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWartenSerialisiert {
    WartenAuf { kontakt: KontaktSerialisiert, trigger: Trigger },
    WartenFür { zeit: Duration },
}
