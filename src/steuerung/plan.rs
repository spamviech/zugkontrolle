//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::{fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{de_serialisieren::Serialisiere, polarität::Fließend, trigger::Trigger},
    gleis::weiche,
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter},
        kontakt::{Kontakt, KontaktSerialisiert},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
        weiche::{Weiche, WeicheSerialisiert},
    },
};

/// Plan für einen automatischen Fahrplan.
#[derive(Debug, Serialize, Deserialize)]
pub struct Plan<Aktion> {
    pub aktionen: Vec<Aktion>,
    pub endlosschleife: bool,
}

/// Eine Aktionen in einem Fahrplan.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(<L as Leiter>::Fahrtrichtung: Clone)]
pub enum Aktion<L: Leiter> {
    Geschwindigkeit(AktionGeschwindigkeit<L>),
    Streckenabschnitt(AktionStreckenabschnitt),
    Schalten(AktionSchalten),
    Warten(AktionWarten),
}

/// Eine Aktionen in einem Fahrplan.
#[derive(zugkontrolle_macros::Debug, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Serialisiere + Leiter, L::Serialisiert: Debug, L::Fahrtrichtung: Debug)]
#[serde(bound(
    serialize = "L::Fahrtrichtung: Serialize",
    deserialize = "L::Fahrtrichtung: Deserialize<'de>",
))]
pub enum AktionSerialisiert<L>
where
    L: Serialisiere + Leiter,
{
    Geschwindigkeit(AktionGeschwindigkeitSerialisiert<L>),
    Streckenabschnitt(AktionStreckenabschnitt<StreckenabschnittSerialisiert>),
    Schalten(AktionSchaltenSerialisiert),
    Warten(AktionWarten<KontaktSerialisiert>),
}

/// Eine Aktion mit einer [Geschwindigkeit].
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(L: Debug, <L as Leiter>::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(<L as Leiter>::Fahrtrichtung: Clone)]
pub enum AktionGeschwindigkeit<L: Leiter> {
    Geschwindigkeit { leiter: Geschwindigkeit<L>, wert: u8 },
    Umdrehen { leiter: Geschwindigkeit<L> },
    Fahrtrichtung { leiter: Geschwindigkeit<L>, fahrtrichtung: <L as Leiter>::Fahrtrichtung },
}

/// Eine Aktion mit einer Geschwindigkeit.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Serialisiere + Leiter, L::Serialisiert: Debug, L::Fahrtrichtung: Debug)]
#[zugkontrolle_clone(L: Serialisiere + Leiter, L::Serialisiert: Clone, L::Fahrtrichtung: Clone)]
#[serde(bound(
    serialize = "L::Fahrtrichtung: Serialize",
    deserialize = "L::Fahrtrichtung: Deserialize<'de>",
))]
pub enum AktionGeschwindigkeitSerialisiert<L>
where
    L: Serialisiere + Leiter,
{
    Geschwindigkeit {
        leiter: GeschwindigkeitSerialisiert<L>,
        wert: u8,
    },
    Umdrehen {
        leiter: GeschwindigkeitSerialisiert<L>,
    },
    Fahrtrichtung {
        leiter: GeschwindigkeitSerialisiert<L>,
        fahrtrichtung: <L as Leiter>::Fahrtrichtung,
    },
}

/// Eine Aktion mit einem Streckenabschnitt.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionStreckenabschnitt<S = Streckenabschnitt> {
    Streckenabschnitt { streckenabschnitt: S, fließend: Fließend },
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWarten<K = Kontakt> {
    WartenAuf { kontakt: K, trigger: Trigger },
    WartenFür { zeit: Duration },
}
