//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        pin::pwm, trigger::Trigger, InputAnschluss, InputSerialisiert, OutputAnschluss,
        OutputSerialisiert,
    },
    steuerung::{
        geschwindigkeit::{Geschwindigkeit, GeschwindigkeitSerialisiert, Mittelleiter, Zweileiter},
        streckenabschnitt::{Streckenabschnitt, StreckenabschnittSerialisiert},
    },
};

// FIXME Benötigt eigenen Serialisiert-Typ
/// Plan für einen automatischen Fahrplan.
#[derive(Debug, Serialize, Deserialize)]
pub struct Plan<Aktion = self::Aktion> {
    pub aktionen: Vec<Aktion>,
    pub endlosschleife: bool,
}

pub type PlanSerialisiert = Plan<AktionSerialisiert>;

// TODO verwende Unter-Enums (z.B. Geschwindigkeit, Schalten, Warten, Strom)?
/// Eine Aktionen in einem Fahrplan.
///
/// Erstellen erster Aktionen führt nicht zwingend zu einem erhöhen der major version.
/// Daher ist das enum als non_exhaustive markiert.
#[non_exhaustive]
#[derive(Debug, Serialize, Deserialize)]
#[serde(bound(serialize = "Pwm: Clone + Serialize, Output: Clone + Serialize, Input: Serialize"))]
pub enum Aktion<Pwm = pwm::Pin, Output = OutputAnschluss, Input = InputAnschluss> {
    // TODO verwende Geschwindigkeit/GeschwindigkeitSerialisiert
    GeschwindigkeitMittelleiter { leiter: Mittelleiter<Pwm, Output>, wert: u8 },
    GeschwindigkeitZweileiter { leiter: Zweileiter<Pwm, Output>, wert: u8 },
    WartenAuf { anschluss: Input, trigger: Trigger },
    WartenFür { zeit: Duration },
}

pub type AktionSerialisiert = Aktion<pwm::Serialisiert, OutputSerialisiert, InputSerialisiert>;
