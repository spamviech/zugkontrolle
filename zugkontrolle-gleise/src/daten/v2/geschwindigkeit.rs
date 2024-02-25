//! Serialisierbare Darstellung einer [`Geschwindigkeit`] in Version 2.

use std::collections::HashMap;

use serde::Deserialize;

use zugkontrolle_anschluss::{pin::pwm, polarität::Polarität};
use zugkontrolle_gleis::{
    steuerung::geschwindigkeit::{self as v4, BekannterLeiter, Mittelleiter, Zweileiter},
    zugtyp::Zugtyp,
};

use crate::daten::{v2::anschluss::OutputSerialisiert, v3, v4::ZugtypSerialisiert};

/// Ein unterstützten Leiter, mit über Namen identifizierten Zugtypen. Aktuell:
/// - [`Mittelleiter`] mit "Märklin".
/// - [`Zweileiter`] mit "Lego".
pub trait BekannterZugtyp: BekannterLeiter {
    /// Serialisierbare Repräsentation in v2.*
    type V2;

    /// Erzeuge einen Zugtyp mit der entsprechenden Leiter-Art, ausgehend von seinem Namen.
    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>>;
}

/// Nicht-leerer Vektor.
///
/// Explizit definiert, anstelle [`nonempty::NonEmpty`],
/// damit die [`Deserialize`]-Implementierung übereinstimmt.
#[derive(Deserialize)]
pub(in crate::daten::v2) struct NonEmpty<T> {
    /// Das erste Element.
    head: T,
    /// Alle folgenden Elemente.
    tail: Vec<T>,
}

/// Serialisierbare Repräsentation eines [`Mittelleiters`](Mittelleiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct MittelleiterSerialisiert(MittelleiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [`Mittelleiters`](Mittelleiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
pub(in crate::daten::v2) enum MittelleiterSerialisiertEnum {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        pin: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputSerialisiert>,
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Der Anschluss mit Überspannung zum Umdrehen der Fahrtrichtung.
        umdrehen: OutputSerialisiert,
    },
}

impl BekannterZugtyp for Mittelleiter {
    type V2 = MittelleiterSerialisiert;

    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>> {
        (name == "Märklin").then(|| ZugtypSerialisiert::from(Zugtyp::märklin()).v3())
    }
}

impl From<MittelleiterSerialisiert> for v4::MittelleiterSerialisiert {
    fn from(input: MittelleiterSerialisiert) -> Self {
        match input.0 {
            MittelleiterSerialisiertEnum::Pwm { pin, polarität } => {
                v4::MittelleiterSerialisiert::Pwm { pin, polarität }
            },
            MittelleiterSerialisiertEnum::KonstanteSpannung {
                geschwindigkeit: NonEmpty { head, tail },
                letzter_wert: _,
                umdrehen,
            } => v4::MittelleiterSerialisiert::KonstanteSpannung {
                geschwindigkeit: nonempty::NonEmpty {
                    head: head.into(),
                    tail: tail.into_iter().map(Into::into).collect(),
                },
                umdrehen: umdrehen.into(),
            },
        }
    }
}

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Deserialize)]
#[allow(missing_debug_implementations)]
pub struct ZweileiterSerialisiert(ZweileiterSerialisiertEnum);

/// Serialisierbare Repräsentation eines [`Zweileiters`](Zweileiter).
#[derive(Deserialize)]
#[allow(variant_size_differences)]
pub(in crate::daten::v2) enum ZweileiterSerialisiertEnum {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Der [`Pwm-Pin`](pwm::Pin).
        geschwindigkeit: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Die Anschlüsse.
        geschwindigkeit: NonEmpty<OutputSerialisiert>,
        /// Der letzte eingestellte Wert.
        #[allow(dead_code)]
        letzter_wert: usize,
        /// Anschluss zur Steuerung der Fahrtrichtung.
        fahrtrichtung: OutputSerialisiert,
    },
}

impl BekannterZugtyp for Zweileiter {
    type V2 = ZweileiterSerialisiert;

    fn bekannter_zugtyp(name: &str) -> Option<v3::zugtyp::ZugtypSerialisiert<Self>> {
        (name == "Lego").then(|| ZugtypSerialisiert::from(Zugtyp::lego()).v3())
    }
}

impl From<ZweileiterSerialisiert> for v4::ZweileiterSerialisiert {
    fn from(input: ZweileiterSerialisiert) -> Self {
        match input.0 {
            ZweileiterSerialisiertEnum::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                v4::ZweileiterSerialisiert::Pwm {
                    geschwindigkeit,
                    polarität,
                    fahrtrichtung: fahrtrichtung.into(),
                }
            },
            ZweileiterSerialisiertEnum::KonstanteSpannung {
                geschwindigkeit: NonEmpty { head, tail },
                letzter_wert: _,
                fahrtrichtung,
            } => v4::ZweileiterSerialisiert::KonstanteSpannung {
                geschwindigkeit: nonempty::NonEmpty {
                    head: head.into(),
                    tail: tail.into_iter().map(Into::into).collect(),
                },
                fahrtrichtung: fahrtrichtung.into(),
            },
        }
    }
}

/// Serialisierbare Repräsentation einer [`Geschwindigkeit`](v4::Geschwindigkeit).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct GeschwindigkeitSerialisiert<LeiterV2> {
    /// Der Leiter der Geschwindigkeit.
    leiter: LeiterV2,
}

impl<LeiterV2, LeiterSerialisiert> From<GeschwindigkeitSerialisiert<LeiterV2>>
    for v4::GeschwindigkeitSerialisiert<LeiterSerialisiert>
where
    LeiterV2: Into<LeiterSerialisiert>,
{
    fn from(input: GeschwindigkeitSerialisiert<LeiterV2>) -> Self {
        v4::GeschwindigkeitSerialisiert { leiter: input.leiter.into() }
    }
}

/// Geschwindigkeiten mit ihrem Namen.
pub(in crate::daten::v2) type GeschwindigkeitMapSerialisiert<LeiterV2> =
    HashMap<Name, GeschwindigkeitSerialisiert<LeiterV2>>;

/// Name einer [`Geschwindigkeit`](GeschwindigkeitSerialisiert).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct Name(pub String);

impl From<Name> for v4::Name {
    fn from(wert: Name) -> Self {
        v4::Name(wert.0)
    }
}
