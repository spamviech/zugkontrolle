//! Serialisierbare Darstellung eines [`Plan`]s in Version 4.X.

#![expect(clippy::pub_use, reason = "Erlaube einfache Anpassung der Typsignaturen.")]

use std::{fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::polarität::Fließend;
use zugkontrolle_gleis::{
    steuerung::{
        geschwindigkeit::{Fahrtrichtung, LeiterSerialisiert},
        kontakt::KontaktSerialisiert,
        plan::{self as v6, AktionSchalten},
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    weiche::{
        self, dreiwege::DreiwegeWeicheSerialisiert,
        gerade::WeicheSerialisiert as GeradeWeicheSerialisiert, kurve::KurvenWeicheSerialisiert,
    },
};
use zugkontrolle_util::void::Void;

use crate::daten::v4::geschwindigkeit::{GeschwindigkeitSerialisiert, Leiter};

pub use zugkontrolle_gleis::steuerung::plan::Name;

/// Eine Weichen-Aktion eines älteren Plans besitzt keine Steuerung.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FehlendeWeichensteuerung {
    /// Die Art der Weiche.
    pub weichenart: &'static str,
    /// Der Name des Plans, falls bekannt.
    pub plan: Option<Name>,
}

#[expect(
    clippy::module_name_repetitions,
    reason = "Verwende Konvention TypName->TypNameSerialisiert"
)]
/// Serialisierbare Repräsentation eines Fahrplans.
pub type PlanSerialisiert<L, S> = PlanEnum<AktionSerialisiert<L, S>>;

#[expect(clippy::module_name_repetitions, reason = "Sollte nicht direkt verwendet werden.")]
/// Ein Fahrplan. Wird normalerweise über das [`Plan`]-alias verwendet.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanEnum<Aktion> {
    /// Alle [`Aktionen`](Aktion) des Plans.
    pub aktionen: Vec<Aktion>,
    /// Werden die Aktionen nach ausführen der letzten wiederholt?
    pub endlosschleife: bool,
}

impl<L: Leiter, S> TryFrom<PlanSerialisiert<L, S>> for v6::PlanSerialisiert
where
    AktionGeschwindigkeitSerialisiert<L, S>: Into<v6::AktionGeschwindigkeitSerialisiert>,
{
    type Error = FehlendeWeichensteuerung;

    fn try_from(value: PlanSerialisiert<L, S>) -> Result<Self, Self::Error> {
        Ok(v6::PlanEnum {
            aktionen: value
                .aktionen
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()?,
            endlosschleife: value.endlosschleife,
        })
    }
}

/// Serialisierbare Repräsentation einer Aktion in einem Fahrplan.
pub type AktionSerialisiert<L, S> = AktionEnum<
    AktionGeschwindigkeitSerialisiert<L, S>,
    AktionStreckenabschnittSerialisiert,
    AnyAktionSchaltenSerialisiert,
    AktionWartenSerialisiert,
>;

/// Eine Aktionen in einem Fahrplan.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionEnum<Geschwindigkeit, Streckenabschnitt, Schalten, Warten> {
    /// Eine [`AktionGeschwindigkeit`].
    Geschwindigkeit(Geschwindigkeit),
    /// Eine [`AktionStreckenabschnitt`].
    Streckenabschnitt(Streckenabschnitt),
    /// Eine [`AnyAktionSchalten`].
    Schalten(Schalten),
    /// Eine [`AktionWarten`].
    Warten(Warten),
    /// Ausführen eines [`Plans`](Plan).
    Ausführen(PlanEnum<Self>),
}

impl<L: Leiter, S> TryFrom<AktionSerialisiert<L, S>> for v6::AktionSerialisiert
where
    AktionGeschwindigkeitSerialisiert<L, S>: Into<v6::AktionGeschwindigkeitSerialisiert>,
{
    type Error = FehlendeWeichensteuerung;

    fn try_from(value: AktionSerialisiert<L, S>) -> Result<Self, Self::Error> {
        Ok(match value {
            AktionEnum::Geschwindigkeit(aktion) => v6::AktionEnum::Geschwindigkeit(aktion.into()),
            AktionEnum::Streckenabschnitt(aktion) => {
                v6::AktionEnum::Streckenabschnitt(aktion.into())
            },
            AktionEnum::Schalten(aktion) => v6::AktionEnum::Schalten(aktion.try_into()?),
            AktionEnum::Warten(aktion) => v6::AktionEnum::Warten(aktion.into()),
            AktionEnum::Ausführen(plan) => v6::AktionEnum::Ausführen(plan.try_into()?),
        })
    }
}

/// Erstelle den Datentyp für eine Geschwindigkeits-Aktion oder seine serialisierbare Repräsentation.
macro_rules! erstelle_aktion_geschwindigkeit {
    (
        $docstring: expr,
        $type: ident < $($par: ident $(: $con0: ident $(+ $con: ident)*)?),* >,
        $geschwindigkeit: ty,
        $fahrtrichtung: ty;
        Debug => $($debug: ty),*;
        Clone => $($clone: ty),*;
        $(
            [$serialize: ident, $deserialize: ident]
            $ser: expr;
            $de: expr;
        )?
    ) => {
        #[doc = $docstring]
        #[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone $(, $serialize, $deserialize)?)]
        $(
            #[zugkontrolle_debug($debug: Debug)]
        )*
        $(
            #[zugkontrolle_clone($clone: Clone)]
        )*
        $(#[serde(bound(serialize = $ser, deserialize = $de))])?
        pub enum $type < $($par $(: $con0 $(+ $con)*)? ),* > {
            /// Einstellen der Fahrgeschwindigkeit.
            Geschwindigkeit {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
                /// Die neue Geschwindigkeit.
                wert: u8,
            },
            /// Umdrehen der Fahrtrichtung.
            Umdrehen {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
            },
            /// Einstellen der Fahrtrichtung.
            Fahrtrichtung {
                /// Die Anschlüsse zur Steuerung der Geschwindigkeit.
                geschwindigkeit: $geschwindigkeit,
                /// Die neue Fahrtrichtung.
                fahrtrichtung: $fahrtrichtung,
            },
        }
    };
}

erstelle_aktion_geschwindigkeit! {
    "Serialisierbare Repräsentation für eine Aktion mit einer [Geschwindigkeit].",
    AktionGeschwindigkeitSerialisiert<L: Leiter, S>,
    GeschwindigkeitSerialisiert<S>,
    <L as Leiter>::Fahrtrichtung;
    Debug => S, <L as Leiter>::Fahrtrichtung;
    Clone => S, <L as Leiter>::Fahrtrichtung;
    [Serialize, Deserialize]
    "S: Serialize, <L as Leiter>::Fahrtrichtung: Serialize";
    "S: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>";
}

/// Helper-Trait to write generic code for a type or [`Void`].
trait FahrtrichtungOrVoid {
    /// Return the value
    fn fahrtrichtung(self) -> Fahrtrichtung;
}

impl FahrtrichtungOrVoid for Void {
    fn fahrtrichtung(self) -> Fahrtrichtung {
        self.unreachable()
    }
}

impl FahrtrichtungOrVoid for Fahrtrichtung {
    fn fahrtrichtung(self) -> Fahrtrichtung {
        self
    }
}

impl<L4, S4> From<AktionGeschwindigkeitSerialisiert<L4, S4>>
    for v6::AktionGeschwindigkeitSerialisiert
where
    L4: Leiter,
    <L4 as Leiter>::Fahrtrichtung: FahrtrichtungOrVoid,
    S4: Into<LeiterSerialisiert>,
{
    fn from(value: AktionGeschwindigkeitSerialisiert<L4, S4>) -> Self {
        match value {
            AktionGeschwindigkeitSerialisiert::Geschwindigkeit { geschwindigkeit, wert } => {
                v6::AktionGeschwindigkeitSerialisiert::Geschwindigkeit {
                    geschwindigkeit: geschwindigkeit.into(),
                    wert,
                }
            },
            AktionGeschwindigkeitSerialisiert::Umdrehen { geschwindigkeit } => {
                v6::AktionGeschwindigkeitSerialisiert::Umdrehen {
                    geschwindigkeit: geschwindigkeit.into(),
                }
            },
            AktionGeschwindigkeitSerialisiert::Fahrtrichtung { geschwindigkeit, fahrtrichtung } => {
                v6::AktionGeschwindigkeitSerialisiert::Fahrtrichtung {
                    geschwindigkeit: geschwindigkeit.into(),
                    fahrtrichtung: fahrtrichtung.fahrtrichtung(),
                }
            },
        }
    }
}

/// Eine Aktion mit einem [`Streckenabschnitt`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionStreckenabschnitt<S = StreckenabschnittSerialisiert> {
    /// Strom auf einem Streckenabschnitt einstellen.
    Strom {
        /// Die Anschlüsse zur Steuerung des Streckenabschnittes.
        streckenabschnitt: S,
        /// Wird der Strom an- oder abgestellt.
        fließend: Fließend,
    },
}

/// Serialisierbare Repräsentation einer Aktion mit einem [`Streckenabschnitt`].
pub type AktionStreckenabschnittSerialisiert =
    AktionStreckenabschnitt<StreckenabschnittSerialisiert>;

impl<S4: Into<S6>, S6> From<AktionStreckenabschnitt<S4>> for v6::AktionStreckenabschnitt<S6> {
    fn from(value: AktionStreckenabschnitt<S4>) -> Self {
        match value {
            AktionStreckenabschnitt::Strom { streckenabschnitt, fließend } => {
                v6::AktionStreckenabschnitt::Strom {
                    streckenabschnitt: streckenabschnitt.into(),
                    fließend,
                }
            },
        }
    }
}

/// Eine Aktion mit einer [`Weiche`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AnyAktionSchalten<
    Gerade = GeradeWeicheSerialisiert,
    Kurve = KurvenWeicheSerialisiert,
    Dreiwege = DreiwegeWeicheSerialisiert,
> {
    /// Schalten einer [Weiche](weiche::gerade::Weiche), [`SKurvenWeiche`](weiche::s_kurve::SKurvenWeiche)
    /// oder [`Kreuzung`](crate::gleis::kreuzung::Kreuzung).
    SchalteGerade(AktionSchalten<Gerade, weiche::gerade::Richtung>),
    /// Schalten einer [`KurvenWeiche`](weiche::kurve::KurvenWeiche).
    SchalteKurve(AktionSchalten<Kurve, weiche::kurve::Richtung>),
    /// Schalten einer [`DreiwegeWeiche`](weiche::dreiwege::DreiwegeWeiche).
    SchalteDreiwege(AktionSchalten<Dreiwege, weiche::dreiwege::Richtung>),
}

/// Serialisierbare Repräsentation für eine Aktion mit einer [`Weiche`].
pub type AnyAktionSchaltenSerialisiert = AnyAktionSchalten<
    GeradeWeicheSerialisiert,
    KurvenWeicheSerialisiert,
    DreiwegeWeicheSerialisiert,
>;

impl TryFrom<AnyAktionSchaltenSerialisiert> for v6::AnyAktionSchaltenSerialisiert {
    type Error = FehlendeWeichensteuerung;

    fn try_from(value: AnyAktionSchaltenSerialisiert) -> Result<Self, Self::Error> {
        let fehlende_steuerung = |weichenart| FehlendeWeichensteuerung { weichenart, plan: None };
        Ok(match value {
            AnyAktionSchalten::SchalteGerade(aktion) => {
                v6::AnyAktionSchalten::SchalteGerade(v6::AktionSchalten {
                    weiche: aktion.weiche.steuerung.ok_or_else(|| fehlende_steuerung("Gerade"))?,
                    richtung: aktion.richtung,
                })
            },
            AnyAktionSchalten::SchalteKurve(aktion) => {
                v6::AnyAktionSchalten::SchalteKurve(v6::AktionSchalten {
                    weiche: aktion.weiche.steuerung.ok_or_else(|| fehlende_steuerung("Kurve"))?,
                    richtung: aktion.richtung,
                })
            },
            AnyAktionSchalten::SchalteDreiwege(aktion) => {
                v6::AnyAktionSchalten::SchalteDreiwege(v6::AktionSchalten {
                    weiche: aktion
                        .weiche
                        .steuerung
                        .ok_or_else(|| fehlende_steuerung("Dreiwege"))?,
                    richtung: aktion.richtung,
                })
            },
        })
    }
}

/// Eine Warte-Aktion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AktionWarten<K = KontaktSerialisiert> {
    /// Warte auf das Auslösen eines [`Kontaktes`](Kontakt).
    WartenAuf {
        /// Die Anschlüsse des Kontaktes.
        kontakt: K,
    },
    /// Warte für eine festgelegte Zeit.
    /// Es kann vorkommen, dass etwas länger gewartet wird, siehe [`std::thread::sleep`].
    WartenFür {
        /// Die Wartezeit.
        zeit: Duration,
    },
}

/// Serialisierbare Repräsentation einer Warte-Aktion.
pub type AktionWartenSerialisiert = AktionWarten<KontaktSerialisiert>;

impl<K4: Into<K6>, K6> From<AktionWarten<K4>> for v6::AktionWarten<K6> {
    fn from(value: AktionWarten<K4>) -> Self {
        match value {
            AktionWarten::WartenAuf { kontakt } => {
                v6::AktionWarten::WartenAuf { kontakt: kontakt.into() }
            },
            AktionWarten::WartenFür { zeit } => v6::AktionWarten::WartenFür { zeit },
        }
    }
}
