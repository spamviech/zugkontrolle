//! Serialisierbare Darstellung eines [`Plan`]s in Version 4.X.

use std::{fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use zugkontrolle_anschluss::polarität::Fließend;
use zugkontrolle_gleis::{
    steuerung::{
        kontakt::KontaktSerialisiert, plan::AktionSchalten,
        streckenabschnitt::StreckenabschnittSerialisiert,
    },
    weiche::{
        self, dreiwege::DreiwegeWeicheSerialisiert,
        gerade::WeicheSerialisiert as GeradeWeicheSerialisiert, kurve::KurvenWeicheSerialisiert,
    },
};

use crate::daten::v4::geschwindigkeit::{GeschwindigkeitSerialisiert, Leiter};

pub use zugkontrolle_gleis::steuerung::plan::Name;

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
