//! Serialisierte Strukturen von Version 4.X.

use std::{collections::HashMap, fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::{Gerade, GeradeUnit},
        gleise::{id, steuerung::MitSteuerung},
        kreuzung::{Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert, Leiter},
        plan::{self, PlanSerialisiert},
        streckenabschnitt::{self, StreckenabschnittSerialisiert},
    },
    typen::{canvas::Position, mm::Spurweite},
    util::eingeschränkt::NichtNegativ,
};

pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert = HashMap<
    streckenabschnitt::Name,
    (StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>),
>;
pub(in crate::gleis::gleise::daten) type GeschwindigkeitMapSerialisiert<LeiterSerialisiert> =
    HashMap<geschwindigkeit::Name, GeschwindigkeitSerialisiert<LeiterSerialisiert>>;

#[derive(zugkontrolle_macros::Debug, Serialize, Deserialize)]
#[zugkontrolle_debug(L: Debug)]
#[zugkontrolle_debug(S: Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[zugkontrolle_debug(<L as Leiter>::Fahrtrichtung: Debug)]
#[serde(bound(
    serialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize, <L as Leiter>::Fahrtrichtung: Serialize, S: Serialize",
    deserialize = "L: Leiter, <L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>, <L as Leiter>::Fahrtrichtung: Deserialize<'de>, S: Deserialize<'de>",
))]
pub(in crate::gleis::gleise) struct ZustandSerialisiert<L: Leiter, S> {
    pub(crate) zugtyp: ZugtypSerialisiert<L>,
    pub(crate) geschwindigkeiten: GeschwindigkeitMapSerialisiert<S>,
    pub(crate) streckenabschnitte: StreckenabschnittMapSerialisiert,
    pub(crate) gleise: GleiseDatenSerialisiert,
    pub(crate) pläne: HashMap<plan::Name, PlanSerialisiert<L, S>>,
}

type GleisMapSerialisiert<T> = HashMap<id::Repräsentation, GleisSerialisiert<T>>;

/// Definition und Position eines Gleises.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
#[zugkontrolle_debug(<T as MitSteuerung>::Serialisiert: Debug)]
#[zugkontrolle_clone(<T as MitSteuerung>::Serialisiert: Clone)]
#[serde(bound(
    serialize = "T: MitSteuerung, <T as MitSteuerung>::Serialisiert: Serialize",
    deserialize = "T: MitSteuerung, <T as MitSteuerung>::Serialisiert: Deserialize<'de>",
))]
pub struct GleisSerialisiert<T: MitSteuerung> {
    /// Die [`Zeichnen`]-Definition des Gleises.
    pub definition: id::Repräsentation,
    /// Die [`Anschlüsse`](anschluss::Anschluss) des Gleises.
    pub steuerung: <T as MitSteuerung>::Serialisiert,
    /// Die Position des Gleises auf dem [`Canvas`](iced::widget::canvas::Canvas).
    pub position: Position,
    /// Der [`Streckenabschnitt`] des Gleises.
    pub streckenabschnitt: Option<streckenabschnitt::Name>,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct GleiseDatenSerialisiert {
    pub(crate) geraden: GleisMapSerialisiert<Gerade>,
    pub(crate) kurven: GleisMapSerialisiert<Kurve>,
    pub(crate) weichen: GleisMapSerialisiert<Weiche>,
    pub(crate) dreiwege_weichen: GleisMapSerialisiert<DreiwegeWeiche>,
    pub(crate) kurven_weichen: GleisMapSerialisiert<KurvenWeiche>,
    pub(crate) s_kurven_weichen: GleisMapSerialisiert<SKurvenWeiche>,
    pub(crate) kreuzungen: GleisMapSerialisiert<Kreuzung>,
}

impl GleiseDatenSerialisiert {
    pub(crate) fn verschmelze(&mut self, andere: GleiseDatenSerialisiert) {
        macro_rules! extend {
            ($($gleis_art: ident),*) => {$(
                self.$gleis_art.extend(andere.$gleis_art);
            )*};
        }
        extend!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        );
    }
}

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
#[serde(bound(
    serialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Serialize, <L as Leiter>::UmdrehenZeit: Serialize",
    deserialize = "<L as Leiter>::VerhältnisFahrspannungÜberspannung: Deserialize<'de>, <L as Leiter>::UmdrehenZeit: Deserialize<'de>",
))]
pub struct ZugtypSerialisiert<L: Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Der [Name der Leiter-Art](BekannterLeiter::NAME) des Zugtyps.
    pub leiter: String,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [`Geraden`](crate::gleis::gerade::Gerade).
    pub geraden: HashMap<u32, GeradeUnit>,
    /// Alle unterstützten [`Kurven`](crate::gleis::kurve::Kurve).
    pub kurven: HashMap<u32, KurveUnit>,
    /// Alle unterstützten [`Weichen`](crate::gleis::weiche::gerade::Weiche).
    pub weichen: HashMap<u32, WeicheUnit>,
    /// Alle unterstützten [`Dreiwege-Weichen`](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: HashMap<u32, DreiwegeWeicheUnit>,
    /// Alle unterstützten [`Kurven-Weichen`](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: HashMap<u32, KurvenWeicheUnit>,
    /// Alle unterstützten [`S-Kurven-Weichen`](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: HashMap<u32, SKurvenWeicheUnit>,
    /// Alle unterstützten [`Kreuzungen`](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: HashMap<u32, KreuzungUnit>,
    /// Frequenz in Herz für den Pwm-Antrieb.
    pub pwm_frequenz: NichtNegativ,
    /// Verhältnis von maximaler Fahrspannung zu Überspannung zum Umdrehen.
    pub verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    /// Zeit zum Anhalten vor dem Umdrehen.
    pub stopp_zeit: Duration,
    /// Zeit die zum Umdrehen verwendete Überspannung anliegt.
    pub umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    /// Zeit die Spannung an Weichen anliegt um diese zu schalten.
    pub schalten_zeit: Duration,
}
