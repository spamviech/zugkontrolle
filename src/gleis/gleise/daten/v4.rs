//! Serialisierte Strukturen von Version 4.X.

use std::{collections::HashMap, fmt::Debug};

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::Gerade,
        gleise::{
            id::{self},
            steuerung::MitSteuerung,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche, s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        geschwindigkeit::{self, GeschwindigkeitSerialisiert, Leiter},
        plan::{self, PlanSerialisiert},
        streckenabschnitt::{self},
    },
    typen::canvas::Position,
    zugtyp::ZugtypSerialisiert2,
};

pub(in crate::gleis::gleise::daten) type StreckenabschnittMapSerialisiert =
    HashMap<streckenabschnitt::Name, GleiseDatenSerialisiert>;
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
    pub(crate) zugtyp: ZugtypSerialisiert2<L>,
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
pub struct GleisSerialisiert<T: MitSteuerung>
where
    <T as MitSteuerung>::SelfUnit: 'static,
{
    /// Die [Zeichnen]-Definition des Gleises.
    pub definition: id::Repräsentation,
    /// Die [Anschlüsse](anschluss::Anschluss) des Gleises.
    pub steuerung: <T as MitSteuerung>::Serialisiert,
    /// Die Position des Gleises auf dem [Canvas](iced::widget::canvas::Canvas).
    pub position: Position,
    /// Der [Streckenabschnitt] des Gleises.
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
    pub(crate) fn neu() -> Self {
        GleiseDatenSerialisiert {
            geraden: GleisMapSerialisiert::new(),
            kurven: GleisMapSerialisiert::new(),
            weichen: GleisMapSerialisiert::new(),
            dreiwege_weichen: GleisMapSerialisiert::new(),
            kurven_weichen: GleisMapSerialisiert::new(),
            s_kurven_weichen: GleisMapSerialisiert::new(),
            kreuzungen: GleisMapSerialisiert::new(),
        }
    }
}
