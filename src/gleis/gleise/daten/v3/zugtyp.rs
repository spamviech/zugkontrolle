//! Serialisierbare Darstellung eines [Zugtyps](crate::zugtyp::Zugtyp) in Version 3.

use std::{fmt::Debug, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::GeradeUnit,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
            s_kurve::SKurvenWeicheUnit,
        },
    },
    steuerung::geschwindigkeit::Leiter,
    typen::mm::Spurweite,
    util::eingeschränkt::NichtNegativ,
};

// TODO <Gleis>Unit auch in v3 definieren

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
    /// Alle unterstützten [Geraden](crate::gleis::gerade::Gerade).
    pub geraden: Vec<GeradeUnit>,
    /// Alle unterstützten [Kurven](crate::gleis::kurve::Kurve).
    pub kurven: Vec<KurveUnit>,
    /// Alle unterstützten [Weichen](crate::gleis::weiche::gerade::Weiche).
    pub weichen: Vec<WeicheUnit>,
    /// Alle unterstützten [Dreiwege-Weichen](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    /// Alle unterstützten [Kurven-Weichen](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    /// Alle unterstützten [S-Kurven-Weichen](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    /// Alle unterstützten [Kreuzungen](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: Vec<KreuzungUnit>,
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
