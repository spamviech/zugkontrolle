//! Alle Eigenschaften und bekannte Gleise für einen [Zugtyp].

use std::{fmt::Debug, marker::PhantomData, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    eingeschränkt::NichtNegativ,
    gleis::{
        gerade::GeradeUnit,
        kreuzung::KreuzungUnit,
        kurve::KurveUnit,
        weiche::{
            dreiwege::DreiwegeWeicheUnit, gerade::WeicheUnit, kurve::KurvenWeicheUnit,
            s_kurve::SKurvenWeicheUnit,
        },
    },
    steuerung::geschwindigkeit::{BekannterLeiter, Leiter},
    typen::mm::Spurweite,
};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct Zugtyp<L: Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Die Leiter-Art des Zugtyps.
    pub leiter: PhantomData<fn() -> L>,
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

/// Der Leiter stimmt nicht mit dem Namen überein.
#[derive(Debug, Clone)]
pub struct FalscherLeiter(pub String);

impl<L: BekannterLeiter> TryFrom<ZugtypSerialisiert<L>> for Zugtyp<L> {
    type Error = FalscherLeiter;

    fn try_from(serialisiert: ZugtypSerialisiert<L>) -> Result<Self, Self::Error> {
        let ZugtypSerialisiert {
            name,
            leiter,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        } = serialisiert;
        let gesucht = L::NAME.to_owned();
        if leiter != gesucht {
            return Err(FalscherLeiter(leiter));
        }
        Ok(Zugtyp {
            name,
            leiter: PhantomData,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        })
    }
}

impl<L: BekannterLeiter> From<Zugtyp<L>> for ZugtypSerialisiert<L> {
    fn from(zugtyp: Zugtyp<L>) -> Self {
        let Zugtyp {
            name,
            leiter: _,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        } = zugtyp;
        let leiter = L::NAME.to_owned();
        ZugtypSerialisiert {
            name,
            leiter,
            spurweite,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
            schalten_zeit,
        }
    }
}
