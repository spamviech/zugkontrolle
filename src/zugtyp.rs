//! Alle Eigenschaften und bekannte Gleise für einen [Zugtyp].

use std::{collections::HashMap, fmt::Debug, marker::PhantomData, time::Duration};

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gerade::{Gerade, GeradeUnit},
        gleise::{
            id::{eindeutig::KeineIdVerfügbar, DefinitionId2, GleisId2},
            steuerung::MitSteuerung,
        },
        kreuzung::{Kreuzung, KreuzungUnit},
        kurve::{Kurve, KurveUnit},
        weiche::{
            dreiwege::{DreiwegeWeiche, DreiwegeWeicheUnit},
            gerade::{Weiche, WeicheUnit},
            kurve::{KurvenWeiche, KurvenWeicheUnit},
            s_kurve::{SKurvenWeiche, SKurvenWeicheUnit},
        },
    },
    steuerung::geschwindigkeit::{BekannterLeiter, Leiter},
    typen::mm::Spurweite,
    util::eingeschränkt::NichtNegativ,
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

pub(crate) type DefinitionMap2<T> = HashMap<DefinitionId2<T>, <T as MitSteuerung>::SelfUnit>;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct Zugtyp2<L: Leiter> {
    /// Der Name des Zugtyps.
    pub name: String,
    /// Die Leiter-Art des Zugtyps.
    pub leiter: PhantomData<fn() -> L>,
    /// Spurweite
    pub spurweite: Spurweite,
    /// Alle unterstützten [Geraden](crate::gleis::gerade::Gerade).
    pub geraden: DefinitionMap2<Gerade>,
    /// Alle unterstützten [Kurven](crate::gleis::kurve::Kurve).
    pub kurven: DefinitionMap2<Kurve>,
    /// Alle unterstützten [Weichen](crate::gleis::weiche::gerade::Weiche).
    pub weichen: DefinitionMap2<Weiche>,
    /// Alle unterstützten [Dreiwege-Weichen](crate::gleis::weiche::dreiwege::DreiwegeWeiche).
    pub dreiwege_weichen: DefinitionMap2<DreiwegeWeiche>,
    /// Alle unterstützten [Kurven-Weichen](crate::gleis::weiche::kurve::KurvenWeiche).
    pub kurven_weichen: DefinitionMap2<KurvenWeiche>,
    /// Alle unterstützten [S-Kurven-Weichen](crate::gleis::weiche::s_kurve::SKurvenWeiche).
    pub s_kurven_weichen: DefinitionMap2<SKurvenWeiche>,
    /// Alle unterstützten [Kreuzungen](crate::gleis::kreuzung::Kreuzung).
    pub kreuzungen: DefinitionMap2<Kreuzung>,
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
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum ZugtypDeserialisierenFehler {
    FalscherLeiter(String),
    KeineIdVerfügbar(KeineIdVerfügbar),
}

macro_rules! erzeuge_maps {
    ($($gleise: ident : $typ: ty),* $(,)?) => {$(
        #[allow(unused_qualifications)]
        let $gleise = $gleise
            .into_iter()
            .map(|definition| Ok((crate::gleis::gleise::id::DefinitionId2::<$typ>::neu()?, definition)) )
            .collect::<Result<crate::zugtyp::DefinitionMap2<$typ>, crate::zugtyp::ZugtypDeserialisierenFehler>>()?;
    )*};
    ($($gleise: ident : $typ: ty | $expect_msg: literal),* $(,)?) => {$(
        #[allow(unused_qualifications)]
        let $gleise = $gleise
            .into_iter()
            .map(|definition| Ok((crate::gleis::gleise::id::DefinitionId2::<$typ>::neu()?, definition)) )
            .collect::<Result<crate::zugtyp::DefinitionMap2<$typ>, crate::zugtyp::ZugtypDeserialisierenFehler>>().expect($expect_msg);
    )*};
}
use erzeuge_maps;

impl<L: BekannterLeiter> TryFrom<ZugtypSerialisiert<L>> for Zugtyp2<L> {
    type Error = ZugtypDeserialisierenFehler;

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
            return Err(ZugtypDeserialisierenFehler::FalscherLeiter(leiter));
        }
        erzeuge_maps!(
            geraden: Gerade,
            kurven: Kurve,
            weichen: Weiche,
            dreiwege_weichen: DreiwegeWeiche,
            kurven_weichen: KurvenWeiche,
            s_kurven_weichen: SKurvenWeiche,
            kreuzungen: Kreuzung,
        );
        Ok(Zugtyp2 {
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

impl<L: BekannterLeiter> From<Zugtyp2<L>> for ZugtypSerialisiert<L> {
    fn from(zugtyp: Zugtyp2<L>) -> Self {
        let Zugtyp2 {
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
        } = zugtyp;
        let leiter = L::NAME.to_owned();
        macro_rules! erzeuge_vecs {
            ($($gleise: ident),* $(,)?) => {$(
                let $gleise = $gleise.into_values().collect();
            )*};
        }
        erzeuge_vecs!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );
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
