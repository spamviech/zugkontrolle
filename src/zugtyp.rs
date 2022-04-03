//! Alle Eigenschaften und bekannte Gleise für einen [Zugtyp].

use std::marker::PhantomData;

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
    typen::mm::Spurweite,
};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct Zugtyp<Leiter> {
    pub name: String,
    pub leiter: PhantomData<fn() -> Leiter>,
    pub spurweite: Spurweite,
    pub geraden: Vec<GeradeUnit>,
    pub kurven: Vec<KurveUnit>,
    pub weichen: Vec<WeicheUnit>,
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    pub kreuzungen: Vec<KreuzungUnit>,
}

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZugtypSerialisiert {
    pub name: String,
    pub leiter: String,
    pub spurweite: Spurweite,
    pub geraden: Vec<GeradeUnit>,
    pub kurven: Vec<KurveUnit>,
    pub weichen: Vec<WeicheUnit>,
    pub dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    pub kurven_weichen: Vec<KurvenWeicheUnit>,
    pub s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    pub kreuzungen: Vec<KreuzungUnit>,
}

pub trait BekannterLeiter: Sized {
    const NAME: &'static str;

    fn bekannter_zugtyp(name: &str) -> Option<Zugtyp<Self>>;
}

#[derive(Debug, Clone)]
pub struct FalscherLeiter(pub String);

impl<Leiter: BekannterLeiter> TryFrom<ZugtypSerialisiert> for Zugtyp<Leiter> {
    type Error = FalscherLeiter;

    fn try_from(serialisiert: ZugtypSerialisiert) -> Result<Self, Self::Error> {
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
        } = serialisiert;
        let gesucht = Leiter::NAME.to_owned();
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
        })
    }
}

impl<Leiter: BekannterLeiter> From<Zugtyp<Leiter>> for ZugtypSerialisiert {
    fn from(zugtyp: Zugtyp<Leiter>) -> Self {
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
        } = zugtyp;
        let leiter = Leiter::NAME.to_owned();
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
        }
    }
}
