//! Zugtyp + Spurweite

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::application::{gleis::*, typen::Spurweite};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
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
