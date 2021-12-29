//! Zugtyp + Spurweite

use std::marker::PhantomData;

use serde::{Deserialize, Serialize};

use crate::application::{gleis::*, typen::Spurweite};

pub mod lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone, Serialize, Deserialize)]
pub struct Zugtyp<Leiter> {
    name: String,
    leiter: PhantomData<fn() -> Leiter>,
    spurweite: Spurweite,
    geraden: Vec<GeradeUnit>,
    kurven: Vec<KurveUnit>,
    weichen: Vec<WeicheUnit>,
    dreiwege_weichen: Vec<DreiwegeWeicheUnit>,
    kurven_weichen: Vec<KurvenWeicheUnit>,
    s_kurven_weichen: Vec<SKurvenWeicheUnit>,
    kreuzungen: Vec<KreuzungUnit>,
}
