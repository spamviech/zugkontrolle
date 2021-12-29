//! Zugtyp + Spurweite

use std::{fmt::Debug, marker::PhantomData};

use serde::{Deserialize, Serialize};

use crate::application::gleis::*;

pub mod lego;
pub use lego::Lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;
pub use märklin::Märklin;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);

/// Spurweite, Leitervariante (als Phantomtyp) und alle bekannten Gleise
#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone, Serialize, Deserialize)]
pub struct Zugtyp<Leiter> {
    name: String,
    leiter: PhantomData<fn() -> Leiter>,
    spurweite: Spurweite,
    geraden: Vec<GeradeUnit<Self>>,
    kurven: Vec<KurveUnit<Self>>,
    weichen: Vec<WeicheUnit<Self>>,
    dreiwege_weichen: Vec<DreiwegeWeicheUnit<Self>>,
    kurven_weichen: Vec<KurvenWeicheUnit<Self>>,
    s_kurven_weichen: Vec<SKurvenWeicheUnit<Self>>,
    kreuzungen: Vec<KreuzungUnit<Self>>,
}
