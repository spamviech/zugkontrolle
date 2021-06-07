//! Zugtyp Trait + Phantom-Typen + Spurweite

use crate::anschluss::serde::{Reserviere, ToSave};
use crate::application::gleis::*;

pub mod lego;
pub use lego::Lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;
pub use märklin::Märklin;
use serde::{Deserialize, Serialize};

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp: Sized {
    /// Spurweite in mm.
    const SPURWEITE: Spurweite;
    const NAME: &'static str;

    /// Art der Stromzufuhr.
    type Leiter: ToSave<Self::LeiterSave>;
    type LeiterSave: Reserviere<Self::Leiter> + Serialize + for<'de> Deserialize<'de>;

    fn geraden() -> Vec<GeradeUnit<Self>>;
    fn kurven() -> Vec<KurveUnit<Self>>;
    fn weichen() -> Vec<WeicheUnit<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeicheUnit<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeicheUnit<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeicheUnit<Self>>;
    fn kreuzungen() -> Vec<KreuzungUnit<Self>>;
}
