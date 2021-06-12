//! Zugtyp Trait + Phantom-Typen + Spurweite

use std::fmt::Debug;

use crate::anschluss::serde::ToSave;
use crate::application::geschwindigkeit::LeiterAnzeige;
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
pub trait Zugtyp: Sized
where
    <<Self as Zugtyp>::Leiter as ToSave>::Save: Debug + Clone,
{
    /// Spurweite in mm.
    const SPURWEITE: Spurweite;
    const NAME: &'static str;

    /// Art der Stromzufuhr.
    type Leiter: ToSave + LeiterAnzeige;

    fn geraden() -> Vec<GeradeUnit<Self>>;
    fn kurven() -> Vec<KurveUnit<Self>>;
    fn weichen() -> Vec<WeicheUnit<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeicheUnit<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeicheUnit<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeicheUnit<Self>>;
    fn kreuzungen() -> Vec<KreuzungUnit<Self>>;
}
