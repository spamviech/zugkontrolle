//! Zugtyp Trait + Phantom-Typen + Spurweite

use std::fmt::{Debug, Display};

use crate::{
    anschluss::speichern::ToSave,
    application::{geschwindigkeit::LeiterAnzeige, gleis::*},
    steuerung::geschwindigkeit::GeschwindigkeitAnschluss,
};

pub mod lego;
pub use lego::Lego;
// path attribute necessary due to non-ascii module name (at least for now)
#[path = "zugtyp/märklin.rs"]
pub mod märklin;
pub use märklin::Märklin;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp: Sized {
    /// Spurweite in mm.
    const SPURWEITE: Spurweite;
    const NAME: &'static str;

    /// Art der Stromzufuhr.
    type Leiter: ToSave<GeschwindigkeitAnschluss> + LeiterAnzeige + Display;

    fn geraden() -> Vec<GeradeUnit<Self>>;
    fn kurven() -> Vec<KurveUnit<Self>>;
    fn weichen() -> Vec<WeicheUnit<Self>>;
    fn dreiwege_weichen() -> Vec<DreiwegeWeicheUnit<Self>>;
    fn kurven_weichen() -> Vec<KurvenWeicheUnit<Self>>;
    fn s_kurven_weichen() -> Vec<SKurvenWeicheUnit<Self>>;
    fn kreuzungen() -> Vec<KreuzungUnit<Self>>;
}
