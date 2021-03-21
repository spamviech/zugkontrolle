//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
use std::marker::PhantomData;

use crate::zug::gleisplan::types::*;

/// Definition einer Weiche
pub struct Weiche<T> {
    pub zugtyp: PhantomData<T>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtung,
}
pub enum WeichenRichtung {
    Gerade(WeichenRichtungGerade),
    Gebogen(WeichenRichtungGebogen),
    SKurve(WeichenRichtungSKurve),
}
pub enum WeichenRichtungGerade {
    Links,
    Rechts,
    Dreiwege,
}
pub enum WeichenRichtungGebogen {
    Links,
    Rechts,
}
pub enum WeichenRichtungSKurve {
    Links(AngleDegrees),
    Rechts(AngleDegrees),
}
