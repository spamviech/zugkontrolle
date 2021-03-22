//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use super::types::*;

/// Definition einer Weiche
#[derive(Debug, Clone)]
pub struct Weiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtung,
}
#[derive(Debug, Clone)]
pub enum WeichenRichtung {
    Gerade(WeichenRichtungGerade),
    Gebogen(WeichenRichtungGebogen),
    SKurve(WeichenRichtungSKurve),
}
#[derive(Debug, Clone, Copy)]
pub enum WeichenRichtungGerade {
    Links,
    Rechts,
    Dreiwege,
}
#[derive(Debug, Clone, Copy)]
pub enum WeichenRichtungGebogen {
    Links,
    Rechts,
}
#[derive(Debug, Clone, Copy)]
pub enum WeichenRichtungSKurve {
    Links(AngleDegrees),
    Rechts(AngleDegrees),
}
