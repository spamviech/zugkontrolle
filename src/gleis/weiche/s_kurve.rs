//! Definition und zeichnen einer Weiche

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::gleis::types::*;

/// Definition einer Weiche mit S-Kurve
#[derive(Debug, Clone)]
pub struct SKurveWeiche<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
    pub radius: Radius,
    pub angle: AngleDegrees,
    pub direction: WeichenRichtungSKurve,
}
#[derive(Debug, Clone, Copy)]
pub enum WeichenRichtungSKurve {
    Links(AngleDegrees),
    Rechts(AngleDegrees),
}
