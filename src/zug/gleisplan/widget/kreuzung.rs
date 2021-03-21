//! Definition und zeichnen einer Kreuzung

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use crate::zug::gleisplan::types::*;

/// Definition einer Kreuzung
pub struct Kreuzung<T> {
    pub zugtyp: PhantomData<T>,
    pub length: Length,
    pub radius: Radius,
    // TODO: winkel kann aus radius und länge berechnet werden?
    pub winkel: AngleDegrees,
}
pub enum KreuzungsArt {
    MitKurve,
    OhneKurve,
}
