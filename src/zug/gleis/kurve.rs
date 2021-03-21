//! Definition und zeichnen einer Kurve

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
use std::marker::PhantomData;

use super::types::*;

/// Definition einer Kurve
pub struct Kurve<T> {
    pub zugtyp: PhantomData<T>,
    pub radius: Radius,
    pub angle: AngleDegrees,
}
