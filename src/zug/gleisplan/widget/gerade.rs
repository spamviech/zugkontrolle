//! Definition und zeichnen einer Gerade

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
use std::marker::PhantomData;

use crate::zug::gleisplan::types::*;

/// Definition einer Gerade
pub struct Gerade<T> {
    pub zugtyp: PhantomData<T>,
    pub length: Length,
}
