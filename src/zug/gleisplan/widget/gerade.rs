use std::marker::PhantomData;

use crate::zug::gleisplan::types::*;

/// Definition einer Gerade
pub struct Gerade<T> {
    pub zugtyp: PhantomData<T>,
    pub length: Length,
}
