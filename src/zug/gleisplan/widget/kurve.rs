use std::marker::PhantomData;

use crate::zug::gleisplan::types::*;

/// Definition einer Kurve
pub struct Kurve<T> {
    pub zugtyp: PhantomData<T>,
    pub radius: Radius,
    pub angle: Angle,
}
