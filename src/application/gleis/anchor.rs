//! anchor points to mark connection points of a rail

use crate::lookup;

pub mod point;
pub(crate) mod rstar;

pub use point::*;

pub trait Lookup<Name>: lookup::Lookup<Name, Anchor> {}
impl<Name, T: lookup::Lookup<Name, Anchor>> Lookup<Name> for T {}
pub use lookup::impl_lookup;
