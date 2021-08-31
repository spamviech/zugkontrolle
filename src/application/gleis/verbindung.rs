//! Verbindung zwischen zwei Gleisen

use crate::lookup;

pub mod point;
pub(crate) mod rstern;

pub use point::*;

/// Spezialisierung des Lookup-Traits auf Verbindung als Element.
pub trait Lookup<Name>: lookup::Lookup<Name, Anchor> {}
impl<Name, T: lookup::Lookup<Name, Anchor>> Lookup<Name> for T {}
