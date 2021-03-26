//! Trait für failure-free lookup von /anchor::Point/

use super::Point;

pub use zugkontrolle_derive::Lookup;

pub trait Lookup<AnchorName> {
    /// failure-free lookup for a specific /anchor::Point/.
    fn get(&self, key: AnchorName) -> &Point;
    /// failure-free mutable lookup for a specific /anchor::Point/.
    fn get_mut(&mut self, key: AnchorName) -> &mut Point;
    /// Perform action for all /anchor::Point/s.
    fn foreach<F: FnMut(&Point)>(&self, action: F);
}
