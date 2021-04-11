//! Trait für failure-free lookup von /anchor::Anchor/

use super::Anchor;

pub use zugkontrolle_derive::Lookup;

pub trait Lookup<AnchorName> {
    /// failure-free lookup for a specific /Anchor/.
    fn get(&self, key: AnchorName) -> &Anchor;
    /// failure-free mutable lookup for a specific /Anchor/.
    fn get_mut(&mut self, key: AnchorName) -> &mut Anchor;
    /// Perform action for all /Anchor/s.
    fn foreach<F: FnMut(&Anchor)>(&self, action: F);
    /// Adjust all /Anchor/s.
    fn map<F: Fn(&Anchor) -> Anchor>(&self, action: F) -> Self;
}
