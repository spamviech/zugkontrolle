//! [FromIterator]-Implementierung für [NonEmpty].

use std::iter::FromIterator;

use nonempty::NonEmpty;

/// Newtype über Option, wegen alternativer FromIterator-Implementierung.
#[derive(Debug)]
pub struct MaybeEmpty<T>(pub Option<NonEmpty<T>>);

impl<T> MaybeEmpty<T> {
    /// Rufe [unwrap](Option::unwrap) auf der enthaltenen [Option] auf.
    ///
    /// Führt zu einem [panic], wenn der enthaltene Wert [None] ist.
    #[inline(always)]
    pub fn unwrap(self) -> NonEmpty<T> {
        self.0.unwrap()
    }
}

impl<T> FromIterator<T> for MaybeEmpty<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut iterator = iter.into_iter();
        MaybeEmpty(iterator.next().map(|head| NonEmpty { head, tail: iterator.collect() }))
    }
}
