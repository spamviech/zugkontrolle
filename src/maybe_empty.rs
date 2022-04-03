//! [FromIterator]-Implementierung für [NonEmpty].

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::iter::FromIterator;

use nonempty::NonEmpty;

/// Newtype über Option, wegen alternativer FromIterator-Implementierung.
#[derive(Debug)]
pub struct MaybeEmpty<T>(Option<NonEmpty<T>>);

impl<T> MaybeEmpty<T> {
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
