//! Ids zur Identifikation der Gleise.

// Erlaubt id::Repräsentation
#![allow(clippy::pub_use)]

use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::eindeutig::{Id, KeineIdVerfügbar};

pub mod eindeutig;

#[cfg(test)]
mod test;

pub use eindeutig::Repräsentation;

// soll direkt importiert werden
#[allow(clippy::module_name_repetitions)]
/// Id für ein Gleis.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
pub struct GleisId<T: 'static>(Arc<Id<T>>);

impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for GleisId<T> {}

impl<T> PartialOrd for GleisId<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for GleisId<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for GleisId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> GleisId<T> {
    /// Erzeuge eine neue [`GleisId`] für den entsprechenden Typ.
    ///
    /// ## Errors
    ///
    /// Wenn für `T` keine neue [`GleisId`] erzeugt werden kann.
    pub fn neu() -> Result<GleisId<T>, KeineIdVerfügbar> {
        Id::neu().map(|id| GleisId(Arc::new(id)))
    }

    /// Erhalte eine eindeutige Zahl für die [`GleisId`].
    ///
    /// Die selbe [`GleisId`], sowie alle ihre Kopien, werde bei jedem Aufruf die selbe Zahl zurückgeben.
    /// Zwei gleichzeitig existierende [`GleisIds`](GleisId) werden unterschiedliche Zahlen zurückgeben.
    ///
    /// Sobald die letzte Kopie einer [`GleisId`] gedroppt wird kann es sein,
    /// dass eine andere [`GleisId`] die selbe Zahl zurückgibt.
    #[must_use]
    pub fn repräsentation(&self) -> Repräsentation {
        self.0.repräsentation()
    }
}
