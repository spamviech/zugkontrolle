//! Eine String-newtype mit auf [`UniCase`]-basierter [`Ord`]-Instanz.
//!
//! Die [Eq]-Instanz vergleicht Strings unter Berücksichtigung der Groß-/Kleinschreibung (wie [str]).
//! Die [Ord]-Instanz vergleicht (wie [`UniCase`]) ohne Berücksichtigung der Groß-/Kleinschreibung,
//! nur bei Gleichheit wird sie ebenfalls berücksichtigt ([Ord]-Instanz von [str]).

use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
};

use unicase::UniCase;

/// Eine String-newtype mit auf [`UniCase`]-basierter [Ord]-Instanz.
///
/// Die [Eq]-Instanz vergleicht Strings unter Berücksichtigung der Groß-/Kleinschreibung (wie [str]).
/// Die [Ord]-Instanz vergleicht (wie [`UniCase`]) ohne Berücksichtigung der Groß-/Kleinschreibung,
/// nur bei Gleichheit wird sie ebenfalls berücksichtigt ([Ord]-Instanz von [str]).
#[derive(Debug, Clone, Copy)]
pub struct UniCaseOrd<S>(UniCase<S>);

impl<S: AsRef<str>> UniCaseOrd<S> {
    /// Erstelle einen neuen [`UniCaseOrd`].
    ///
    /// Anmerkung: Dabei wird überprüft, ob der Text nur aus ASCII-Zeichen besteht.

    pub fn neu(string: S) -> Self {
        UniCaseOrd(UniCase::new(string))
    }
}

impl<S> UniCaseOrd<S> {
    /// Erhalte den in [`UniCaseOrd`] enthaltenen Wert.

    pub fn into_inner(self) -> S {
        self.0.into_inner()
    }
}

impl<S: Display> Display for UniCaseOrd<S> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(formatter)
    }
}

impl<S: AsRef<str>> AsRef<str> for UniCaseOrd<S> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl<S: AsRef<str>, T: AsRef<str>> PartialEq<UniCaseOrd<T>> for UniCaseOrd<S> {
    fn eq(&self, other: &UniCaseOrd<T>) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}

impl<S: AsRef<str>> Eq for UniCaseOrd<S> {}

impl<S: AsRef<str>> PartialOrd for UniCaseOrd<S> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<S: AsRef<str>> Ord for UniCaseOrd<S> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.0.cmp(&other.0) {
            Ordering::Equal => self.0.as_ref().cmp(other.0.as_ref()),
            ordering @ (Ordering::Less | Ordering::Greater) => ordering,
        }
    }
}
