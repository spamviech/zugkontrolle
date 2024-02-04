//! Ein analog zu [`enumerate`](Iterator::enumerate) funktionierender Iterator, der nach einem Overflow nicht abbricht.

use num_traits::{bounds::LowerBounded, CheckedAdd, One};

/// Ein analog zu [`enumerate`](Iterator::enumerate) funktionierender Iterator, der nach einem Overflow nicht abbricht.
///
/// Beginnend mit [`LowerBounded::min_value`] wird jedes Element des Iterators mit einem aufsteigenden Zähler annotiert.
/// Nach einem Overflow wird jedes Item mit [`None`] annotiert.
#[derive(Debug)]
pub struct EnumerateChecked<C, I> {
    /// Der Nächste Zähler-Wert.
    next: Option<C>,
    /// Der verzierte Iterator.
    iterator: I,
}

// Repetition bewusst gewählt.
#[allow(clippy::module_name_repetitions)]
/// Erweiterungs-trait für alle Iteratoren, damit die
/// [`enumerate_checked`](EnumerateCheckedExt::enumerate_checked)-Methode verfügbar ist.
pub trait EnumerateCheckedExt<C, I> {
    /// Beginnend mit [`LowerBounded::min_value`] wird jedes Element des Iterators mit einem aufsteigenden Zähler annotiert.
    /// Nach einem Overflow wird jedes Item mit [`None`] annotiert.
    fn enumerate_checked(self) -> EnumerateChecked<C, I>;
}

impl<C: LowerBounded, I: Iterator> EnumerateCheckedExt<C, I> for I {
    fn enumerate_checked(self) -> EnumerateChecked<C, I> {
        EnumerateChecked { next: Some(<C as LowerBounded>::min_value()), iterator: self }
    }
}

impl<C, I> Iterator for EnumerateChecked<C, I>
where
    C: Clone + CheckedAdd + One,
    I: Iterator,
{
    type Item = (Option<C>, <I as Iterator>::Item);

    fn next(&mut self) -> Option<Self::Item> {
        let count = self.next.clone();
        self.next = self.next.clone().and_then(|next| next.checked_add(&<C as One>::one()));
        self.iterator.next().map(|item| (count, item))
    }
}
