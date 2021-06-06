//! Vektoren mit mindestens einem Element.

use std::iter::FromIterator;
use std::ops::{Index, IndexMut};
use std::{slice, vec};

use serde::{Deserialize, Serialize};

/// Ein Vektor mit mindestens einem Element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NonEmpty<T> {
    pub head: T,
    pub tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn head(&self) -> &T {
        &self.head
    }

    pub fn head_mut(&mut self) -> &mut T {
        &mut self.head
    }

    pub fn split_head(&self) -> (&T, &Vec<T>) {
        let NonEmpty { head, tail } = self;
        (head, tail)
    }

    pub fn split_head_mut(&mut self) -> (&mut T, &mut Vec<T>) {
        let NonEmpty { head, tail } = self;
        (head, tail)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter { head: &self.head, is_head: true, tail: self.tail.iter() }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut { head: &mut self.head, is_head: true, tail: self.tail.iter_mut() }
    }

    pub fn len(&self) -> usize {
        1 + self.tail.len()
    }
}

impl<T> Index<usize> for NonEmpty<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        if index == 0 {
            &self.head
        } else {
            self.tail.index(index + 1)
        }
    }
}
impl<T> IndexMut<usize> for NonEmpty<T> {
    fn index_mut(&mut self, index: usize) -> &mut <Self as Index<usize>>::Output {
        if index == 0 {
            &mut self.head
        } else {
            self.tail.index_mut(index + 1)
        }
    }
}

pub struct Iter<'t, T> {
    head: &'t T,
    is_head: bool,
    tail: slice::Iter<'t, T>,
}

impl<'t, T> Iterator for Iter<'t, T> {
    type Item = &'t T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_head {
            self.is_head = false;
            Some(self.head)
        } else {
            self.tail.next()
        }
    }
}

pub struct IterMut<'t, T> {
    head: &'t mut T,
    is_head: bool,
    tail: slice::IterMut<'t, T>,
}

/*
// lifetime problems due to mutable reference
impl<'t, T> MutIterator for IterMut<'t, T> {
    type Item = &'t mut T;

    fn next<'n>(&'n mut self) -> Option<Self::Item> {
        if self.is_head {
            self.is_head = false;
            Some(self.head)
        } else {
            self.tail.next()
        }
    }
}
*/

pub struct IntoIter<T> {
    head: T,
    is_head: bool,
    tail: vec::IntoIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_head {
            self.is_head = false;
            Some(self.head)
        } else {
            self.tail.next()
        }
    }
}
impl<T> IntoIterator for NonEmpty<T> {
    type IntoIter = IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter()
    }
}

// Newtype über Option, wegen alternativer FromIterator-Implementierung.
pub struct MaybeEmpty<T>(Option<NonEmpty<T>>);
impl<T> MaybeEmpty<T> {
    pub fn unwrap(self) -> NonEmpty<T> {
        self.0.unwrap()
    }
}
impl<T> FromIterator<T> for MaybeEmpty<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut iterator = iter.into_iter();
        match iterator.next() {
            Some(head) => MaybeEmpty(Some(NonEmpty { head, tail: iterator.collect() })),
            None => MaybeEmpty(None),
        }
    }
}
