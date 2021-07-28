//! Vektoren mit mindestens einem Element.

use std::iter::FromIterator;
use std::{slice, vec};

use serde::{Deserialize, Serialize};

/// Ein Vektor mit mindestens einem Element.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct NonEmpty<T> {
    pub head: T,
    pub tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn from_vec(mut vec: Vec<T>) -> Option<Self> {
        if vec.len() > 0 {
            let head = vec.remove(0);
            Some(NonEmpty { head, tail: vec })
        } else {
            None
        }
    }

    pub fn singleton(head: T) -> Self {
        NonEmpty { head, tail: Vec::new() }
    }

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

    /// Add a new element to the end of the NonEmpty.
    pub fn push(&mut self, t: T) {
        self.tail.push(t)
    }

    /// Return the last element and removes it from the NonEmpty,
    /// returning None if only one element remains.
    pub fn pop(&mut self) -> Option<T> {
        self.tail.pop()
    }

    /// Remove the element at the specified index and return it.
    /// None is returned for out-of-bound indices and the last element.
    pub fn remove(&mut self, index: usize) -> Option<T> {
        let len = self.len();
        if index > len || (len == 1 && index == 0) {
            return None;
        }
        if index == 0 {
            let mut new_head = self.tail.remove(0);
            std::mem::swap(&mut self.head, &mut new_head);
            Some(new_head)
        } else {
            Some(self.tail.remove(index - 1))
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        if index == 0 {
            Some(&self.head)
        } else {
            self.tail.get(index - 1)
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        if index == 0 {
            Some(&mut self.head)
        } else {
            self.tail.get_mut(index - 1)
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

// lifetime problems due to mutable reference
impl<'t, T> Iterator for IterMut<'t, T> {
    type Item = &'t mut T;

    fn next<'n>(&'n mut self) -> Option<Self::Item> {
        if self.is_head {
            self.is_head = false;
            // http://smallcultfollowing.com/babysteps/blog/2013/10/24/iterators-yielding-mutable-references/
            // https://doc.rust-lang.org/nomicon/transmutes.html
            // as long as IterMut exists (i.e. lifetime 't) no one else can access the NonEmpty
            // therefore, unsafe to extend the returned lifetime is safe here
            let head: &'t mut T = unsafe { std::mem::transmute(&mut self.head) };
            Some(head)
        } else {
            self.tail.next()
        }
    }
}

pub struct IntoIter<T> {
    head: Option<T>,
    tail: vec::IntoIter<T>,
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.head.is_some() {
            let head = self.head.take().unwrap();
            Some(head)
        } else {
            self.tail.next()
        }
    }
}
impl<T> IntoIterator for NonEmpty<T> {
    type IntoIter = IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { head: Some(self.head), tail: self.tail.into_iter() }
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
        MaybeEmpty(iterator.next().map(|head| NonEmpty { head, tail: iterator.collect() }))
    }
}
