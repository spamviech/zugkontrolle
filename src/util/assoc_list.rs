//! Eine assoziierte Liste auf Basis eines [Vec].

use std::{
    borrow::Borrow,
    mem,
    slice::{Iter, IterMut},
    vec::{Drain, IntoIter},
};

/// Eine assoziierte Liste auf Basis eines [Vec].
///
/// Die Methoden basieren rein auf der [`PartialEq`]-Implementierung und sind meistens `O(n)`.
///
/// In der Regel ist eine [`HashMap`](std::collections::HashMap) oder [`BTreeMap`](std::collections::BTreeMap)
/// zu bevorzugen. Die [`AssocList`] existiert als fallback,
/// falls der Schlüssel weder [`Hash`](std::hash::Hash) noch [`Ord`] implementiert.
///
/// Anmerkung: Alle Methoden benötigen nur [`PartialEq`] für den Schlüssel, es gibt aber gute Argumente nur
/// [`Ord`]-Schlüssel zu verwenden. So können z.B. Element mit [`f32::NAN`]-Schlüssel weder gefunden noch entfernt werden
/// ([`PartialEq::eq`] ist immer [`false`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssocList<K, V>(Vec<(K, V)>);

impl<K, V> AssocList<K, V> {
    /// Erzeuge eine neue [`AssocList`].
    #[must_use]
    pub const fn neu() -> Self {
        AssocList(Vec::new())
    }

    /// Erzeuge eine neue [`AssocList`] mit mindestend der spezifizierten `capacity`.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        AssocList(Vec::with_capacity(capacity))
    }

    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys(self.0.iter())
    }

    pub fn into_keys(self) -> IntoKeys<K, V> {
        IntoKeys(self.0.into_iter())
    }

    pub fn values(&self) -> Values<'_, K, V> {
        Values(self.0.iter())
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        ValuesMut(self.0.iter_mut())
    }

    pub fn into_values(self) -> IntoValues<K, V> {
        IntoValues(self.0.into_iter())
    }

    pub fn iter(&self) -> Iter<'_, (K, V)> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, (K, V)> {
        self.0.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn drain(&mut self) -> Drain<'_, (K, V)> {
        self.0.drain(..)
    }

    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Erhalte den zum `schlüssel` gehörigen [`Entry`].
    pub fn entry(&mut self, schlüssel: K) -> Entry<'_, K, V>
    where
        K: PartialEq,
    {
        for (index, (enthaltener_schlüssel, enthaltener_wert)) in self.0.iter_mut().enumerate() {
            if enthaltener_schlüssel == &schlüssel {
                return Entry::Occupied(OccupiedEntry { vec: &mut self.0, index });
            }
        }
        Entry::Vacant(VacantEntry { vec: &mut self.0, schlüssel })
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }

    pub fn get_key_value<Q>(&self, k: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }

    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }

    /// Füge ein neues Element für den `schlüssel` hinzu.
    /// Falls bereits ein Element in der [`AssocList`] für den `schlüssel` existiert wird dieses zurückgegeben.
    pub fn insert(&mut self, schlüssel: K, wert: V) -> Option<V>
    where
        K: PartialEq,
    {
        for (enthaltener_schlüssel, enthaltener_wert) in &mut self.0 {
            if enthaltener_schlüssel == &schlüssel {
                let bisher = mem::replace(enthaltener_wert, wert);
                return Some(bisher);
            }
        }
        self.0.push((schlüssel, wert));
        None
    }

    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }

    pub fn remove_entry<Q>(&mut self, k: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q>,
        Q: PartialEq + ?Sized,
    {
        todo!()
    }
}

impl<K, V> IntoIterator for AssocList<K, V> {
    type Item = (K, V);

    type IntoIter = IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a AssocList<K, V> {
    type Item = &'a (K, V);

    type IntoIter = Iter<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut AssocList<K, V> {
    type Item = &'a mut (K, V);

    type IntoIter = IterMut<'a, (K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl<K: PartialEq, V> FromIterator<(K, V)> for AssocList<K, V> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut assoc_list = AssocList::neu();
        for (key, value) in iter {
            // Bei wiederholten Schlüssel werden frühere Werte überschrieben.
            let _ = assoc_list.insert(key, value);
        }
        assoc_list
    }
}

pub struct Keys<'a, K, V>(Iter<'a, (K, V)>);

pub struct IntoKeys<K, V>(IntoIter<(K, V)>);

pub struct Values<'a, K, V>(Iter<'a, (K, V)>);

pub struct ValuesMut<'a, K, V>(IterMut<'a, (K, V)>);

pub struct IntoValues<K, V>(IntoIter<(K, V)>);

/// Ein Ausschnitt einer [`AssocList`] für ein einzelnes Element. Es kann entweder vorhanden, oder aktuell fehlend sein.
pub enum Entry<'a, K, V> {
    Occupied(OccupiedEntry<'a, K, V>),
    Vacant(VacantEntry<'a, K, V>),
}

impl<'a, K, V> Entry<'a, K, V> {
    pub fn key(&self) -> &K {
        match self {
            Entry::Occupied(occupied) => occupied.key(),
            Entry::Vacant(vacant) => vacant.key(),
        }
    }

    pub fn or_insert(self, wert: V) -> &'a mut V {
        match self {
            Entry::Occupied(occupied) => occupied.get_mut(),
            Entry::Vacant(vacant) => vacant.insert(wert),
        }
    }
}

pub struct OccupiedEntry<'a, K, V> {
    vec: &'a mut Vec<(K, V)>,
    index: usize,
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    pub fn key(&self) -> &K {
        let (schlüssel, _wert) = self.vec.get(self.index).expect("Index out of bounds!");
        schlüssel
    }

    pub fn get(self) -> &'a V {
        let (_schlüssel, wert) = self.vec.get(self.index).expect("Index out of bounds!");
        wert
    }

    pub fn get_mut(self) -> &'a mut V {
        let (_schlüssel, wert) = self.vec.get_mut(self.index).expect("Index out of bounds!");
        wert
    }

    pub fn remove_entry(self) -> (K, V) {
        self.vec.swap_remove(self.index)
    }

    pub fn remove(self) -> V {
        let (_schlüssel, wert) = self.vec.swap_remove(self.index);
        wert
    }

    pub fn insert(&mut self, neuer_wert: V) -> V {
        let (_schlüssel, wert) = self.vec.get_mut(self.index).expect("Index out of bounds!");
        mem::replace(wert, neuer_wert)
    }
}

pub struct VacantEntry<'a, K, V> {
    vec: &'a mut Vec<(K, V)>,
    schlüssel: K,
}

impl<'a, K, V> VacantEntry<'a, K, V> {
    pub fn key(&self) -> &K {
        &self.schlüssel
    }

    /// Füge eine Element für den Schlüssel hinzu.
    ///
    /// ## Panics
    ///
    /// Bei einem Programmier-Fehler, wenn [`Vec::last_mut`] [`None`] zurückgibt.
    pub fn insert(self, wert: V) -> &'a mut V {
        self.vec.push((self.schlüssel, wert));
        let (_schlüssel, wert) = self.vec.last_mut().expect("Element wurde gerade hinzugefügt!");
        wert
    }
}
