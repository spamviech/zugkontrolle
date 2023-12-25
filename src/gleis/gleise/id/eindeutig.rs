//! Erzeuge eindeutige [Ids](Id).

use std::{
    any::{type_name, TypeId},
    cmp::Ordering,
    collections::{
        btree_map::{BTreeMap, Entry},
        BTreeSet,
    },
    hash::{Hash, Hasher},
    marker::PhantomData,
};

use log::{error, trace};
use parking_lot::{const_mutex, MappedMutexGuard, Mutex, MutexGuard};

/// Zahlen-typ, der über [Id::Repräsentation] erhalten werden kann.
///
/// Implementierungs-Detail: aktuell verwenden Eq, Ord, Hash-Instanzen von [Id] diese Repräsentation.
pub type Repräsentation = u32;

static VERWENDETE_IDS: Mutex<BTreeMap<TypeId, BTreeSet<Repräsentation>>> =
    const_mutex(BTreeMap::new());

fn type_set<'t, T: 'static>() -> MappedMutexGuard<'t, BTreeSet<Repräsentation>> {
    MutexGuard::map(VERWENDETE_IDS.lock(), |id_map| {
        let type_id = TypeId::of::<T>();
        match id_map.entry(type_id) {
            Entry::Vacant(v) => v.insert(BTreeSet::new()),
            Entry::Occupied(o) => o.into_mut(),
        }
    })
}

/// Eine eindeutige [Id] für den Typ T.
#[derive(zugkontrolle_macros::Debug)]
pub struct Id<T: 'static> {
    id: Repräsentation,
    phantom: PhantomData<fn() -> T>,
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.phantom.hash(state);
        TypeId::of::<T>().hash(state);
    }
}

impl<T> Drop for Id<T> {
    fn drop(&mut self) {
        let mut set = type_set::<T>();
        if !set.remove(&self.id) {
            error!(
                "Gedroppte Id '{}' für Typ '{}' war nicht als verwendet markiert!",
                self.id,
                type_name::<T>()
            );
        } else {
            trace!("Drop Id '{}' für Typ '{}'.", self.id, type_name::<T>());
        }
    }
}

/// Alle [Ids](Id) wurden bereits verwendet. Es ist aktuell keine eindeutige [Id] verfügbar.
#[derive(Debug, Clone, Copy)]
pub struct KeineIdVerfügbar;

impl<T> Id<T> {
    /// Erhalte eine bisher unbenutzte [Id].
    pub fn neu() -> Result<Id<T>, KeineIdVerfügbar> {
        let mut set = type_set::<T>();
        let initial =
            if let Some(last) = set.last() { last.wrapping_add(1) } else { Repräsentation::MIN };
        let mut id = initial;
        while !set.insert(id) {
            id = id.wrapping_add(1);
            if id == initial {
                return Err(KeineIdVerfügbar);
            }
        }
        trace!("Erzeuge Id '{}' für Typ '{}'.", id, type_name::<T>());
        Ok(Id { id, phantom: PhantomData })
    }

    /// Erhalte eine eindeutige Zahl für die [Id].
    ///
    /// Die selbe [Id] wird bei jedem Aufruf die selbe Zahl zurückgeben.
    /// Zwei gleichzeitig existierende [Ids](Id) werden unterschiedliche Zahlen zurückgeben.
    ///
    /// Sobald eine [Id] gedroppt wird kann es sein, dass eine andere [Id] die selbe Zahl zurückgibt.
    pub fn repräsentation(&self) -> Repräsentation {
        self.id
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashSet;

    use crate::test_util::{expect_eq, expect_true, init_test_logging, Expectation};

    #[test]
    fn eindeutig() -> Result<(), Expectation> {
        init_test_logging();

        let ids: Vec<_> = (0..32)
            .map(|i| (i, Id::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids.")))
            .filter_map(|(i, id)| (i % 2 == 0).then_some(id))
            .collect();
        let num = ids.len();
        let btree_set: BTreeSet<_> = ids.iter().collect();
        let btree_num = btree_set.len();
        let hash_set: HashSet<_> = ids.into_iter().collect();
        let hash_num = hash_set.len();

        // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids,
        // unabhängig der verwendeten set-Variante.
        expect_eq(num, btree_num)?;
        expect_eq(num, hash_num)?;
        Ok(())
    }

    #[test]
    fn repräsentation_eindeutig() -> Result<(), Expectation> {
        init_test_logging();

        let ids: Vec<_> = (0..32)
            .map(|_i| Id::<()>::neu().expect("Test verwendet weniger als Repräsentation::MAX Ids."))
            .collect();
        let repräsentationen: Vec<_> = ids.iter().map(Id::repräsentation).collect();
        let num = repräsentationen.len();
        let set: BTreeSet<_> = repräsentationen.into_iter().collect();
        let num_eindeutig = set.len();

        // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids.
        expect_eq(num, num_eindeutig)?;
        Ok(())
    }

    #[test]
    fn freigeben() -> Result<(), Expectation> {
        init_test_logging();

        // verwende eigenen Typ um nicht mit parallel laufenden Tests zu konkurrieren.
        struct Dummy;

        let ids: Vec<_> = (0..32)
            .map(|_i| Id::<Dummy>::neu().expect("Test verwendet weniger als usize::MAX Ids!"))
            .collect();
        drop(ids);

        // nach drop der Ids können wieder neue mit ihrem Wert erzeugt werden
        expect_true(
            VERWENDETE_IDS
                .lock()
                .get(&TypeId::of::<Dummy>())
                .expect("Ids für unit type () wurden vorher erzeugt!")
                .is_empty(),
        )?;
        Ok(())
    }

    #[test]
    fn unabhängig() -> Result<(), Expectation> {
        init_test_logging();

        // verwende eigenen Typ um nicht mit parallel laufenden Tests zu konkurrieren.
        struct Param<T>(T);

        let a = Id::<Param<()>>::neu().expect("Test verwendet weniger als usize::MAX Ids!");
        let b = Id::<Param<bool>>::neu().expect("Test verwendet weniger als usize::MAX Ids!");

        // die erste Id ist identisch (Ids sind unabhängig)
        expect_eq(a.id, b.id)?;
        Ok(())
    }
}
