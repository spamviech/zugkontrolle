//! Erzeuge eindeutige [Ids](Id).

use std::{
    any::TypeId,
    collections::{
        btree_map::{BTreeMap, Entry},
        BTreeSet,
    },
    marker::PhantomData,
    sync::Arc,
};

use log::error;
use parking_lot::{const_mutex, MappedMutexGuard, Mutex, MutexGuard};

static VERWENDETE_IDS: Mutex<BTreeMap<TypeId, BTreeSet<usize>>> = const_mutex(BTreeMap::new());

fn type_set<'t, T: 'static>() -> MappedMutexGuard<'t, BTreeSet<usize>> {
    MutexGuard::map(VERWENDETE_IDS.lock(), |id_map| {
        let type_id = TypeId::of::<T>();
        match id_map.entry(type_id) {
            Entry::Vacant(v) => v.insert(BTreeSet::new()),
            Entry::Occupied(o) => o.into_mut(),
        }
    })
}

/// Eine eindeutige Id für den Typ T.
///
/// Im Gegensatz zu [Id] ist dieser Typ nicht klonbar, was eine einfachere [Drop]-Implementierung ermöglicht.
/// Verwende stattdessen die [Arc]-Funktionalität für mehrere Kopien der selben ID.
#[derive(zugkontrolle_macros::Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IdIntern<T: 'static> {
    id: usize,
    phantom: PhantomData<fn() -> T>,
}

impl<T> Drop for IdIntern<T> {
    fn drop(&mut self) {
        let mut set = type_set::<T>();
        if !set.remove(&self.id) {
            error!("Gedroppte Id war nicht als verwendet markiert: {self:?}");
        }
    }
}

/// Eine eindeutige Id für den Typ T.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id<T: 'static>(Arc<IdIntern<T>>);

/// Alle [Ids](Id) wurden bereits verwendet. Es ist aktuell keine eindeutige [Id] verfügbar.
#[derive(Debug, Clone, Copy)]
pub struct KeineIdVerfügbar;

impl<T> Id<T> {
    /// Erhalte eine bisher unbenutzte [Id].
    pub fn neu() -> Result<Id<T>, KeineIdVerfügbar> {
        let mut set = type_set::<T>();
        let initial = if let Some(last) = set.last() { last.wrapping_add(1) } else { usize::MIN };
        let mut id = initial;
        while !set.insert(id) {
            id = id.wrapping_add(1);
            if id == initial {
                return Err(KeineIdVerfügbar);
            }
        }
        Ok(Id(Arc::new(IdIntern { id, phantom: PhantomData })))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn expect_eq<T: PartialEq>(a: T, b: T) -> Result<(), ()> {
        if a == b {
            Ok(())
        } else {
            Err(())
        }
    }

    fn expect_ne<T: PartialEq>(a: T, b: T) -> Result<(), ()> {
        if a != b {
            Ok(())
        } else {
            Err(())
        }
    }

    #[test]
    fn eindeutig() -> Result<(), ()> {
        let ids: Vec<_> = (0..32)
            .map(|i| (i, Id::<()>::neu().expect("test verwendet weniger als usize::MAX Ids.")))
            .filter_map(|(i, id)| (i % 2 == 0).then_some(id))
            .collect();
        let num = ids.len();
        let set: BTreeSet<_> = ids.into_iter().collect();
        let num_eindeutig = set.len();

        // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids.
        expect_eq(num, num_eindeutig)
    }

    #[test]
    fn freigeben() -> Result<(), ()> {
        let ids: Vec<_> = (0..32)
            .map(|i| (i, Id::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!")))
            .collect();
        drop(ids);

        // nach drop der Ids können wieder neue mit ihrem Wert erzeugt werden
        expect_eq(
            VERWENDETE_IDS
                .lock()
                .get(&TypeId::of::<()>())
                .expect("Ids für unit type () wurden vorher erzeugt!")
                .len(),
            0,
        )
    }

    #[test]
    fn clone() -> Result<(), ()> {
        let id = Id::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!");
        let id_clone = id.clone();
        // durch drop des Original-Werts wird die Id nicht wieder freigegeben.
        drop(id);

        let ids =
            (0..32).map(|_i| Id::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!"));

        // alle erzeugten Ids haben einen anderen Wert.
        ids.map(|id| expect_ne(id_clone.clone(), id)).collect()
    }
}
