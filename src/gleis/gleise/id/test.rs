//! Tests für [GleisId].

use std::collections::{BTreeSet, HashSet};

use crate::{
    gleis::gleise::id::GleisId2,
    test_util::{expect_eq, expect_gt, expect_ne, init_test_logging, ExpectNe, Expectation},
};

#[test]
fn eindeutig() -> Result<(), Expectation> {
    init_test_logging();

    let ids: Vec<_> = (0..32)
        .map(|i| (i, GleisId2::<()>::neu().expect("test verwendet weniger als usize::MAX Ids.")))
        .filter_map(|(i, id)| (i % 7 != 0).then_some(id))
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
fn u32_eindeutig() -> Result<(), Expectation> {
    init_test_logging();

    let ids: Vec<_> = (0..32)
        .map(|_i| GleisId2::<()>::neu().expect("test verwendet weniger als usize::MAX Ids."))
        .collect();
    let u32s: Vec<_> = ids.iter().map(GleisId2::u32).collect();
    let num = u32s.len();
    let set: BTreeSet<_> = u32s.into_iter().collect();
    let num_eindeutig = set.len();

    // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids.
    expect_eq(num, num_eindeutig)?;
    Ok(())
}

#[test]
fn u32_kopie_identisch() -> Result<(), Expectation> {
    init_test_logging();

    let id = GleisId2::<()>::neu().expect("Erste Id.");
    let mut u32s = vec![id.u32()];
    // eine Kopie liefert das selbe Ergebnis
    let clone = id.clone();
    u32s.push(clone.u32());
    // ein erneuter Aufruf liefert das selbe Ergebnis
    u32s.push(id.u32());
    u32s.push(clone.u32());

    let num = u32s.len();
    let set: BTreeSet<_> = u32s.into_iter().collect();
    let num_eindeutig = set.len();

    // Alle Ids sind identisch, dementsprechend hat die Menge nur ein Element.
    expect_gt(num, 1)?;
    expect_eq(num_eindeutig, 1)?;
    Ok(())
}

#[test]
fn clone() -> Result<(), Expectation> {
    init_test_logging();

    let id = GleisId2::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!");
    let id_clone = id.clone();
    // durch drop des Original-Werts wird die Id nicht wieder freigegeben.
    drop(id);

    let ids = (0..32)
        .map(|_i| GleisId2::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!"));

    // alle erzeugten Ids haben einen anderen Wert.
    ids.map(|id| expect_ne(id_clone.clone(), id)).collect::<Result<_, ExpectNe>>()?;
    Ok(())
}
