//! Tests für [`GleisId`].

use std::collections::{BTreeSet, HashSet};

use crate::{
    gleis::gleise::id::GleisId,
    test_util::{expect_eq, expect_gt, expect_ne, init_test_logging, ExpectNe, Expectation},
};

#[test]
fn eindeutig() -> Result<(), Expectation> {
    init_test_logging();

    let ids: Vec<_> = (0..32)
        .map(|i| (i, GleisId::<()>::neu().expect("test verwendet weniger als usize::MAX Ids.")))
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
fn repräsentation_eindeutig() -> Result<(), Expectation> {
    init_test_logging();

    let ids: Vec<_> = (0..32)
        .map(|_i| GleisId::<()>::neu().expect("test verwendet weniger als usize::MAX Ids."))
        .collect();
    let repräsentationen: Vec<_> = ids.iter().map(GleisId::repräsentation).collect();
    let num = repräsentationen.len();
    let set: BTreeSet<_> = repräsentationen.into_iter().collect();
    let num_eindeutig = set.len();

    // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids.
    expect_eq(num, num_eindeutig)?;
    Ok(())
}

#[test]
fn repräsentation_kopie_identisch() -> Result<(), Expectation> {
    init_test_logging();

    let id = GleisId::<()>::neu().expect("Erste Id.");
    let mut repräsentationen = vec![id.repräsentation()];
    // eine Kopie liefert das selbe Ergebnis
    let clone = id.clone();
    repräsentationen.push(clone.repräsentation());
    // ein erneuter Aufruf liefert das selbe Ergebnis
    repräsentationen.push(id.repräsentation());
    repräsentationen.push(clone.repräsentation());

    let num = repräsentationen.len();
    let set: BTreeSet<_> = repräsentationen.into_iter().collect();
    let num_eindeutig = set.len();

    // Alle Ids sind identisch, dementsprechend hat die Menge nur ein Element.
    expect_gt(num, 1)?;
    expect_eq(num_eindeutig, 1)?;
    Ok(())
}

#[test]
fn clone() -> Result<(), Expectation> {
    init_test_logging();

    let id = GleisId::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!");
    let id_clone = id.clone();
    // durch drop des Original-Werts wird die Id nicht wieder freigegeben.
    drop(id);

    let ids =
        (0..32).map(|_i| GleisId::<()>::neu().expect("Test verwendet weniger als usize::MAX Ids!"));

    // alle erzeugten Ids haben einen anderen Wert.
    ids.map(|id| expect_ne(id_clone.clone(), id)).collect::<Result<_, ExpectNe>>()?;
    Ok(())
}
