//! Tests für [GleisId].

use std::collections::BTreeSet;

use crate::{
    gleis::gleise::id::GleisId2,
    test_util::{expect_eq, expect_ne, init_test_logging, ExpectNe, Expectation},
};

#[test]
fn eindeutig() -> Result<(), Expectation> {
    init_test_logging();

    let ids: Vec<_> = (0..32)
        .map(|i| (i, GleisId2::<()>::neu().expect("test verwendet weniger als usize::MAX Ids.")))
        .filter_map(|(i, id)| (i % 2 == 0).then_some(id))
        .collect();
    let num = ids.len();
    let set: BTreeSet<_> = ids.into_iter().collect();
    let num_eindeutig = set.len();

    // die Anzahl an erzeugten Ids ist identisch zur Anzahl der eindeutigen Ids.
    expect_eq(num, num_eindeutig)?;
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
