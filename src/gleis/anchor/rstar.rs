//! storing of anchors in a spacial container

use rstar::primitives::PointWithData;

use super::{point, Lookup};
use crate::gleis::types::Zeichnen;
use crate::gleis::widget::{GleisId, Position};

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub struct RTree<Z>(rstar::RTree<PointWithData<GleisId<Z>, point::Position>>);
impl<Z> RTree<Z> {
    // FIXME should be used somewhere, right??
    #[allow(dead_code)]
    pub(crate) fn new() -> RTree<Z> {
        RTree(rstar::RTree::new())
    }
    fn insert(&mut self, t: PointWithData<GleisId<Z>, point::Position>) {
        self.0.insert(t)
    }
    fn remove(&mut self, t: &PointWithData<GleisId<Z>, point::Position>) {
        self.0.remove(t);
    }
}

// see section `Cheating Rank-2`
// https://leshow.github.io/post/cheat_rank_n/
pub trait Transform {
    fn transform<T, F, Z>(&self, anchor_points: &mut RTree<Z>, definition: &T, gleis_id: F)
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
        F: Fn() -> GleisId<Z>;
}
/// Add anchor points
pub struct Add {
    pub position: Position,
}
impl Transform for Add {
    fn transform<T, F, Z>(&self, anchor_points: &mut RTree<Z>, definition: &T, gleis_id: F)
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
        F: Fn() -> GleisId<Z>,
    {
        definition.anchor_points().foreach(|anchor| {
            anchor_points.insert(PointWithData::new(
                gleis_id(),
                self.position.transformation(anchor.position),
            ))
        })
    }
}

/// Relocate (move) anchor points
pub struct Relocate {
    pub from: Position,
    pub to: Position,
}
impl Transform for Relocate {
    fn transform<T, F, Z>(&self, anchor_points: &mut RTree<Z>, definition: &T, gleis_id: F)
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
        F: Fn() -> GleisId<Z>,
    {
        definition.anchor_points().foreach(|anchor| {
            anchor_points
                .remove(&PointWithData::new(gleis_id(), self.from.transformation(anchor.position)));
            anchor_points
                .insert(PointWithData::new(gleis_id(), self.to.transformation(anchor.position)))
        })
    }
}

/// Remove (delete) anchor points
pub struct Remove {
    pub position: Position,
}
impl Transform for Remove {
    fn transform<T, F, Z>(&self, anchor_points: &mut RTree<Z>, definition: &T, gleis_id: F)
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
        F: Fn() -> GleisId<Z>,
    {
        definition.anchor_points().foreach(|anchor| {
            anchor_points.remove(&PointWithData::new(
                gleis_id(),
                self.position.transformation(anchor.position),
            ));
        })
    }
}
