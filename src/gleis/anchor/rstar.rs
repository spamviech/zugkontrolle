//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use super::{point, Lookup};
use crate::gleis::types::Zeichnen;
use crate::gleis::widget::{GleisId, Position};

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub struct RTree<Z>(rstar::RTree<PointWithData<GleisId<Z>, point::Position>>);
impl<Z> RTree<Z> {
    pub(crate) fn new() -> RTree<Z> {
        RTree(rstar::RTree::new())
    }
    pub(crate) fn insert(&mut self, t: PointWithData<GleisId<Z>, point::Position>) {
        self.0.insert(t)
    }
    pub(crate) fn remove(&mut self, t: &PointWithData<GleisId<Z>, point::Position>) {
        self.0.remove(t);
    }
    // Vec return type since the original return type is private and /impl Iterator/ has 'static requirement
    pub(crate) fn has_other_id_at_point(
        &self,
        gleis_id: &GleisId<Z>,
        position: &point::Position,
    ) -> bool {
        let other_ids_at_point = self
            .0
            .locate_all_at_point(position)
            .map(|PointWithData { data, .. }| data)
            .filter(|&id| id != gleis_id)
            .count();
        other_ids_at_point > 0
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

/// Relocate (move) anchor points
pub(crate) struct Relocate {
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
pub(crate) struct Remove {
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
