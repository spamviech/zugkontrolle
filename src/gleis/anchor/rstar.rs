//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use crate::gleis::types::canvas;
use crate::gleis::widget::{Any, GleisId};

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub(crate) struct RTree(rstar::RTree<PointWithData<GleisId<Any>, canvas::Point>>);
impl RTree {
    /// create a new RTree to store anchors with position data
    pub(crate) fn new() -> Self {
        RTree(rstar::RTree::new())
    }
    /// insert an anchor into the RTree
    pub(crate) fn insert(&mut self, gleis_id: GleisId<Any>, position: canvas::Point) {
        self.0.insert(PointWithData::new(gleis_id, position))
    }
    /// remove one copy of the specified anchor from the RTree
    pub(crate) fn remove(&mut self, gleis_id: GleisId<Any>, &position: &canvas::Point) {
        self.0.remove(&PointWithData::new(gleis_id, position));
    }
    /// check if an anchor with a different id is present at the specified position
    pub(crate) fn has_other_id_at_point(
        &self,
        gleis_id: &GleisId<Any>,
        &position: &canvas::Point,
    ) -> bool {
        // TODO also store and check if direction matches?
        let other_ids_at_point = self
            .0
            .locate_within_distance(position, 2.5)
            .filter(|&PointWithData { data, .. }| data != gleis_id)
            .count();
        other_ids_at_point > 0
    }
}
