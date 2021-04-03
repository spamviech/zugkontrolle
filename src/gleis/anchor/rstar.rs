//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use super::point;
use crate::gleis::widget::{Any, GleisId};

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub(crate) struct RTree(rstar::RTree<PointWithData<GleisId<Any>, point::Position>>);
impl RTree {
    /// create a new RTree to store anchors with position data
    pub(crate) fn new() -> Self {
        RTree(rstar::RTree::new())
    }
    /// insert an anchor into the RTree
    pub(crate) fn insert<T>(&mut self, gleis_id: &GleisId<T>, position: point::Position) {
        self.0.insert(PointWithData::new(gleis_id.as_any(), position))
    }
    /// remove one copy of the specified anchor from the RTree
    pub(crate) fn remove<T>(&mut self, gleis_id: &GleisId<T>, &position: &point::Position) {
        self.0.remove(&PointWithData::new(gleis_id.as_any(), position));
    }
    /// check if an anchor with a different id is present at the specified position
    pub(crate) fn has_other_id_at_point(
        &self,
        gleis_id: &GleisId<Any>,
        &position: &point::Position,
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
