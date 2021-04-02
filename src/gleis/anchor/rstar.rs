//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use super::point;
use crate::gleis::widget::{Any, GleisId};

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub(crate) struct RTree(rstar::RTree<PointWithData<GleisId<Any>, point::Position>>);
impl RTree {
    pub(crate) fn new() -> Self {
        RTree(rstar::RTree::new())
    }
    pub(crate) fn insert(&mut self, t: PointWithData<GleisId<Any>, point::Position>) {
        self.0.insert(t)
    }
    pub(crate) fn remove(&mut self, t: &PointWithData<GleisId<Any>, point::Position>) {
        self.0.remove(t);
    }
    // Vec return type since the original return type is private and /impl Iterator/ has 'static requirement
    pub(crate) fn has_other_id_at_point(
        &self,
        gleis_id: &GleisId<Any>,
        position: &point::Position,
    ) -> bool {
        // TODO use other data structure to also find "close" points,
        // not only completely identical ones (double precision is a thing!)
        let other_ids_at_point = self
            .0
            .locate_all_at_point(position)
            .map(|PointWithData { data, .. }| data)
            .filter(|&id| id != gleis_id)
            .count();
        other_ids_at_point > 0
    }
}
