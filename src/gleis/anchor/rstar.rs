//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use super::point::Anchor;
use crate::gleis::types::canvas;
use crate::gleis::widget::{Any, GleisId};

const SEARCH_RADIUS: f32 = 5.0;

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(Debug)]
pub(crate) struct RTree(rstar::RTree<PointWithData<(GleisId<Any>, canvas::Vector), canvas::Point>>);
impl RTree {
    /// create a new RTree to store anchors with position data
    pub(crate) fn new() -> Self {
        RTree(rstar::RTree::new())
    }

    /// insert an anchor into the RTree
    pub(crate) fn insert(
        &mut self,
        gleis_id: GleisId<Any>,
        Anchor { position, direction }: Anchor,
    ) {
        self.0.insert(PointWithData::new((gleis_id, direction), position))
    }

    /// remove one copy of the specified anchor from the RTree
    pub(crate) fn remove(
        &mut self,
        gleis_id: GleisId<Any>,
        &Anchor { position, direction }: &Anchor,
    ) {
        self.0.remove(&PointWithData::new((gleis_id, direction), position));
    }

    /// Check if an anchor with a different id is present at the specified position,
    /// returning the first found.
    pub(crate) fn get_other_id_at_point(
        &self,
        gleis_id: GleisId<Any>,
        &Anchor { position, direction: _ }: &Anchor,
    ) -> Option<Anchor> {
        self.0.locate_within_distance(position, SEARCH_RADIUS).find_map(|point_with_data| {
            let stored_position = point_with_data.position();
            let PointWithData { data: (stored_id, stored_direction), .. } = point_with_data;
            if stored_id != &gleis_id {
                Some(Anchor { position: *stored_position, direction: *stored_direction })
            } else {
                None
            }
        })
    }

    /// Check if an anchor with a different id is present at the specified position,
    /// returning the first pointing in the opposite direction.
    pub(crate) fn has_other_and_grabbed_id_at_point(
        &self,
        gleis_id: &GleisId<Any>,
        is_grabbed: impl Fn(&GleisId<Any>) -> bool,
        &Anchor { position, direction }: &Anchor,
    ) -> (bool, bool) {
        let mut opposing: bool = false;
        let mut grabbed: bool = false;
        for point_with_data in self.0.locate_within_distance(position, SEARCH_RADIUS) {
            let PointWithData { data: (stored_id, stored_direction), .. } = point_with_data;
            if !opposing
                && stored_id != gleis_id
                && direction.scalar_product_normalized(stored_direction) < -0.95
            {
                opposing = true;
                if grabbed {
                    break;
                }
            }
            if !grabbed
                && ((is_grabbed(stored_id) && stored_id != gleis_id)
                    || (is_grabbed(gleis_id) && opposing))
            {
                grabbed = true;
                if opposing {
                    break;
                }
            }
        }
        (opposing, grabbed)
    }
}
