//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use crate::application::{
    gleis::gleise::id::AnyId,
    typen::{winkel, Skalar, Trigonometrie, Vektor, Winkel},
    verbindung::Anchor,
};

const SEARCH_RADIUS: Skalar = Skalar(5.0);

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(zugkontrolle_derive::Debug)]
pub(crate) struct RTree<Z>(rstar::RTree<PointWithData<(AnyId<Z>, Winkel), Vektor>>);
impl<Z> RTree<Z> {
    /// create a new RTree to store anchors with position data
    pub(crate) fn new() -> Self {
        RTree(rstar::RTree::new())
    }

    /// insert an anchor into the RTree
    pub(crate) fn insert(&mut self, gleis_id: AnyId<Z>, Anchor { position, richtung }: Anchor) {
        self.0.insert(PointWithData::new((gleis_id, richtung), position))
    }

    /// remove one copy of the specified anchor from the RTree
    pub(crate) fn remove(&mut self, gleis_id: AnyId<Z>, &Anchor { position, richtung }: &Anchor) {
        self.0.remove(&PointWithData::new((gleis_id, richtung), position));
    }

    /// Check if an anchor with a different id is present at the specified position,
    /// returning the first found.
    pub(crate) fn get_other_id_at_point(
        &self,
        gleis_id: AnyId<Z>,
        &Anchor { position, richtung: _ }: &Anchor,
    ) -> Option<Anchor> {
        self.0.locate_within_distance(position, SEARCH_RADIUS.0).find_map(|point_with_data| {
            let stored_position = point_with_data.position();
            let PointWithData { data: (stored_id, stored_direction), .. } = point_with_data;
            if stored_id != &gleis_id {
                Some(Anchor { position: *stored_position, richtung: *stored_direction })
            } else {
                None
            }
        })
    }

    /// Check if an anchor with a different id is present at the specified position,
    /// returning the first pointing in the opposite direction.
    pub(crate) fn has_other_and_grabbed_id_at_point(
        &self,
        gleis_id: &AnyId<Z>,
        is_grabbed: impl Fn(&AnyId<Z>) -> bool,
        &Anchor { position, richtung }: &Anchor,
    ) -> (bool, bool) {
        let mut opposing: bool = false;
        let mut grabbed: bool = false;
        for point_with_data in self.0.locate_within_distance(position, SEARCH_RADIUS.0) {
            let PointWithData { data: (stored_id, gespeicherte_richtung), .. } = point_with_data;
            if !opposing
                && stored_id != gleis_id
                && (winkel::PI + richtung - gespeicherte_richtung).normalisiert().abs()
                    < Winkel(0.1)
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
