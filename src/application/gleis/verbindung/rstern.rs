//! storing of anchors in a spacial container

pub(crate) use rstar::primitives::PointWithData;

use crate::{
    application::{
        gleis::gleise::id::AnyId,
        typen::{winkel, Skalar, Trigonometrie, Vektor, Winkel},
        verbindung::Verbindung,
    },
    zugtyp::Zugtyp,
};

const SEARCH_RADIUS: Skalar = Skalar(5.0);

/// R-Tree of all anchor points, specifying the corresponding widget definition
#[derive(zugkontrolle_derive::Debug)]
pub(crate) struct RStern<Z: Zugtyp>(rstar::RTree<PointWithData<(AnyId<Z>, Winkel), Vektor>>);
impl<Z: Zugtyp> RStern<Z> {
    /// create a new RTree to store anchors with position data
    pub(crate) fn neu() -> Self {
        RStern(rstar::RTree::new())
    }

    /// insert an anchor into the RTree
    pub(crate) fn hinzufügen(
        &mut self,
        gleis_id: AnyId<Z>,
        Verbindung { position, richtung }: Verbindung,
    ) {
        self.0.insert(PointWithData::new((gleis_id, richtung), position))
    }

    /// remove one copy of the specified anchor from the RTree
    pub(crate) fn entfernen(
        &mut self,
        gleis_id: AnyId<Z>,
        &Verbindung { position, richtung }: &Verbindung,
    ) {
        self.0.remove(&PointWithData::new((gleis_id, richtung), position));
    }

    /// Check if an anchor with a different id is present at the specified position,
    /// returning the first found.
    pub(crate) fn andere_id_an_position(
        &self,
        gleis_id: AnyId<Z>,
        &Verbindung { position, richtung: _ }: &Verbindung,
    ) -> Option<Verbindung> {
        self.0.locate_within_distance(position, SEARCH_RADIUS.0).find_map(|point_with_data| {
            let stored_position = point_with_data.position();
            let PointWithData { data: (stored_id, stored_direction), .. } = point_with_data;
            if stored_id != &gleis_id {
                Some(Verbindung { position: *stored_position, richtung: *stored_direction })
            } else {
                None
            }
        })
    }

    /// Check if an anchor with a different id pointing in opposite direction is present
    /// at the specified position, and if a grabbed id is present pointing in any direction.
    pub(crate) fn hat_andere_id_und_grabbed_an_position(
        &self,
        gleis_id: &AnyId<Z>,
        is_grabbed: impl Fn(&AnyId<Z>) -> bool,
        &Verbindung { position, richtung }: &Verbindung,
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
