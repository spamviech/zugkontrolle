//! Summen-Typ für Gleis-Definition (+ Konvertierungs-Trait)

use std::fmt::Debug;

use super::anchor;
use super::gerade::Gerade;
use super::kreuzung::Kreuzung;
use super::kurve::Kurve;
use super::types::Zugtyp;
use super::weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche};
use super::widget::GleisId;

/// Definition eines Gleises
#[derive(Debug, Clone)]
pub enum GleisDefinition<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    Weiche(Weiche<Z>),
    DreiwegeWeiche(DreiwegeWeiche<Z>),
    KurvenWeiche(KurvenWeiche<Z>),
    SKurvenWeiche(SKurvenWeiche<Z>),
    Kreuzung(Kreuzung<Z>),
}
impl<Z: Debug + Zugtyp> GleisDefinition<Z> {
    pub(crate) fn execute<T: anchor::rstar::Transform, F: Fn() -> GleisId<Z>>(
        &self,
        action: &T,
        anchor_points: &mut anchor::rstar::RTree<Z>,
        gleis_id: F,
    ) {
        match self {
            GleisDefinition::Gerade(gerade) => action.transform(anchor_points, gerade, gleis_id),
            GleisDefinition::Kurve(kurve) => action.transform(anchor_points, kurve, gleis_id),
            GleisDefinition::Weiche(weiche) => action.transform(anchor_points, weiche, gleis_id),
            GleisDefinition::DreiwegeWeiche(dreiwege_weiche) => {
                action.transform(anchor_points, dreiwege_weiche, gleis_id)
            }
            GleisDefinition::KurvenWeiche(kurven_weiche) => {
                action.transform(anchor_points, kurven_weiche, gleis_id)
            }
            GleisDefinition::SKurvenWeiche(s_kurven_weiche) => {
                action.transform(anchor_points, s_kurven_weiche, gleis_id)
            }
            GleisDefinition::Kreuzung(kreuzung) => {
                action.transform(anchor_points, kreuzung, gleis_id)
            }
        }
    }
}
pub trait Definition<Z> {
    /// Konvertiere in eine GleisDefinition
    fn definition(self) -> GleisDefinition<Z>;
}
impl<Z> Definition<Z> for Gerade<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::Gerade(self)
    }
}
impl<Z> Definition<Z> for Kurve<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::Kurve(self)
    }
}
impl<Z> Definition<Z> for Weiche<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::Weiche(self)
    }
}
impl<Z> Definition<Z> for DreiwegeWeiche<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::DreiwegeWeiche(self)
    }
}
impl<Z> Definition<Z> for KurvenWeiche<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::KurvenWeiche(self)
    }
}
impl<Z> Definition<Z> for SKurvenWeiche<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::SKurvenWeiche(self)
    }
}
impl<Z> Definition<Z> for Kreuzung<Z> {
    fn definition(self) -> GleisDefinition<Z> {
        GleisDefinition::Kreuzung(self)
    }
}
