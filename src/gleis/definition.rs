//! Summen-Typ für Gleis-Definition (+ Konvertierungs-Trait)

use std::fmt::Debug;

use super::anchor::{self, Lookup};
use super::gerade::Gerade;
use super::kreuzung::Kreuzung;
use super::kurve::Kurve;
use super::types::{Cairo, CanvasAbstand, Zeichnen, Zugtyp};
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

    pub(crate) fn verwende<T: Verwende>(&self, action: &T) -> T::Output {
        match self {
            GleisDefinition::Gerade(gerade) => action.verwende::<Gerade<Z>, Z>(gerade),
            GleisDefinition::Kurve(kurve) => action.verwende::<Kurve<Z>, Z>(kurve),
            GleisDefinition::Weiche(weiche) => action.verwende::<Weiche<Z>, Z>(weiche),
            GleisDefinition::DreiwegeWeiche(dreiwege_weiche) => {
                action.verwende::<DreiwegeWeiche<Z>, Z>(dreiwege_weiche)
            }
            GleisDefinition::KurvenWeiche(kurven_weiche) => {
                action.verwende::<KurvenWeiche<Z>, Z>(kurven_weiche)
            }
            GleisDefinition::SKurvenWeiche(s_kurven_weiche) => {
                action.verwende::<SKurvenWeiche<Z>, Z>(s_kurven_weiche)
            }
            GleisDefinition::Kreuzung(kreuzung) => action.verwende::<Kreuzung<Z>, Z>(kreuzung),
        }
    }
    pub(crate) fn verwende_anchor_points<T: VerwendeAnchorPoints>(
        &self,
        action: &T,
        anchor_points: &anchor::rstar::RTree<Z>,
        gleis_id: &GleisId<Z>,
    ) {
        match self {
            GleisDefinition::Gerade(gerade) => {
                action.verwende_anchor_points(anchor_points, gerade, gleis_id)
            }
            GleisDefinition::Kurve(kurve) => {
                action.verwende_anchor_points(anchor_points, kurve, gleis_id)
            }
            GleisDefinition::Weiche(weiche) => {
                action.verwende_anchor_points(anchor_points, weiche, gleis_id)
            }
            GleisDefinition::DreiwegeWeiche(dreiwege_weiche) => {
                action.verwende_anchor_points(anchor_points, dreiwege_weiche, gleis_id)
            }
            GleisDefinition::KurvenWeiche(kurven_weiche) => {
                action.verwende_anchor_points(anchor_points, kurven_weiche, gleis_id)
            }
            GleisDefinition::SKurvenWeiche(s_kurven_weiche) => {
                action.verwende_anchor_points(anchor_points, s_kurven_weiche, gleis_id)
            }
            GleisDefinition::Kreuzung(kreuzung) => {
                action.verwende_anchor_points(anchor_points, kreuzung, gleis_id)
            }
        }
    }
}
impl<Z> From<Gerade<Z>> for GleisDefinition<Z> {
    fn from(gerade: Gerade<Z>) -> GleisDefinition<Z> {
        GleisDefinition::Gerade(gerade)
    }
}
impl<Z> From<Kurve<Z>> for GleisDefinition<Z> {
    fn from(kurve: Kurve<Z>) -> GleisDefinition<Z> {
        GleisDefinition::Kurve(kurve)
    }
}
impl<Z> From<Weiche<Z>> for GleisDefinition<Z> {
    fn from(weiche: Weiche<Z>) -> GleisDefinition<Z> {
        GleisDefinition::Weiche(weiche)
    }
}
impl<Z> From<DreiwegeWeiche<Z>> for GleisDefinition<Z> {
    fn from(dreiwege_weiche: DreiwegeWeiche<Z>) -> GleisDefinition<Z> {
        GleisDefinition::DreiwegeWeiche(dreiwege_weiche)
    }
}
impl<Z> From<KurvenWeiche<Z>> for GleisDefinition<Z> {
    fn from(kurven_weiche: KurvenWeiche<Z>) -> GleisDefinition<Z> {
        GleisDefinition::KurvenWeiche(kurven_weiche)
    }
}
impl<Z> From<SKurvenWeiche<Z>> for GleisDefinition<Z> {
    fn from(s_kurven_weiche: SKurvenWeiche<Z>) -> GleisDefinition<Z> {
        GleisDefinition::SKurvenWeiche(s_kurven_weiche)
    }
}
impl<Z> From<Kreuzung<Z>> for GleisDefinition<Z> {
    fn from(kreuzung: Kreuzung<Z>) -> GleisDefinition<Z> {
        GleisDefinition::Kreuzung(kreuzung)
    }
}

// see section `Cheating Rank-2`
// https://leshow.github.io/post/cheat_rank_n/
pub trait Verwende {
    type Output;
    fn verwende<T: Zeichnen, Z>(&self, definition: &T) -> Self::Output;
}

/// Berechne Breite/Höhe
pub enum Size {
    Width,
    Height,
}
impl Verwende for Size {
    type Output = u64;
    fn verwende<T: Zeichnen, Z>(&self, definition: &T) -> u64 {
        match self {
            Size::Width => definition.width(),
            Size::Height => definition.height(),
        }
    }
}

/// zeichne GleisDefinition an aktuelle Position
pub struct Zeichne<'s, 't>(pub &'s Cairo<'t>);
impl<'s, 't> Verwende for Zeichne<'s, 't> {
    type Output = ();
    fn verwende<T: Zeichnen, Z>(&self, definition: &T) {
        definition.zeichne(&self.0)
    }
}

// see section `Cheating Rank-2`
// https://leshow.github.io/post/cheat_rank_n/
pub trait VerwendeAnchorPoints {
    fn verwende_anchor_points<T, Z>(
        &self,
        anchor_points: &anchor::rstar::RTree<Z>,
        definition: &T,
        gleis_id: &GleisId<Z>,
    ) where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>;
}
pub struct ZeichneAnchorPoints<'s, 't>(pub &'s Cairo<'t>);
impl<'s, 't> VerwendeAnchorPoints for ZeichneAnchorPoints<'s, 't> {
    fn verwende_anchor_points<T, Z>(
        &self,
        _anchor_points: &anchor::rstar::RTree<Z>,
        definition: &T,
        _gleis_id: &GleisId<Z>,
    ) where
        T: Zeichnen,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        // current position???
        // we need to compare with other anchor points
        definition.anchor_points().foreach(
            |anchor::Point {
                 position: anchor::Position { x, y },
                 direction: anchor::Direction { dx, dy },
             }| {
                // TODO check current position, if match with different gleisId use green instead
                // self.0.set_source_rgb(0., 1., 0.);
                self.0.set_source_rgb(0., 0., 1.);
                self.0.move_to(*x, *y);
                self.0.line_to(
                    *x + 5. * CanvasAbstand::from(*dx),
                    *y + 5. * CanvasAbstand::from(*dy),
                );
                self.0.stroke();
            },
        )
    }
}
