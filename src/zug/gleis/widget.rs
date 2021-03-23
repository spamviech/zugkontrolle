//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use cairo::Context;
use log::*;
use rstar::{primitives::PointWithData, RTree};

use super::anchor::*;
use super::gerade::*;
use super::kreuzung::*;
use super::kurve::*;
use super::types::*;
use super::weiche::*;

pub trait Zeichnen {
    /// Maximale Breite
    fn width(&self) -> u64;

    /// Maximale Höhe
    fn height(&self) -> u64;

    /// Darstellen im Kontext an Position (0,0).
    ///
    /// Der Kontext wurde bereits für eine Darstellung in korrekter Position transformiert.
    fn zeichne(&self, c: Context);

    /// Identifier for AnchorPoints.
    /// An enum is advised, but others work as well.
    ///
    /// Since they are used as keys in an HashMap, Hash+Eq must be implemented (derived).
    type AnchorName;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> AnchorPointMap<Self::AnchorName>;
}

/// Definition eines Gleises
#[derive(Debug, Clone)]
pub enum GleisDefinition<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    Weiche(Weiche<Z>),
    Kreuzung(Kreuzung<Z>),
}

impl<Z: Zugtyp + Debug> GleisDefinition<Z> {
    fn into_zeichnen(&self) -> &impl Zeichnen {
        match self {
            GleisDefinition::Gerade(gerade) => gerade,
            _ => unimplemented!("{:?}.into_zeichnen", self),
        }
    }
}

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone)]
pub struct Position {
    pub x: CanvasX,
    pub y: CanvasY,
    pub winkel: Angle,
}
impl Position {
    /// AnchorPosition nachdem das Objekt an die Position bewegt und um den Winkel gedreht wird.
    pub fn transformation(&self, anchor: AnchorPosition) -> AnchorPosition {
        let x = CanvasX(self.x.0 + anchor.x.0 * self.winkel.cos() - anchor.y.0 * self.winkel.sin());
        let y = CanvasY(self.y.0 + anchor.x.0 * self.winkel.sin() + anchor.y.0 * self.winkel.cos());
        AnchorPosition { x, y }
    }
}

/// R-Tree of all anchor points, specifying the corresponding widget definition
type AnchorPointRTree<Z> = RTree<PointWithData<GleisId<Z>, AnchorPosition>>;

/// If GleisIdLock<Z>::read contains a Some, the GleisId<Z> is guaranteed to be valid.
#[derive(Debug)]
pub struct GleisIdLock<Z>(Arc<RwLock<Option<GleisId<Z>>>>);

impl<Z> Clone for GleisIdLock<Z> {
    fn clone(&self) -> Self {
        GleisIdLock(self.0.clone())
    }
}

impl<Z: Debug> GleisIdLock<Z> {
    fn new(gleis_id: u64) -> GleisIdLock<Z> {
        GleisIdLock(Arc::new(RwLock::new(Some(GleisId::new(gleis_id)))))
    }

    pub fn read(&self) -> RwLockReadGuard<Option<GleisId<Z>>> {
        self.0.read().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }

    fn write(&self) -> RwLockWriteGuard<Option<GleisId<Z>>> {
        self.0.write().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }
}

/// Identifier for a Gleis.  Will probably change between restarts.
///
/// The API will only provide &GleisIdLock<Z>.
#[derive(Debug)]
pub struct GleisId<Z>(u64, PhantomData<*const Z>);
impl<Z> GleisId<Z> {
    fn new(gleis_id: u64) -> GleisId<Z> {
        GleisId(gleis_id, PhantomData)
    }
}
// explicit implementation needed due to phantom type
// derived instead required corresponding Trait implemented on phantom type
impl<Z> PartialEq for GleisId<Z> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<Z> Eq for GleisId<Z> {}
impl<Z> Hash for GleisId<Z> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(Debug, Clone)]
pub struct Gleis<Z> {
    definition: GleisDefinition<Z>,
    position: Position,
}
#[derive(Debug, Clone)]
pub struct Gleise<Z>(Arc<RwLock<GleiseInternal<Z>>>);

impl<Z: Debug> Gleise<Z> {
    // fn read(&self) -> RwLockReadGuard<GleiseInternal<Z>> {
    //     self.0.read().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleiseMap"))
    // }
    fn write(&self) -> RwLockWriteGuard<GleiseInternal<Z>> {
        self.0.write().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleiseMap"))
    }
}

#[derive(Debug)]
struct GleiseInternal<Z> {
    map: HashMap<GleisId<Z>, Gleis<Z>>,
    anchor_points: AnchorPointRTree<Z>,
    next_id: u64,
}

impl<Z: Zugtyp + Debug + Eq> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add(&mut self, gleis: Gleis<Z>) -> GleisIdLock<Z> {
        let mut gleise = self.write();
        let gleis_id: u64 = gleise.next_id;
        let gleis_id_lock: GleisIdLock<Z> = GleisIdLock::new(gleis_id);
        // increase next id
        gleise.next_id += 1;
        // add to anchor_points
        let Gleis { definition, position } = &gleis;
        for anchor in definition.into_zeichnen().anchor_points().values() {
            gleise.anchor_points.insert(PointWithData::new(
                GleisId::new(gleis_id),
                position.transformation(anchor.position),
            ))
        }
        // add to HashMap
        gleise.map.insert(GleisId::new(gleis_id), gleis);
        gleis_id_lock
    }
    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate(&mut self, gleis_id: &GleisId<Z>, position_neu: Position) {
        let GleiseInternal { map, anchor_points, next_id: _ } = &mut *self.write();
        let Gleis { definition, position } =
            map.get_mut(gleis_id).expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
        // delete from anchor_points
        for anchor in definition.into_zeichnen().anchor_points().values() {
            // delete old anchor position
            anchor_points.remove(&PointWithData::new(
                // TODO is it possible to use the reference we have?
                GleisId::new(gleis_id.0),
                position.transformation(anchor.position),
            ));
            // add new anchor position
            anchor_points.insert(PointWithData::new(
                // TODO is it possible to use the reference we have?
                GleisId::new(gleis_id.0),
                position_neu.transformation(anchor.position),
            ))
        }
        // store new position
        *position = position_neu;
    }
    /// Remove the Gleis associated the the GleisId.
    ///
    /// The value contained inside GleisIdLock<Z> is set to None.
    /// Removing a value multiple times is no error.
    /// Only the first remove has an effect.
    pub fn remove(&mut self, gleis_id_lock: GleisIdLock<Z>) {
        let mut gleise = self.write();
        let mut optional_id = gleis_id_lock.write();
        // only delete once
        if let Some(gleis_id) = optional_id.as_ref() {
            let Gleis { definition, position } = gleise
                .map
                .remove(gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            // delete from anchor_points
            for anchor in definition.into_zeichnen().anchor_points().values() {
                gleise.anchor_points.remove(&PointWithData::new(
                    // TODO is it possible to use the reference we have?
                    GleisId::new(gleis_id.0),
                    position.transformation(anchor.position),
                ));
            }
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", description, poisoned);
    poisoned.into_inner()
}