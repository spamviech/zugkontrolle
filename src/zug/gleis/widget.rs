use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use cairo::Context;
use log::*;
use nonempty::NonEmpty;
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

    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> AnchorPointMap;
}

/// Definition eines Gleises
#[derive(Debug, Clone)]
pub enum GleisDefinition<Z> {
    Gerade(Gerade<Z>),
    Kurve(Kurve<Z>),
    Weiche(Weiche<Z>),
    Kreuzung(Kreuzung<Z>),
}

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone)]
pub struct Position {
    pub x: CanvasX,
    pub y: CanvasY,
    pub winkel: Angle,
}

/// R-Tree of all anchor points, specifying the corresponding widget definition
pub type AnchorPointRTree<Z> = RTree<PointWithData<NonEmpty<GleisIdLock<Z>>, AnchorPosition>>;

/// If GleisIdLock<Z>::read contains a Some, the GleisId<Z> is guaranteed to be valid.
#[derive(Debug)]
pub struct GleisIdLock<Z>(Arc<RwLock<Option<GleisId<Z>>>>);

impl<Z> Clone for GleisIdLock<Z> {
    fn clone(&self) -> Self {
        GleisIdLock(self.0.clone())
    }
}

impl<Z: Debug> GleisIdLock<Z> {
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

impl<Z: Debug> Gleise<Z> {
    pub fn add(&mut self, gleis: Gleis<Z>) -> GleisIdLock<Z> {
        let mut gleise = self.write();
        let gleis_id: u64 = gleise.next_id;
        // increase next id
        gleise.next_id += 1;
        // TODO add to AnchorMap
        let gleis_id_lock: GleisIdLock<Z> =
            GleisIdLock(Arc::new(RwLock::new(Some(GleisId(gleis_id, PhantomData)))));
        let _bla = &gleis;
        // TODO add to HashMap
        gleise.map.insert(GleisId(gleis_id, PhantomData), gleis);
        gleis_id_lock
    }

    pub fn remove(&mut self, gleis_id_lock: GleisIdLock<Z>) {
        let mut gleise = self.write();
        let mut optional_id = gleis_id_lock.write();
        // only delete once
        if let Some(gleis_id) = optional_id.as_ref() {
            let _gleis = gleise.map.remove(gleis_id);
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}", description, poisoned);
    poisoned.into_inner()
}
