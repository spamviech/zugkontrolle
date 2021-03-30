//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use log::*;

use super::anchor::{self, Lookup};
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
    fn zeichne(&self, cairo: &Cairo);

    /// Identifier for AnchorPoints.
    /// An enum is advised, but others work as well.
    ///
    /// Since they are used as keys in an HashMap, Hash+Eq must be implemented (derived).
    type AnchorName;
    /// Storage Type for AnchorPoints, should implement /AnchorLookup<Self::AnchorName>/.
    type AnchorPoints;
    /// AnchorPoints (Anschluss-Möglichkeiten für andere Gleise).
    ///
    /// Position ausgehend von zeichnen bei (0,0),
    /// Richtung nach außen zeigend.
    fn anchor_points(&self) -> Self::AnchorPoints;
}

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
    fn execute<F: anchor::rstar::Transform>(
        &self,
        action: &F,
        anchor_points: &mut anchor::rstar::RTree<Z>,
        gleis_id: u64,
    ) {
        match self {
            GleisDefinition::Gerade(gerade) => {
                action.transform(anchor_points, gerade, || GleisId::new(gleis_id))
            }
            GleisDefinition::Kurve(kurve) => {
                action.transform(anchor_points, kurve, || GleisId::new(gleis_id))
            }
            GleisDefinition::Weiche(weiche) => {
                action.transform(anchor_points, weiche, || GleisId::new(gleis_id))
            }
            GleisDefinition::DreiwegeWeiche(dreiwege_weiche) => {
                action.transform(anchor_points, dreiwege_weiche, || GleisId::new(gleis_id))
            }
            GleisDefinition::KurvenWeiche(kurven_weiche) => {
                action.transform(anchor_points, kurven_weiche, || GleisId::new(gleis_id))
            }
            GleisDefinition::SKurvenWeiche(s_kurven_weiche) => {
                action.transform(anchor_points, s_kurven_weiche, || GleisId::new(gleis_id))
            }
            GleisDefinition::Kreuzung(kreuzung) => {
                action.transform(anchor_points, kreuzung, || GleisId::new(gleis_id))
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

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone)]
pub struct Position {
    pub x: CanvasX,
    pub y: CanvasY,
    pub winkel: Angle,
}
impl Position {
    /// anchor::Position nachdem das Objekt an die Position bewegt und um den Winkel gedreht wird.
    pub fn transformation(&self, anchor: anchor::Position) -> anchor::Position {
        let x = CanvasX(self.x.0 + anchor.x.0 * self.winkel.cos() - anchor.y.0 * self.winkel.sin());
        let y = CanvasY(self.y.0 + anchor.x.0 * self.winkel.sin() + anchor.y.0 * self.winkel.cos());
        anchor::Position { x, y }
    }
}

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
    anchor_points: anchor::rstar::RTree<Z>,
    next_id: u64,
}

impl<Z: Debug> GleiseInternal<Z> {
    fn next_id(&mut self) -> (u64, GleisIdLock<Z>) {
        let gleis_id: u64 = self.next_id;
        let gleis_id_lock: GleisIdLock<Z> = GleisIdLock::new(gleis_id);
        // increase next id
        self.next_id += 1;
        (gleis_id, gleis_id_lock)
    }
}

impl<Z: Zugtyp + Debug + Eq> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add(&mut self, gleis: Gleis<Z>) -> GleisIdLock<Z> {
        let mut gleise = self.write();
        let (gleis_id, gleis_id_lock) = gleise.next_id();
        // increase next id
        gleise.next_id += 1;
        // add to anchor_points
        let Gleis { definition, position } = &gleis;
        definition.execute(
            &anchor::rstar::Add { position: position.clone() },
            &mut gleise.anchor_points,
            gleis_id,
        );
        // add to HashMap
        gleise.map.insert(GleisId::new(gleis_id), gleis);
        gleis_id_lock
    }

    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate(&mut self, gleis_id: &GleisId<Z>, position_neu: Position) {
        let gleise: &mut GleiseInternal<Z> = &mut *self.write();
        let Gleis { definition, position } = gleise
            .map
            .get_mut(&gleis_id)
            .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
        // relocate anchor_points
        definition.execute(
            &anchor::rstar::Relocate { from: position.clone(), to: position_neu.clone() },
            &mut gleise.anchor_points,
            gleis_id.0,
        );
        // store new position
        *position = position_neu;
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn attach<T>(
        &mut self,
        definition: T,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Point,
    ) -> GleisIdLock<Z>
    where
        T: Zeichnen + Definition<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let mut gleise = self.write();
        let (gleis_id, gleis_id_lock) = gleise.next_id();
        // calculate new position
        fn winkel_mit_x_achse(direction: &anchor::Direction) -> Angle {
            let len = (direction.dx.0 * direction.dx.0 + direction.dy.0 * direction.dy.0).sqrt();
            let acos_winkel = Angle((direction.dx.0 / len).acos());
            if direction.dy < CanvasY(0.) {
                -acos_winkel
            } else {
                acos_winkel
            }
        }
        let anchor_points: T::AnchorPoints = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Angle = winkel_mit_x_achse(&anchor_point.direction)
            - winkel_mit_x_achse(&target_anchor_point.direction);
        let position = Position {
            x: target_anchor_point.position.x
                - CanvasAbstand::from(anchor_point.position.x) * winkel.cos()
                + CanvasAbstand::from(anchor_point.position.y) * winkel.sin(),
            y: target_anchor_point.position.y
                - CanvasAbstand::from(anchor_point.position.x) * winkel.sin()
                - CanvasAbstand::from(anchor_point.position.y) * winkel.cos(),
            winkel,
        };
        let gleis = Gleis { definition: definition.definition(), position: position.clone() };
        // add to anchor_points
        gleis.definition.execute(
            &anchor::rstar::Add { position },
            &mut gleise.anchor_points,
            gleis_id,
        );
        // add to HashMap
        gleise.map.insert(GleisId::new(gleis_id), gleis);
        gleis_id_lock
    }

    /// Remove the Gleis associated the the GleisId.
    ///
    /// The value contained inside GleisIdLock<Z> is set to None.
    /// Removing a value multiple times is no error.
    /// Only the first remove has an effect.
    /// Regardless, after a remove the associated Gleis is guaranteed to be removed.
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
            definition.execute(
                &anchor::rstar::Remove { position },
                &mut gleise.anchor_points,
                gleis_id.0,
            );
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", description, poisoned);
    poisoned.into_inner()
}
