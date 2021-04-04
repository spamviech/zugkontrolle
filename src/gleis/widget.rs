//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

#[cfg(feature = "gtk-rs")]
use gtk::{DrawingArea, DrawingAreaBuilder, WidgetExt};
#[cfg(feature = "gtk4-rs")]
use gtk4::{DrawingArea, DrawingAreaBuilder, DrawingAreaExt, WidgetExt};
use log::*;

use super::anchor::{self, Lookup};
use super::gerade::Gerade;
use super::kreuzung::Kreuzung;
use super::kurve::Kurve;
use super::types::*;
use super::weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche};

/// Position eines Gleises/Textes auf der Canvas
#[derive(Debug, Clone)]
pub struct Position {
    pub x: canvas::X,
    pub y: canvas::Y,
    pub winkel: Angle,
}
impl Position {
    /// anchor::Position nachdem das Objekt an die Position bewegt und um den Winkel gedreht wird.
    pub fn transformation(&self, anchor: anchor::Position) -> anchor::Position {
        let x =
            canvas::X(self.x.0 + anchor.x.0 * self.winkel.cos() - anchor.y.0 * self.winkel.sin());
        let y =
            canvas::Y(self.y.0 + anchor.x.0 * self.winkel.sin() + anchor.y.0 * self.winkel.cos());
        anchor::Position { x, y }
    }

    /// anchor::Direction nachdem das Objekt um den Winkel gedreht wird.
    pub fn rotation(&self, direction: anchor::Direction) -> anchor::Direction {
        let dx = canvas::X(0.) + direction.dx.to_abstand() * self.winkel.cos()
            - direction.dy.to_abstand() * self.winkel.sin();
        let dy = canvas::Y(0.) + direction.dx.to_abstand() * self.winkel.sin()
            - direction.dy.to_abstand() * self.winkel.cos();
        anchor::Direction { dx, dy }
    }

    /// Position damit anchor::Point übereinander mit entgegengesetzter Richtung liegen
    fn attach_position<T>(
        definition: &T,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Point,
    ) -> Self
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
    {
        let anchor_points: T::AnchorPoints = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Angle = (-anchor_point.direction).winkel_mit_x_achse()
            + target_anchor_point.direction.winkel_mit_x_achse();
        Position {
            x: target_anchor_point.position.x - anchor_point.position.x.to_abstand() * winkel.cos()
                + anchor_point.position.y.to_abstand() * winkel.sin(),
            y: target_anchor_point.position.y
                - anchor_point.position.x.to_abstand() * winkel.sin()
                - anchor_point.position.y.to_abstand() * winkel.cos(),
            winkel,
        }
    }
}

/// If GleisIdLock<Z>::read contains a Some, the GleisId<Z> is guaranteed to be valid.
#[derive(Debug)]
pub struct GleisIdLock<T>(Arc<RwLock<Option<GleisId<T>>>>);

impl<T> Clone for GleisIdLock<T> {
    fn clone(&self) -> Self {
        GleisIdLock(self.0.clone())
    }
}

impl<T: Debug> GleisIdLock<T> {
    fn new(gleis_id: u64) -> Self {
        GleisIdLock(Arc::new(RwLock::new(Some(GleisId::new(gleis_id)))))
    }

    pub fn read(&self) -> RwLockReadGuard<Option<GleisId<T>>> {
        self.0.read().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }

    fn write(&self) -> RwLockWriteGuard<Option<GleisId<T>>> {
        self.0.write().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleisId"))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Any;

/// Identifier for a Gleis.  Will probably change between restarts.
///
/// The API will only provide &GleisIdLock<Z>.
#[derive(Debug)]
pub struct GleisId<T>(u64, PhantomData<*const T>);
impl<T> GleisId<T> {
    fn new(gleis_id: u64) -> GleisId<T> {
        GleisId(gleis_id, PhantomData)
    }

    pub(crate) fn as_any(&self) -> GleisId<Any> {
        GleisId::new(self.0)
    }
}
// explicit implementation needed due to phantom type
// derived instead required corresponding Trait implemented on phantom type
impl<T> PartialEq for GleisId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Eq for GleisId<T> {}
impl<T> Hash for GleisId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(Debug, Clone)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: Position,
}

/// Anzeige aller Gleise
///
/// Achtung: Alle Methoden sind blocking!
#[derive(Debug, Clone)]
pub struct Gleise<Z>(Arc<RwLock<GleiseInternal<Z>>>);

impl<Z: Debug> Gleise<Z> {
    fn read(&self) -> RwLockReadGuard<GleiseInternal<Z>> {
        self.0.read().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleiseMap"))
    }
    fn write(&self) -> RwLockWriteGuard<GleiseInternal<Z>> {
        self.0.write().unwrap_or_else(|poisoned| warn_poison(poisoned, "GleiseMap"))
    }
}

/// Internal Representation for Gleise
///
/// This is only exported, since /GleiseMap/ needs this to be public.
#[derive(Debug)]
pub struct GleiseInternal<Z> {
    canvas: canvas::Cache,
    geraden: HashMap<GleisId<Gerade<Z>>, Gleis<Gerade<Z>>>,
    kurven: HashMap<GleisId<Kurve<Z>>, Gleis<Kurve<Z>>>,
    kreuzungen: HashMap<GleisId<Kreuzung<Z>>, Gleis<Kreuzung<Z>>>,
    weichen: HashMap<GleisId<Weiche<Z>>, Gleis<Weiche<Z>>>,
    dreiwege_weichen: HashMap<GleisId<DreiwegeWeiche<Z>>, Gleis<DreiwegeWeiche<Z>>>,
    kurven_weichen: HashMap<GleisId<KurvenWeiche<Z>>, Gleis<KurvenWeiche<Z>>>,
    s_kurven_weichen: HashMap<GleisId<SKurvenWeiche<Z>>, Gleis<SKurvenWeiche<Z>>>,
    anchor_points: anchor::rstar::RTree,
    next_id: u64,
}

pub trait GleiseMap<Z>: Sized {
    fn get_map_mut(gleise: &mut GleiseInternal<Z>) -> &mut HashMap<GleisId<Self>, Gleis<Self>>;
}
impl<Z> GleiseMap<Z> for Gerade<Z> {
    fn get_map_mut(
        GleiseInternal { geraden, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        geraden
    }
}
impl<Z> GleiseMap<Z> for Kurve<Z> {
    fn get_map_mut(
        GleiseInternal { kurven, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven
    }
}
impl<Z> GleiseMap<Z> for Weiche<Z> {
    fn get_map_mut(
        GleiseInternal { weichen, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        weichen
    }
}
impl<Z> GleiseMap<Z> for KurvenWeiche<Z> {
    fn get_map_mut(
        GleiseInternal { kurven_weichen, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for DreiwegeWeiche<Z> {
    fn get_map_mut(
        GleiseInternal { dreiwege_weichen, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        dreiwege_weichen
    }
}
impl<Z> GleiseMap<Z> for SKurvenWeiche<Z> {
    fn get_map_mut(
        GleiseInternal { s_kurven_weichen, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        s_kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for Kreuzung<Z> {
    fn get_map_mut(
        GleiseInternal { kreuzungen, .. }: &mut GleiseInternal<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kreuzungen
    }
}

impl<Z: Debug> GleiseInternal<Z> {
    fn next_id<T: Debug>(&mut self) -> (u64, GleisIdLock<T>) {
        let gleis_id: u64 = self.next_id;
        let gleis_id_lock: GleisIdLock<T> = GleisIdLock::new(gleis_id);
        // increase next id
        self.next_id += 1;
        (gleis_id, gleis_id_lock)
    }
}

fn zeichne_alle_gleise<T, F>(
    frame: &mut canvas::Frame,
    has_other_id_at_point: F,
    map: &HashMap<GleisId<T>, Gleis<T>>,
) where
    T: Zeichnen,
    F: Fn(GleisId<Any>, anchor::Position) -> bool,
{
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            // bewege Kontext zur Position
            frame.translate(canvas::Vector { dx: position.x, dy: position.y });
            // drehe Kontext um (0,0)
            frame.rotate(position.winkel);
            let path = definition.zeichne();
            // einfärben (vor Kontur zeichen, damit diese auf jeden Fall sichtbar ist)
            frame.with_save(|frame| {
                // TODO Farbe abhängig vom Streckenabschnitt
                frame.fill(
                    &path,
                    canvas::Fill {
                        color: canvas::Color { r: 1., g: 0., b: 0., a: 1. },
                        rule: canvas::FillRule::NonZero,
                    },
                );
            });
            // zeichne Gleis
            frame.with_save(|frame| {
                frame.stroke(&path, canvas::Stroke::default());
            });
            // zeichne anchor points
            frame.with_save(|frame| {
                definition.anchor_points().foreach(|&anchor| {
                    let color = if has_other_id_at_point(
                        gleis_id.as_any(),
                        position.transformation(anchor.position),
                    ) {
                        canvas::Color { r: 0., g: 1., b: 0., a: 1. }
                    } else {
                        canvas::Color { r: 0., g: 0., b: 1., a: 1. }
                    };
                    let path_builder = canvas::PathBuilder::new();
                    path_builder.move_to(anchor.position.into());
                    path_builder
                        .line_to(canvas::Point::from(anchor.position) + anchor.direction.into());
                    let path = path_builder.build();
                    frame.stroke(&path, canvas::Stroke { color, ..canvas::Stroke::default() });
                })
            });
        });
    }
}

impl<Z: Zugtyp + Debug + Eq + Clone + 'static> Gleise<Z> {
    pub fn new() -> Gleise<Z> {
        // create outwards representation
        Gleise(Arc::new(RwLock::new(GleiseInternal {
            canvas: canvas::Cache::default(),
            geraden: HashMap::new(),
            kurven: HashMap::new(),
            weichen: HashMap::new(),
            kurven_weichen: HashMap::new(),
            dreiwege_weichen: HashMap::new(),
            s_kurven_weichen: HashMap::new(),
            kreuzungen: HashMap::new(),
            anchor_points: anchor::rstar::RTree::new(),
            next_id: 0,
        })))
    }
}
impl<Z: Debug + Zugtyp> iced::canvas::Program<()> for Gleise<Z> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let GleiseInternal {
            canvas,
            geraden,
            kurven,
            weichen,
            kurven_weichen,
            dreiwege_weichen,
            s_kurven_weichen,
            kreuzungen,
            anchor_points,
            ..
        } = &*self.read();
        vec![canvas.draw(bounds.size(), |frame| {
            // TODO don't draw out of bound Gleise
            // Zeichne Gleise
            let has_other_id_at_point =
                |gleis_id, position| anchor_points.has_other_id_at_point(&gleis_id, &position);
            let boxed_frame = canvas::Frame::new(frame);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, geraden);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, kurven);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, weichen);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, kurven_weichen);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, s_kurven_weichen);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, dreiwege_weichen);
            zeichne_alle_gleise(&mut boxed_frame, has_other_id_at_point, kreuzungen);
        })]
    }
}

impl<Z: Zugtyp + Debug + Eq> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add<T>(&mut self, gleis: Gleis<T>) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let Gleis { definition, position } = &gleis;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Point { position: anchor_position, direction }| anchor::Point {
                position: position.transformation(anchor_position),
                direction: position.rotation(direction),
            },
        );
        let mut gleise = self.write();
        let (gleis_id, gleis_id_lock) = gleise.next_id();
        // increase next id
        gleise.next_id += 1;
        // add to anchor_points
        anchor_points.foreach(|anchor| {
            gleise.anchor_points.insert::<T>(&GleisId::new(gleis_id), anchor.position)
        });
        // add to HashMap
        T::get_map_mut(&mut gleise).insert(GleisId::new(gleis_id), gleis);
        // trigger redraw
        gleise.canvas.clear();
        // return value
        (gleis_id_lock, anchor_points)
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn add_attach<T>(
        &mut self,
        definition: T,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Point,
    ) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        // calculate new position
        let position = Position::attach_position(&definition, anchor_name, target_anchor_point);
        // add new gleis
        self.add(Gleis { definition, position })
    }

    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate<T>(&mut self, gleis_id: &GleisId<T>, position_neu: Position) -> T::AnchorPoints
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        // TODO find a way to return T::AnchorPoints here
        // e.g. a new phantom type for GleisId??
        let gleise: &mut GleiseInternal<Z> = &mut *self.write();
        let Gleis { definition, position } = T::get_map_mut(gleise)
            .get_mut(&gleis_id)
            .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
        // calculate absolute position for current AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Point { position: anchor_position, direction }| anchor::Point {
                position: position.transformation(anchor_position),
                direction: position.rotation(direction),
            },
        );
        // calculate absolute position for new AnchorPoints
        let anchor_points_neu = definition.anchor_points().map(
            |&anchor::Point { position: anchor_position, direction }| anchor::Point {
                position: position_neu.transformation(anchor_position),
                direction: position_neu.rotation(direction),
            },
        );
        // store new position
        *position = position_neu;
        // delete old from anchor_points
        anchor_points.foreach(|anchor| {
            gleise.anchor_points.remove(gleis_id, &anchor.position);
        });
        // add new to anchor_points
        anchor_points_neu.foreach(|anchor| gleise.anchor_points.insert(gleis_id, anchor.position));
        // trigger redraw
        gleise.canvas.clear();
        // return value
        anchor_points_neu
    }

    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub fn relocate_attach<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Point,
    ) -> T::AnchorPoints
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let position = {
            let gleise: &mut GleiseInternal<Z> = &mut *self.write();
            let Gleis { definition, .. } = T::get_map_mut(gleise)
                .get(&gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            Position::attach_position(definition, anchor_name, target_anchor_point)
        };
        // move gleis to new position
        self.relocate(gleis_id, position)
    }

    /// Remove the Gleis associated the the GleisId.
    ///
    /// The value contained inside GleisIdLock<Z> is set to None.
    /// Removing a value multiple times is no error.
    /// Only the first remove has an effect.
    /// Regardless, after a remove the associated Gleis is guaranteed to be removed.
    pub fn remove<T>(&mut self, gleis_id_lock: GleisIdLock<T>)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: Lookup<T::AnchorName>,
    {
        let mut gleise = self.write();
        let mut optional_id = gleis_id_lock.write();
        // only delete once
        if let Some(gleis_id) = optional_id.as_ref() {
            let Gleis { definition, position } = T::get_map_mut(&mut gleise)
                .remove(gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            // delete from anchor_points
            definition.anchor_points().foreach(|anchor| {
                gleise.anchor_points.remove(gleis_id, &position.transformation(anchor.position));
            });
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
        // trigger redraw
        gleise.canvas.clear();
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", description, poisoned);
    poisoned.into_inner()
}
