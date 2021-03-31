//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

#[cfg(feature = "gtk4-rs")]
use glib::{Cast, IsA};
#[cfg(feature = "gtk-rs")]
use gtk::{ContainerExt, DrawingArea, DrawingAreaBuilder, PanedExt, WidgetExt};
#[cfg(feature = "gtk4-rs")]
use gtk4::ScrolledWindow;
#[cfg(feature = "gtk4-rs")]
use gtk4::{DrawingArea, DrawingAreaBuilder, DrawingAreaExt, Paned, WidgetExt};
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

    /// anchor::Direction nachdem das Objekt um den Winkel gedreht wird.
    pub fn rotation(&self, direction: anchor::Direction) -> anchor::Direction {
        let dx = CanvasX(0.) + CanvasAbstand::from(direction.dx) * self.winkel.cos()
            - CanvasAbstand::from(direction.dy) * self.winkel.sin();
        let dy = CanvasY(0.) + CanvasAbstand::from(direction.dx) * self.winkel.sin()
            - CanvasAbstand::from(direction.dy) * self.winkel.cos();
        anchor::Direction { dx, dy }
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
    drawing_area: DrawingArea,
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

impl<Z: Zugtyp + Debug + Eq + Clone + 'static> Gleise<Z> {
    pub fn new() -> Gleise<Z> {
        // TODO is this a good default size?
        Gleise::new_with_size(CanvasX(600.), CanvasY(400.))
    }

    /// convenience function that automatically calls set_size_request on the newly created widget
    pub fn new_with_size(width: CanvasX, height: CanvasY) -> Self {
        let drawing_area = DrawingAreaBuilder::new()
            .width_request(width.0 as i32)
            .height_request(height.0 as i32)
            .build();
        // create outwards representation
        let gleise = Gleise(Arc::new(RwLock::new(GleiseInternal {
            drawing_area,
            geraden: HashMap::new(),
            kurven: HashMap::new(),
            weichen: HashMap::new(),
            kurven_weichen: HashMap::new(),
            dreiwege_weichen: HashMap::new(),
            s_kurven_weichen: HashMap::new(),
            kreuzungen: HashMap::new(),
            anchor_points: anchor::rstar::RTree::new(),
            next_id: 0,
        })));
        // connect draw callback
        let gleise_clone = gleise.clone();
        #[cfg(feature = "gtk-rs")]
        let zeichne_gleise_mit_anchor_points =
            move |drawing_area: &DrawingArea, c: &cairo::Context| {
                // TODO don't draw out of bound Gleise
                let _allocation = drawing_area.get_allocation();
                let cairo: &Cairo = &Cairo::new(c);
                // Zeichne Gleise
                let GleiseInternal { drawing_area: _, map, anchor_points, next_id: _ } =
                    &*gleise_clone.read();
                for (gleis_id, Gleis { definition, position }) in map.iter() {
                    cairo.with_save_restore(|cairo| {
                        // bewege Kontext zur Position
                        cairo.translate(position.x, position.y);
                        // drehe Kontext um die Mitte
                        let width = CanvasX(0.5 * (definition.verwende(&Size::Width) as f64));
                        let height = CanvasY(0.5 * (definition.verwende(&Size::Height) as f64));
                        cairo.translate(width, height);
                        cairo.rotate(position.winkel);
                        cairo.translate(-width, -height);
                        // zeichne Gleis
                        cairo.with_save_restore(|cairo| {
                            definition.verwende(&Zeichne(cairo));
                            cairo.stroke();
                        });
                        // zeichne anchor points
                        cairo.with_save_restore(|cairo| {
                            definition.verwende_anchor_points(
                                &ZeichneAnchorPoints { cairo, position },
                                anchor_points,
                                gleis_id,
                            );
                        });
                    });
                }
                glib::signal::Inhibit(false)
            };
        #[cfg(feature = "gtk-rs")]
        gleise.read().drawing_area.connect_draw(zeichne_gleise_mit_anchor_points);
        #[cfg(feature = "gtk4-rs")]
        let zeichne_gleise_mit_anchor_points =
            move |drawing_area: &DrawingArea, c: &cairo::Context, _width: i32, _height: i32| {
                // TODO don't draw out of bound Gleise
                let _allocation = drawing_area.get_allocation();
                let cairo: &Cairo = &Cairo::new(c);
                // Zeichne Gleise
                let GleiseInternal {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                    anchor_points,
                    ..
                } = &*gleise_clone.read();
                fn zeichne_alle_gleise<T, F: Fn(&GleisId<T>, &anchor::Position) -> bool>(
                    cairo: &Cairo,
                    has_other_id_at_point: F,
                    map: &HashMap<GleisId<T>, Gleis<T>>,
                ) where
                    T: Zeichnen,
                    T::AnchorPoints: Lookup<T::AnchorName>,
                {
                    for (gleis_id, Gleis { definition, position }) in map.iter() {
                        cairo.with_save_restore(|cairo| {
                            // bewege Kontext zur Position
                            cairo.translate(position.x, position.y);
                            // drehe Kontext um (0,0)
                            cairo.rotate(position.winkel);
                            // zeichne Gleis
                            cairo.with_save_restore(|cairo| {
                                definition.zeichne(cairo);
                                cairo.stroke();
                            });
                            // zeichne anchor points
                            cairo.with_save_restore(|cairo| {
                                definition.anchor_points().foreach(
                                    |&anchor::Point {
                                         position: anchor_position,
                                         direction: anchor::Direction { dx, dy },
                                     }| {
                                        let (r, g, b) = if has_other_id_at_point(
                                            gleis_id,
                                            &position.transformation(anchor_position),
                                        ) {
                                            (0., 1., 0.)
                                        } else {
                                            (0., 0., 1.)
                                        };
                                        cairo.set_source_rgb(r, g, b);
                                        let anchor::Position { x, y } = anchor_position;
                                        cairo.move_to(x, y);
                                        cairo.line_to(
                                            x + 5. * CanvasAbstand::from(dx),
                                            y + 5. * CanvasAbstand::from(dy),
                                        );
                                        cairo.stroke();
                                    },
                                )
                            });
                        });
                    }
                }
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    geraden,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    kurven,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    weichen,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    kurven_weichen,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    s_kurven_weichen,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    dreiwege_weichen,
                );
                zeichne_alle_gleise(
                    cairo,
                    |gleis_id, position| {
                        anchor_points.has_other_id_at_point(&gleis_id.as_any(), position)
                    },
                    kreuzungen,
                );
            };
        #[cfg(feature = "gtk4-rs")]
        gleise.read().drawing_area.set_draw_func(zeichne_gleise_mit_anchor_points);
        // return
        gleise
    }

    pub fn set_size_request(&mut self, width: CanvasX, height: CanvasY) {
        self.write().drawing_area.set_size_request(width.0 as i32, height.0 as i32);
    }

    /// TODO Placeholder, bis mir eine bessere methode einfällt
    #[cfg(feature = "gtk-rs")]
    pub fn add_to_container<C: ContainerExt>(&self, container: &C) {
        container.add(&self.read().drawing_area)
    }
    #[cfg(feature = "gtk4-rs")]
    pub fn add_to_scrolled_window<C: IsA<ScrolledWindow>>(&self, scrolled_window: &C) {
        scrolled_window.upcast_ref().set_child(Some(&self.read().drawing_area))
    }

    /// TODO Placeholder, bis mir eine bessere methode einfällt
    #[cfg(feature = "gtk-rs")]
    pub fn add_to_paned1<P: PanedExt>(&self, paned: &P) {
        paned.add1(&self.read().drawing_area)
    }
    #[cfg(feature = "gtk4-rs")]
    pub fn add_to_paned1<P: IsA<Paned>>(&self, paned: &P) {
        paned.upcast_ref().set_start_child(&self.read().drawing_area)
    }

    /// TODO Placeholder, bis mir eine bessere methode einfällt
    #[cfg(feature = "gtk-rs")]
    pub fn add_to_paned2<P: PanedExt>(&self, paned: &P) {
        paned.add2(&self.read().drawing_area)
    }
    #[cfg(feature = "gtk4-rs")]
    pub fn add_to_paned2<P: IsA<Paned>>(&self, paned: &P) {
        paned.upcast_ref().set_end_child(&self.read().drawing_area)
    }
}

impl<Z: Zugtyp + Debug + Eq> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add<T>(&mut self, definition: T, position: Position) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Point { position: anchor_position, direction }| anchor::Point {
                position: position.transformation(anchor_position),
                direction: position.rotation(direction),
            },
        );
        // create gleis
        let gleis = Gleis { definition, position };
        let mut gleise = self.write();
        let (gleis_id, gleis_id_lock) = gleise.next_id();
        // increase next id
        gleise.next_id += 1;
        // add to anchor_points
        anchor_points.foreach(|anchor| {
            gleise
                .anchor_points
                .insert(anchor::rstar::PointWithData::new(GleisId::new(gleis_id), anchor.position))
        });
        // add to HashMap
        T::get_map_mut(&mut gleise).insert(GleisId::new(gleis_id), gleis);
        // trigger redraw
        gleise.drawing_area.queue_draw();
        // return value
        (gleis_id_lock, anchor_points)
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
            gleise
                .anchor_points
                .remove(&anchor::rstar::PointWithData::new(gleis_id.as_any(), anchor.position));
        });
        // add new to anchor_points
        anchor_points_neu.foreach(|anchor| {
            gleise
                .anchor_points
                .insert(anchor::rstar::PointWithData::new(gleis_id.as_any(), anchor.position))
        });
        // trigger redraw
        gleise.drawing_area.queue_draw();
        // return value
        anchor_points_neu
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn attach<T>(
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
        fn winkel_mit_x_achse(direction: &anchor::Direction) -> Angle {
            let len = (direction.dx.0 * direction.dx.0 + direction.dy.0 * direction.dy.0).sqrt();
            let acos_winkel = Angle((direction.dx.0 / len).acos());
            if direction.dy < CanvasY(0.) {
                acos_winkel
            } else {
                -acos_winkel
            }
        }
        let anchor_points: T::AnchorPoints = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Angle = winkel_mit_x_achse(&-anchor_point.direction)
            + winkel_mit_x_achse(&target_anchor_point.direction);
        let position = Position {
            x: target_anchor_point.position.x
                - CanvasAbstand::from(anchor_point.position.x) * winkel.cos()
                + CanvasAbstand::from(anchor_point.position.y) * winkel.sin(),
            y: target_anchor_point.position.y
                - CanvasAbstand::from(anchor_point.position.x) * winkel.sin()
                - CanvasAbstand::from(anchor_point.position.y) * winkel.cos(),
            winkel,
        };
        // add new gleis
        self.add(definition, position)
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
                gleise.anchor_points.remove(&anchor::rstar::PointWithData::new(
                    gleis_id.as_any(),
                    position.transformation(anchor.position),
                ));
            });
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
        // trigger redraw
        gleise.drawing_area.queue_draw();
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", description, poisoned);
    poisoned.into_inner()
}
