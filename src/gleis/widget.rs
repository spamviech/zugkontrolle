//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use log::*;

use super::anchor::{self, Lookup};
use super::gerade::Gerade;
use super::kreuzung::Kreuzung;
use super::kurve::Kurve;
use super::types::*;
use super::weiche::{DreiwegeWeiche, KurvenWeiche, SKurvenWeiche, Weiche};

/// If GleisIdLock<Z>::read contains a Some, the GleisId<Z> is guaranteed to be valid.
#[derive(zugkontrolle_derive::Clone, zugkontrolle_derive::Debug)]
pub struct GleisIdLock<T>(Arc<RwLock<Option<GleisId<T>>>>);

impl<T> GleisIdLock<T> {
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
#[derive(zugkontrolle_derive::Debug)]
pub struct GleisId<T>(u64, PhantomData<*const T>);
impl<T> GleisId<T> {
    fn new(gleis_id: u64) -> GleisId<T> {
        GleisId(gleis_id, PhantomData)
    }

    fn as_any(&self) -> GleisId<Any> {
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
    pub position: canvas::Position,
}

/// Anzeige aller Gleise.
#[derive(zugkontrolle_derive::Debug)]
pub struct Gleise<Z> {
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

impl<Z> Gleise<Z> {
    pub fn new() -> Self {
        Gleise {
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
        }
    }
    fn next_id<T: Debug>(&mut self) -> (u64, GleisIdLock<T>) {
        let gleis_id: u64 = self.next_id;
        let gleis_id_lock: GleisIdLock<T> = GleisIdLock::new(gleis_id);
        // increase next id
        self.next_id += 1;
        (gleis_id, gleis_id_lock)
    }
}

pub trait GleiseMap<Z>: Sized {
    fn get_map_mut(gleise: &mut Gleise<Z>) -> &mut HashMap<GleisId<Self>, Gleis<Self>>;
}
impl<Z> GleiseMap<Z> for Gerade<Z> {
    fn get_map_mut(
        Gleise { geraden, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        geraden
    }
}
impl<Z> GleiseMap<Z> for Kurve<Z> {
    fn get_map_mut(
        Gleise { kurven, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven
    }
}
impl<Z> GleiseMap<Z> for Weiche<Z> {
    fn get_map_mut(
        Gleise { weichen, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        weichen
    }
}
impl<Z> GleiseMap<Z> for KurvenWeiche<Z> {
    fn get_map_mut(
        Gleise { kurven_weichen, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for DreiwegeWeiche<Z> {
    fn get_map_mut(
        Gleise { dreiwege_weichen, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        dreiwege_weichen
    }
}
impl<Z> GleiseMap<Z> for SKurvenWeiche<Z> {
    fn get_map_mut(
        Gleise { s_kurven_weichen, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        s_kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for Kreuzung<Z> {
    fn get_map_mut(
        Gleise { kreuzungen, .. }: &mut Gleise<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kreuzungen
    }
}

fn move_to_position(frame: &mut canvas::Frame, position: &canvas::Position) {
    // bewege Kontext zur Position
    frame.transformation(&canvas::Transformation::Translate(position.point.into()));
    // drehe Kontext um (0,0)
    frame.transformation(&canvas::Transformation::Rotate(position.winkel));
}
fn fuelle_alle_gleise<T: Zeichnen>(frame: &mut canvas::Frame, map: &HashMap<GleisId<T>, Gleis<T>>) {
    for (_gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // einfärben
            for path in definition.fuelle() {
                frame.with_save(|frame| {
                    // TODO Farbe abhängig vom Streckenabschnitt
                    frame.fill(
                        &path,
                        canvas::Fill {
                            color: canvas::Color { r: 1., g: 0., b: 0., a: 1. },
                            rule: canvas::FillRule::EvenOdd,
                        },
                    );
                });
            }
        })
    }
}
fn zeichne_alle_gleise<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &HashMap<GleisId<T>, Gleis<T>>,
) {
    for (_gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    frame.stroke(
                        &path,
                        canvas::Stroke {
                            color: canvas::Color::BLACK,
                            width: 1.5,
                            ..Default::default()
                        },
                    );
                });
            }
        })
    }
}
fn zeichne_alle_anchor_points<T: Zeichnen>(
    frame: &mut canvas::Frame,
    has_other_id_at_point: impl Fn(GleisId<Any>, anchor::Anchor) -> bool,
    map: &HashMap<GleisId<T>, Gleis<T>>,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.anchor_points().foreach(|&anchor| {
                frame.with_save(|frame| {
                    let color = if has_other_id_at_point(
                        gleis_id.as_any(),
                        anchor::Anchor {
                            position: position.transformation(anchor.position),
                            direction: position.rotation(anchor.direction),
                        },
                    ) {
                        canvas::Color::from_rgb(0., 1., 0.)
                    } else {
                        canvas::Color::from_rgb(0., 0., 1.)
                    };
                    let direction: canvas::Vector = anchor.direction.into();
                    let direction_side: canvas::Vector = direction.rotate(AngleDegrees::new(90.));
                    let anchor_position: canvas::Point = anchor.position.into();
                    let scale: f32 = canvas::X(5.).to_abstand() / direction.length::<canvas::X>();
                    let mut path_builder = canvas::PathBuilder::new();
                    path_builder.move_to(anchor_position + 0.5 * scale * direction_side);
                    path_builder.line_to(anchor_position + scale * direction);
                    path_builder.line_to(anchor_position - 0.5 * scale * direction_side);
                    let path = path_builder.build();
                    // TODO fill on connect/snap for drag&drop
                    frame.stroke(
                        &path,
                        canvas::Stroke { color, width: 1.5, ..canvas::Stroke::default() },
                    );
                });
            });
        })
    }
}
fn schreibe_alle_beschreibungen<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &HashMap<GleisId<T>, Gleis<T>>,
) {
    for (_gleis_id, Gleis { definition, position }) in map.iter() {
        if let Some((relative_position, content)) = definition.beschreibung() {
            let point = position.point
                + canvas::Vector::from(relative_position.point).rotate(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = canvas::Position { point, winkel };
            frame.with_save(|frame| {
                move_to_position(frame, &absolute_position);
                frame.fill_text(canvas::Text {
                    content: content.to_string(),
                    position: iced::Point::ORIGIN,
                    color: canvas::Color::BLACK,
                    horizontal_alignment: canvas::HorizontalAlignment::Center,
                    vertical_alignment: canvas::VerticalAlignment::Center,
                    ..Default::default()
                });
            })
        }
    }
}
impl<Z: Zugtyp, T> iced::canvas::Program<T> for Gleise<Z> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let Gleise {
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
        } = self;
        vec![canvas.draw(bounds.size(), |frame| {
            // TODO don't draw out of bound Gleise
            // Zeichne Gleise
            let has_other_id_at_point = |gleis_id, position| {
                anchor_points.has_other_id_at_point(&gleis_id, &position).is_some()
            };
            let mut boxed_frame = canvas::Frame::new(frame);
            // Hintergrund
            fuelle_alle_gleise(&mut boxed_frame, geraden);
            fuelle_alle_gleise(&mut boxed_frame, kurven);
            fuelle_alle_gleise(&mut boxed_frame, weichen);
            fuelle_alle_gleise(&mut boxed_frame, kurven_weichen);
            fuelle_alle_gleise(&mut boxed_frame, s_kurven_weichen);
            fuelle_alle_gleise(&mut boxed_frame, dreiwege_weichen);
            fuelle_alle_gleise(&mut boxed_frame, kreuzungen);
            // Kontur
            zeichne_alle_gleise(&mut boxed_frame, geraden);
            zeichne_alle_gleise(&mut boxed_frame, kurven);
            zeichne_alle_gleise(&mut boxed_frame, weichen);
            zeichne_alle_gleise(&mut boxed_frame, kurven_weichen);
            zeichne_alle_gleise(&mut boxed_frame, s_kurven_weichen);
            zeichne_alle_gleise(&mut boxed_frame, dreiwege_weichen);
            zeichne_alle_gleise(&mut boxed_frame, kreuzungen);
            // AnchorPoints
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, geraden);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, kurven);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, weichen);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, kurven_weichen);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, s_kurven_weichen);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, dreiwege_weichen);
            zeichne_alle_anchor_points(&mut boxed_frame, has_other_id_at_point, kreuzungen);
            // Beschreibung
            schreibe_alle_beschreibungen(&mut boxed_frame, geraden);
            schreibe_alle_beschreibungen(&mut boxed_frame, kurven);
            schreibe_alle_beschreibungen(&mut boxed_frame, weichen);
            schreibe_alle_beschreibungen(&mut boxed_frame, kurven_weichen);
            schreibe_alle_beschreibungen(&mut boxed_frame, s_kurven_weichen);
            schreibe_alle_beschreibungen(&mut boxed_frame, dreiwege_weichen);
            schreibe_alle_beschreibungen(&mut boxed_frame, kreuzungen);
        })]
    }
}

impl canvas::Position {
    /// Position damit anchor::Anchor übereinander mit entgegengesetzter Richtung liegen
    fn attach_position<T>(
        definition: &T,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> Self
    where
        T: Zeichnen,
        T::AnchorPoints: Lookup<T::AnchorName>,
    {
        let anchor_points: T::AnchorPoints = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Angle = (-anchor_point.direction).winkel_mit_x_achse()
            + target_anchor_point.direction.winkel_mit_x_achse();
        canvas::Position {
            point: canvas::Point {
                x: target_anchor_point.position.x
                    - anchor_point.position.x.to_abstand() * winkel.cos()
                    + anchor_point.position.y.to_abstand().as_x() * winkel.sin(),
                y: target_anchor_point.position.y
                    - anchor_point.position.x.to_abstand().as_y() * winkel.sin()
                    - anchor_point.position.y.to_abstand() * winkel.cos(),
            },
            winkel,
        }
    }
}

impl<Z: Zugtyp> Gleise<Z> {
    /// Add a new gleis to its position.
    pub fn add<T>(&mut self, gleis: Gleis<T>) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let Gleis { definition, position } = &gleis;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, direction }| anchor::Anchor {
                position: position.transformation(anchor_position),
                direction: position.rotation(direction),
            },
        );
        let (gleis_id, gleis_id_lock) = self.next_id();
        // increase next id
        self.next_id += 1;
        // add to anchor_points
        anchor_points
            .foreach(|anchor| self.anchor_points.insert(GleisId::new(gleis_id), anchor.clone()));
        // add to HashMap
        T::get_map_mut(self).insert(GleisId::new(gleis_id), gleis);
        // trigger redraw
        self.canvas.clear();
        // return value
        (gleis_id_lock, anchor_points)
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn add_attach<T>(
        &mut self,
        definition: T,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        // calculate new position
        let position =
            canvas::Position::attach_position(&definition, anchor_name, target_anchor_point);
        // add new gleis
        self.add(Gleis { definition, position })
    }

    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        position_neu: canvas::Position,
    ) -> T::AnchorPoints
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let Gleis { definition, position } = T::get_map_mut(self)
            .get_mut(&gleis_id)
            .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
        // calculate absolute position for current AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, direction }| anchor::Anchor {
                position: position.transformation(anchor_position),
                direction: position.rotation(direction),
            },
        );
        // calculate absolute position for new AnchorPoints
        let anchor_points_neu = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, direction }| anchor::Anchor {
                position: position_neu.transformation(anchor_position),
                direction: position_neu.rotation(direction),
            },
        );
        // store new position
        *position = position_neu;
        // delete old from anchor_points
        anchor_points.foreach(|anchor| {
            self.anchor_points.remove(gleis_id.as_any(), &anchor);
        });
        // add new to anchor_points
        anchor_points_neu
            .foreach(|anchor| self.anchor_points.insert(gleis_id.as_any(), anchor.clone()));
        // trigger redraw
        self.canvas.clear();
        // return value
        anchor_points_neu
    }

    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub fn relocate_attach<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        anchor_name: T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> T::AnchorPoints
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let position = {
            let Gleis { definition, .. } = T::get_map_mut(self)
                .get(&gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            canvas::Position::attach_position(definition, anchor_name, target_anchor_point)
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
        let mut optional_id = gleis_id_lock.write();
        // only delete once
        if let Some(gleis_id) = optional_id.as_ref() {
            let Gleis { definition, position } = T::get_map_mut(self)
                .remove(gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            // delete from anchor_points
            definition.anchor_points().foreach(|anchor| {
                self.anchor_points.remove(
                    gleis_id.as_any(),
                    &anchor::Anchor {
                        position: position.transformation(anchor.position),
                        direction: position.rotation(anchor.direction),
                    },
                );
            });
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
        // trigger redraw
        self.canvas.clear();
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, description: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", description, poisoned);
    poisoned.into_inner()
}
