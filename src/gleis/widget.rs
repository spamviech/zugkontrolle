//! Anzeige der GleisDefinition auf einem Canvas

use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard};

use log::*;
use serde::{Deserialize, Serialize};

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
#[derive(zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct GleisId<T>(u64, PhantomData<*const T>);
impl<T> GleisId<T> {
    pub fn new(gleis_id: u64) -> Self {
        GleisId(gleis_id, PhantomData)
    }

    pub(crate) fn as_any(&self) -> GleisId<Any> {
        GleisId::new(self.0)
    }

    // implemented as method, so it stays private
    fn clone(&self) -> Self {
        GleisId(self.0, self.1)
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Gleis<T> {
    pub definition: T,
    pub position: canvas::Position,
}

#[derive(zugkontrolle_derive::Debug)]
struct Grabbed<Z> {
    gleis_id: AnyId<Z>,
    grab_location: canvas::Vector,
}
enum AnyId<Z> {
    Gerade(GleisId<Gerade<Z>>),
    Kurve(GleisId<Kurve<Z>>),
    Weiche(GleisId<Weiche<Z>>),
    DreiwegeWeiche(GleisId<DreiwegeWeiche<Z>>),
    KurvenWeiche(GleisId<KurvenWeiche<Z>>),
    SKurvenWeiche(GleisId<SKurvenWeiche<Z>>),
    Kreuzung(GleisId<Kreuzung<Z>>),
}
macro_rules! maybe_clone {
    ($x:expr => @no_clone) => {
        $x
    };
    ($x:expr $(=> @clone)?) => {
        $x.clone()
    };
}
macro_rules! with_any_id {
    ($any_id: expr $(=> @$clone: tt)?, $function: expr$(, $($extra_arg:expr),+)?) => {
        match $any_id {
            AnyId::Gerade(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::Kurve(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::Weiche(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::DreiwegeWeiche(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::KurvenWeiche(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::SKurvenWeiche(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
            AnyId::Kreuzung(gleis_id) => {
                let gleis_id_clone = maybe_clone!(gleis_id$(=> @$clone)?);
                $function(gleis_id_clone$(, $($extra_arg),+)?)
            }
        }
    };
}
impl<Z> AnyId<Z> {
    fn id_as_any(&self) -> GleisId<Any> {
        with_any_id!(self => @no_clone, GleisId::as_any)
    }
}

impl<Z> std::fmt::Debug for AnyId<Z> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Grabbed::")?;
        match self {
            AnyId::Gerade(gleis_id) => {
                write!(f, "Gerade({:?})", gleis_id)
            }
            AnyId::Kurve(gleis_id) => {
                write!(f, "Kurve({:?})", gleis_id)
            }
            AnyId::Weiche(gleis_id) => {
                write!(f, "Weiche({:?})", gleis_id)
            }
            AnyId::DreiwegeWeiche(gleis_id) => {
                write!(f, "DreiwegeWeiche({:?})", gleis_id)
            }
            AnyId::KurvenWeiche(gleis_id) => {
                write!(f, "KurvenWeiche({:?})", gleis_id)
            }
            AnyId::SKurvenWeiche(gleis_id) => {
                write!(f, "SKurvenWeiche({:?})", gleis_id)
            }
            AnyId::Kreuzung(gleis_id) => {
                write!(f, "Kreuzung({:?})", gleis_id)
            }
        }
    }
}

// TODO Konvertierungsfunktion von/zu Gleise<Z>
#[derive(zugkontrolle_derive::Debug, Serialize, Deserialize)]
pub struct GleiseVecs<Z> {
    geraden: Vec<Gleis<Gerade<Z>>>,
    kurven: Vec<Gleis<Kurve<Z>>>,
    kreuzungen: Vec<Gleis<Kreuzung<Z>>>,
    weichen: Vec<Gleis<Weiche<Z>>>,
    dreiwege_weichen: Vec<Gleis<DreiwegeWeiche<Z>>>,
    kurven_weichen: Vec<Gleis<KurvenWeiche<Z>>>,
    s_kurven_weichen: Vec<Gleis<SKurvenWeiche<Z>>>,
}

#[derive(zugkontrolle_derive::Debug)]
pub struct GleiseMaps<Z> {
    geraden: HashMap<GleisId<Gerade<Z>>, Gleis<Gerade<Z>>>,
    kurven: HashMap<GleisId<Kurve<Z>>, Gleis<Kurve<Z>>>,
    kreuzungen: HashMap<GleisId<Kreuzung<Z>>, Gleis<Kreuzung<Z>>>,
    weichen: HashMap<GleisId<Weiche<Z>>, Gleis<Weiche<Z>>>,
    dreiwege_weichen: HashMap<GleisId<DreiwegeWeiche<Z>>, Gleis<DreiwegeWeiche<Z>>>,
    kurven_weichen: HashMap<GleisId<KurvenWeiche<Z>>, Gleis<KurvenWeiche<Z>>>,
    s_kurven_weichen: HashMap<GleisId<SKurvenWeiche<Z>>, Gleis<SKurvenWeiche<Z>>>,
}

pub trait GleiseMap<Z>: Sized {
    fn get_map_mut(gleise: &mut GleiseMaps<Z>) -> &mut HashMap<GleisId<Self>, Gleis<Self>>;
}
impl<Z> GleiseMap<Z> for Gerade<Z> {
    fn get_map_mut(
        GleiseMaps { geraden, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        geraden
    }
}
impl<Z> GleiseMap<Z> for Kurve<Z> {
    fn get_map_mut(
        GleiseMaps { kurven, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven
    }
}
impl<Z> GleiseMap<Z> for Weiche<Z> {
    fn get_map_mut(
        GleiseMaps { weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        weichen
    }
}
impl<Z> GleiseMap<Z> for KurvenWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { kurven_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for DreiwegeWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { dreiwege_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        dreiwege_weichen
    }
}
impl<Z> GleiseMap<Z> for SKurvenWeiche<Z> {
    fn get_map_mut(
        GleiseMaps { s_kurven_weichen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        s_kurven_weichen
    }
}
impl<Z> GleiseMap<Z> for Kreuzung<Z> {
    fn get_map_mut(
        GleiseMaps { kreuzungen, .. }: &mut GleiseMaps<Z>,
    ) -> &mut HashMap<GleisId<Self>, Gleis<Self>> {
        kreuzungen
    }
}

/// Anzeige aller Gleise.
#[derive(zugkontrolle_derive::Debug)]
pub struct Gleise<Z> {
    canvas: canvas::Cache,
    maps: GleiseMaps<Z>,
    anchor_points: anchor::rstar::RTree,
    next_id: u64,
    // TODO move into Mode enum
    // Bauen, Fahren
    grabbed: Option<Grabbed<Z>>,
}

impl<Z> Gleise<Z> {
    pub fn new() -> Self {
        Gleise {
            canvas: canvas::Cache::new(),
            maps: GleiseMaps {
                geraden: HashMap::new(),
                kurven: HashMap::new(),
                weichen: HashMap::new(),
                kurven_weichen: HashMap::new(),
                dreiwege_weichen: HashMap::new(),
                s_kurven_weichen: HashMap::new(),
                kreuzungen: HashMap::new(),
            },
            anchor_points: anchor::rstar::RTree::new(),
            next_id: 0,
            grabbed: None,
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

fn move_to_position(frame: &mut canvas::Frame, position: &canvas::Position) {
    // bewege Kontext zur Position
    frame.transformation(&canvas::Transformation::Translate(position.point.into()));
    // drehe Kontext um (0,0)
    frame.transformation(&canvas::Transformation::Rotate(position.winkel));
}
fn transparency<T>(gleis_id: &GleisId<T>, is_grabbed: &impl Fn(GleisId<Any>) -> bool) -> f32 {
    if is_grabbed(gleis_id.as_any()) {
        0.5
    } else {
        1.
    }
}
fn fuelle_alle_gleise<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &HashMap<GleisId<T>, Gleis<T>>,
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // einfärben
            for path in definition.fuelle() {
                frame.with_save(|frame| {
                    // TODO Farbe abhängig vom Streckenabschnitt
                    frame.fill(
                        &path,
                        canvas::Fill {
                            color: canvas::Color {
                                r: 1.,
                                g: 0.,
                                b: 0.,
                                a: transparency(gleis_id, &is_grabbed),
                            },
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
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    frame.stroke(
                        &path,
                        canvas::Stroke {
                            color: canvas::Color {
                                a: transparency(gleis_id, &is_grabbed),
                                ..canvas::Color::BLACK
                            },
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
    map: &HashMap<GleisId<T>, Gleis<T>>,
    has_other_and_grabbed_id_at_point: impl Fn(GleisId<Any>, anchor::Anchor) -> (bool, bool),
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.anchor_points().foreach(|_name, &anchor| {
                frame.with_save(|frame| {
                    let (opposing, grabbed) = has_other_and_grabbed_id_at_point(
                        gleis_id.as_any(),
                        anchor::Anchor {
                            position: position.transformation(anchor.position),
                            direction: position.rotation(anchor.direction),
                        },
                    );
                    let color = if opposing {
                        canvas::Color::from_rgba(0., 1., 0., transparency(gleis_id, &is_grabbed))
                    } else {
                        canvas::Color::from_rgba(0., 0., 1., transparency(gleis_id, &is_grabbed))
                    };
                    let direction: canvas::Vector = anchor.direction.into();
                    let direction_side: canvas::Vector = direction.rotate(AngleDegrees::new(90.));
                    let anchor_position: canvas::Point = anchor.position.into();
                    let scale: f32 = canvas::X(5.).to_abstand() / direction.length_x();
                    let mut path_builder = canvas::PathBuilder::new();
                    path_builder.move_to(anchor_position + 0.5 * scale * direction_side);
                    path_builder.line_to(anchor_position + scale * direction);
                    path_builder.line_to(anchor_position - 0.5 * scale * direction_side);
                    let path = path_builder.build();
                    frame.stroke(&path, canvas::Stroke { color, width: 1.5, ..Default::default() });
                    // fill on connect/snap for drag&drop
                    if grabbed {
                        frame.fill(&path, canvas::Fill { color, ..Default::default() });
                    }
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
fn relocate_grabbed<Z: Zugtyp, T: Debug + Zeichnen + GleiseMap<Z>>(
    gleis_id: GleisId<T>,
    gleise: &mut Gleise<Z>,
    point: canvas::Point,
) {
    let Gleis { position, .. } =
        T::get_map_mut(&mut gleise.maps).get(&gleis_id).expect("grabbed a non-existing gleis");
    let position_neu = canvas::Position { point, winkel: position.winkel };
    gleise.relocate(&gleis_id, position_neu);
}
fn snap_to_anchor<Z: Zugtyp, T: Debug + Zeichnen + GleiseMap<Z>>(
    gleis_id: GleisId<T>,
    gleise: &mut Gleise<Z>,
) {
    let Gleis { definition, position } =
        T::get_map_mut(&mut gleise.maps).get(&gleis_id).expect("failed to lookup grabbed Gleis");
    // calculate absolute position for AnchorPoints
    let anchor_points = definition.anchor_points().map(
        |&anchor::Anchor { position: anchor_position, direction }| anchor::Anchor {
            position: position.transformation(anchor_position),
            direction: position.rotation(direction),
        },
    );
    let mut snap = None;
    anchor_points.foreach(|anchor_name, anchor| {
        if snap.is_none() {
            snap = gleise
                .anchor_points
                .get_other_id_at_point(gleis_id.as_any(), anchor)
                .map(|snap_anchor| (anchor_name, snap_anchor))
        }
    });
    if let Some((snap_name, snap_anchor)) = snap {
        gleise.relocate_attach(&gleis_id, snap_name, snap_anchor);
    };
}
impl<Z: Zugtyp, Message> iced::canvas::Program<Message> for Gleise<Z> {
    fn draw(
        &self,
        bounds: iced::Rectangle,
        _cursor: iced::canvas::Cursor,
    ) -> Vec<iced::canvas::Geometry> {
        let Gleise {
            canvas,
            maps:
                GleiseMaps {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                },
            anchor_points,
            grabbed,
            ..
        } = self;
        vec![canvas.draw(
            canvas::Size::new(
                canvas::X(bounds.width).to_abstand(),
                canvas::Y(bounds.height).to_abstand(),
            ),
            |frame| {
                // TODO don't draw out of bound Gleise
                // Zeichne Gleise
                let is_grabbed = |parameter_id| {
                    if let Some(Grabbed { gleis_id, .. }) = grabbed {
                        parameter_id == gleis_id.id_as_any()
                    } else {
                        false
                    }
                };
                let has_other_and_grabbed_id_at_point = |gleis_id, position| {
                    anchor_points.has_other_and_grabbed_id_at_point(
                        &gleis_id,
                        |id| is_grabbed(id.clone()),
                        &position,
                    )
                };
                macro_rules! mit_allen_gleisen {
                    ($funktion:expr$(, $($extra_args:expr),+)?) => {
                        $funktion(frame, geraden$(, $($extra_args),+)?);
                        $funktion(frame, kurven$(, $($extra_args),+)?);
                        $funktion(frame, weichen$(, $($extra_args),+)?);
                        $funktion(frame, kurven_weichen$(, $($extra_args),+)?);
                        $funktion(frame, s_kurven_weichen$(, $($extra_args),+)?);
                        $funktion(frame, dreiwege_weichen$(, $($extra_args),+)?);
                        $funktion(frame, kreuzungen$(, $($extra_args),+)?);
                    };
                }
                // Hintergrund
                mit_allen_gleisen!(fuelle_alle_gleise, is_grabbed);
                // Kontur
                mit_allen_gleisen!(zeichne_alle_gleise, is_grabbed);
                // AnchorPoints
                mit_allen_gleisen!(
                    zeichne_alle_anchor_points,
                    has_other_and_grabbed_id_at_point,
                    &is_grabbed
                );
                // Beschreibung
                mit_allen_gleisen!(schreibe_alle_beschreibungen);
            },
        )]
    }

    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Message>) {
        let event_status = match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) if cursor.is_over(&bounds) => {
                // TODO store bounding box in rtree as well, to avoid searching everything stored?
                if let Some(in_pos) = cursor.position_in(&bounds) {
                    let canvas_pos = canvas::Point::new(canvas::X(in_pos.x), canvas::Y(in_pos.y));
                    macro_rules! find_clicked {
                        ($map:expr, AnyId::$konstruktor:ident) => {
                            if self.grabbed.is_none() {
                                for (gleis_id, Gleis { definition, position }) in $map.iter() {
                                    let relative_pos = canvas::Vector::from(
                                        canvas_pos - canvas::Vector::from(position.point),
                                    );
                                    let rotated_pos = relative_pos.rotate(-position.winkel);
                                    if definition.innerhalb(rotated_pos) {
                                        self.grabbed = Some(Grabbed {
                                            gleis_id: AnyId::$konstruktor(gleis_id.clone()),
                                            grab_location: relative_pos,
                                        });
                                        break;
                                    }
                                }
                            }
                        };
                    }
                    find_clicked!(self.maps.geraden, AnyId::Gerade);
                    find_clicked!(self.maps.kurven, AnyId::Kurve);
                    find_clicked!(self.maps.weichen, AnyId::Weiche);
                    find_clicked!(self.maps.dreiwege_weichen, AnyId::DreiwegeWeiche);
                    find_clicked!(self.maps.kurven_weichen, AnyId::KurvenWeiche);
                    find_clicked!(self.maps.s_kurven_weichen, AnyId::SKurvenWeiche);
                    find_clicked!(self.maps.kreuzungen, AnyId::Kreuzung);
                }
                if self.grabbed.is_some() {
                    iced::canvas::event::Status::Captured
                } else {
                    iced::canvas::event::Status::Ignored
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) => {
                if let Some(Grabbed { gleis_id, .. }) = &self.grabbed {
                    with_any_id!(gleis_id, snap_to_anchor, self);
                    self.grabbed = None;
                    iced::canvas::event::Status::Captured
                } else {
                    iced::canvas::event::Status::Ignored
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position: _ })
                if cursor.is_over(&bounds) =>
            {
                if let Some(Grabbed { gleis_id, grab_location }) = &self.grabbed {
                    if let Some(in_pos) = cursor.position_in(&bounds) {
                        let point = canvas::Point::new(canvas::X(in_pos.x), canvas::Y(in_pos.y))
                            - grab_location;
                        with_any_id!(gleis_id, relocate_grabbed, self, point);
                    }
                }
                iced::canvas::event::Status::Captured
            }
            _otherwise => iced::canvas::event::Status::Ignored,
        };
        if event_status == iced::canvas::event::Status::Captured {
            self.canvas.clear()
        }
        (event_status, None)
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
        let winkel: Angle = anchor_point.direction.winkel_mit_x_achse()
            - (-target_anchor_point.direction).winkel_mit_x_achse();
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
        anchor_points.foreach(|_name, anchor| {
            self.anchor_points.insert(GleisId::new(gleis_id), anchor.clone())
        });
        // add to HashMap
        T::get_map_mut(&mut self.maps).insert(GleisId::new(gleis_id), gleis);
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
        let Gleis { definition, position } = T::get_map_mut(&mut self.maps)
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
        anchor_points.foreach(|_name, anchor| {
            self.anchor_points.remove(gleis_id.as_any(), &anchor);
        });
        // add new to anchor_points
        anchor_points_neu
            .foreach(|_name, anchor| self.anchor_points.insert(gleis_id.as_any(), anchor.clone()));
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
            let Gleis { definition, .. } = T::get_map_mut(&mut self.maps)
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
            let Gleis { definition, position } = T::get_map_mut(&mut self.maps)
                .remove(gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            // delete from anchor_points
            definition.anchor_points().foreach(|_name, anchor| {
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
