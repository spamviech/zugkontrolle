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
use super::typen::*;
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
    pub position: Position,
}

#[derive(zugkontrolle_derive::Debug)]
struct Grabbed<Z> {
    gleis_id: AnyId<Z>,
    grab_location: Vektor,
}
#[derive(zugkontrolle_derive::Debug)]
pub(crate) enum AnyId<Z> {
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

macro_rules! impl_any_id_from {
    ($type:ident) => {
        impl<Z> From<GleisId<$type<Z>>> for AnyId<Z> {
            fn from(input: GleisId<$type<Z>>) -> Self {
                AnyId::$type(input)
            }
        }
    };
}
impl_any_id_from! {Gerade}
impl_any_id_from! {Kurve}
impl_any_id_from! {Weiche}
impl_any_id_from! {DreiwegeWeiche}
impl_any_id_from! {KurvenWeiche}
impl_any_id_from! {SKurvenWeiche}
impl_any_id_from! {Kreuzung}

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

// Aktuelle Modus von /Gleise/
#[derive(zugkontrolle_derive::Debug)]
enum Modus<Z> {
    Bauen {
        grabbed: Option<Grabbed<Z>>,
    },
    // TODO
    #[allow(dead_code)]
    Fahren,
}
/// Anzeige aller Gleise.
#[derive(zugkontrolle_derive::Debug)]
pub struct Gleise<Z> {
    canvas: canvas::Cache,
    // TODO actually use pivot and scale
    pivot: Position,
    skalieren: Skalar,
    maps: GleiseMaps<Z>,
    anchor_points: anchor::rstar::RTree,
    next_id: u64,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: Modus<Z>,
}

impl<Z> Gleise<Z> {
    pub fn new() -> Self {
        Gleise {
            canvas: canvas::Cache::new(),
            pivot: Position { punkt: Vektor { x: Skalar(0.), y: Skalar(0.) }, winkel: Winkel(0.) },
            skalieren: Skalar(1.),
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
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: Modus::Bauen { grabbed: None },
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

pub(crate) fn move_to_position(
    frame: &mut canvas::Frame,
    position: &Position,
    zu_iced_vektor: impl Fn(Vektor) -> iced::Vector,
) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt), &zu_iced_vektor);
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel), zu_iced_vektor);
}
fn transparency<T>(gleis_id: &GleisId<T>, is_grabbed: &impl Fn(GleisId<Any>) -> bool) -> f32 {
    if is_grabbed(gleis_id.as_any()) {
        0.5
    } else {
        1.
    }
}
fn fülle_alle_gleise<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &HashMap<GleisId<T>, Gleis<T>>,
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
    zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + Clone + 'static,
    zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + Clone + 'static,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position, zu_iced_vektor.clone());
            // einfärben
            for path in definition.fülle(zu_iced_vektor.clone(), zu_iced_bogen.clone()) {
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
                        zu_iced_vektor.clone(),
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
    zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + Clone + 'static,
    zu_iced_bogen: impl Fn(Bogen) -> iced::canvas::path::Arc + Clone + 'static,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position, zu_iced_vektor.clone());
            // zeichne Kontur
            for path in definition.zeichne(zu_iced_vektor.clone(), zu_iced_bogen.clone()) {
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
                        zu_iced_vektor.clone(),
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
    zu_iced_vektor: impl Fn(Vektor) -> iced::Vector + 'static,
) {
    for (gleis_id, Gleis { definition, position }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position, &zu_iced_vektor);
            // zeichne anchor points
            definition.anchor_points().foreach(|_name, &anchor| {
                frame.with_save(|frame| {
                    let (opposing, grabbed) =
                        has_other_and_grabbed_id_at_point(gleis_id.as_any(), anchor::Anchor {
                            position: position.transformation(anchor.position),
                            richtung: position.winkel + anchor.richtung,
                        });
                    let color = if opposing {
                        canvas::Color::from_rgba(0., 1., 0., transparency(gleis_id, &is_grabbed))
                    } else {
                        canvas::Color::from_rgba(0., 0., 1., transparency(gleis_id, &is_grabbed))
                    };
                    let direction: Vektor = Vektor::polar_koordinaten(Skalar(5.), anchor.richtung);
                    let direction_side: Vektor = Skalar(0.5) * direction.rotiere(winkel::FRAC_PI_2);
                    let anchor_position: Vektor = anchor.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(anchor_position + direction_side, &zu_iced_vektor);
                    path_builder.line_to(anchor_position + direction, &zu_iced_vektor);
                    path_builder.line_to(anchor_position - direction_side, &zu_iced_vektor);
                    let path = path_builder.baue();
                    frame.stroke(
                        &path,
                        canvas::Stroke { color, width: 1.5, ..Default::default() },
                        &zu_iced_vektor,
                    );
                    // fill on connect/snap for drag&drop
                    if grabbed {
                        frame.fill(
                            &path,
                            canvas::Fill { color, ..Default::default() },
                            &zu_iced_vektor,
                        );
                    }
                });
            });
        })
    }
}
fn schreibe_alle_beschreibungen<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &HashMap<GleisId<T>, Gleis<T>>,
    zu_iced_vektor: impl Fn(Vektor) -> iced::Vector,
) {
    for (_gleis_id, Gleis { definition, position }) in map.iter() {
        if let Some((relative_position, content)) = definition.beschreibung() {
            let punkt =
                position.punkt + Vektor::from(relative_position.punkt).rotiere(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = Position { punkt, winkel };
            frame.with_save(|frame| {
                move_to_position(frame, &absolute_position, &zu_iced_vektor);
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
    punkt: Vektor,
) {
    let Gleis { position, .. } =
        T::get_map_mut(&mut gleise.maps).get(&gleis_id).expect("grabbed a non-existing gleis");
    let position_neu = Position { punkt, winkel: position.winkel };
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
        |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
            position: position.transformation(anchor_position),
            richtung: position.winkel + richtung,
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
            modus,
            ..
        } = self;
        vec![canvas.draw(bounds.size(), |frame| {
            // TODO don't draw out of bound Gleise
            // Zeichne Gleise
            let grabbed_id =
                if let Modus::Bauen { grabbed: Some(Grabbed { gleis_id, .. }), .. } = modus {
                    Some(gleis_id.id_as_any())
                } else {
                    None
                };
            let is_grabbed = |parameter_id| Some(parameter_id) == grabbed_id;
            let has_other_and_grabbed_id_at_point = |gleis_id, position| {
                anchor_points.has_other_and_grabbed_id_at_point(
                    &gleis_id,
                    |id| is_grabbed(id.clone()),
                    &position,
                )
            };

            let pivot_clone = self.pivot.clone();
            let skalieren_clone = self.skalieren.clone();
            let zu_iced_vektor =
                move |vektor| Vektor::zu_iced(vektor, pivot_clone.clone(), skalieren_clone.clone());
            let pivot_clone = self.pivot.clone();
            let skalieren_clone = self.skalieren.clone();
            let zu_iced_bogen =
                move |bogen| Bogen::zu_iced(bogen, pivot_clone.clone(), skalieren_clone.clone());
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
            mit_allen_gleisen!(
                fülle_alle_gleise,
                is_grabbed,
                zu_iced_vektor.clone(),
                zu_iced_bogen.clone()
            );
            // Kontur
            mit_allen_gleisen!(
                zeichne_alle_gleise,
                is_grabbed,
                zu_iced_vektor.clone(),
                zu_iced_bogen.clone()
            );
            // AnchorPoints
            mit_allen_gleisen!(
                zeichne_alle_anchor_points,
                has_other_and_grabbed_id_at_point,
                &is_grabbed,
                zu_iced_vektor.clone()
            );
            // Beschreibung
            mit_allen_gleisen!(schreibe_alle_beschreibungen, zu_iced_vektor.clone());
        })]
    }

    fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Message>) {
        self.last_size = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
        let event_status = match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) if cursor.is_over(&bounds) => {
                // TODO store bounding box in rtree as well, to avoid searching everything stored?
                if let Some(in_pos) = cursor.position_in(&bounds) {
                    let Gleise { maps, modus, .. } = self;
                    let canvas_pos = Vektor { x: Skalar(in_pos.x), y: Skalar(in_pos.y) };
                    macro_rules! find_clicked {
                        ($map:expr,AnyId:: $konstruktor:ident) => {
                            if let Modus::Bauen { grabbed, .. } = modus {
                                take_mut::take(grabbed, |grabbed| {
                                    grabbed.or_else(|| {
                                        for (gleis_id, Gleis { definition, position }) in
                                            $map.iter()
                                        {
                                            let relative_pos = canvas_pos - position.punkt;
                                            let rotated_pos =
                                                relative_pos.rotiere(-position.winkel);
                                            if definition.innerhalb(rotated_pos) {
                                                return Some(Grabbed {
                                                    gleis_id: AnyId::$konstruktor(gleis_id.clone()),
                                                    grab_location: relative_pos,
                                                })
                                            }
                                        }
                                        None
                                    })
                                })
                            }
                        };
                    }
                    find_clicked!(maps.geraden, AnyId::Gerade);
                    find_clicked!(maps.kurven, AnyId::Kurve);
                    find_clicked!(maps.weichen, AnyId::Weiche);
                    find_clicked!(maps.dreiwege_weichen, AnyId::DreiwegeWeiche);
                    find_clicked!(maps.kurven_weichen, AnyId::KurvenWeiche);
                    find_clicked!(maps.s_kurven_weichen, AnyId::SKurvenWeiche);
                    find_clicked!(maps.kreuzungen, AnyId::Kreuzung);
                }
                if let Modus::Bauen { grabbed: None, .. } = self.modus {
                    iced::canvas::event::Status::Ignored
                } else {
                    iced::canvas::event::Status::Captured
                }
            },
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) => {
                if let Modus::Bauen { grabbed: Some(Grabbed { gleis_id, .. }) } = &self.modus {
                    with_any_id!(gleis_id, snap_to_anchor, self);
                    self.modus = Modus::Bauen { grabbed: None };
                    iced::canvas::event::Status::Captured
                } else {
                    iced::canvas::event::Status::Ignored
                }
            },
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position: _ }) => {
                if let Some(pos) = cursor.position() {
                    // position_in only returns a Some-value if it is in-bounds
                    // make the calculation explicitly instead
                    self.last_mouse =
                        Vektor { x: Skalar(pos.x - bounds.x), y: Skalar(pos.y - bounds.y) };
                }
                let mut event_status = iced::canvas::event::Status::Ignored;
                if let Modus::Bauen { grabbed } = &mut self.modus {
                    if let Some(in_pos) = cursor.position_in(&bounds) {
                        if cursor.is_over(&bounds) {
                            if let Some(Grabbed { gleis_id, grab_location }) = &*grabbed {
                                let point = Vektor { x: Skalar(in_pos.x), y: Skalar(in_pos.y) }
                                    - grab_location;
                                with_any_id!(gleis_id, relocate_grabbed, self, point);
                                event_status = iced::canvas::event::Status::Captured
                            }
                        }
                    }
                }
                event_status
            },
            _otherwise => iced::canvas::event::Status::Ignored,
        };
        if event_status == iced::canvas::event::Status::Captured {
            self.canvas.clear()
        }
        (event_status, None)
    }
}

impl Position {
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
        let anchor_points = definition.anchor_points();
        let anchor_point = anchor_points.get(anchor_name);
        let winkel: Winkel = winkel::PI - anchor_point.richtung + target_anchor_point.richtung;
        Position {
            punkt: Vektor {
                x: target_anchor_point.position.x - anchor_point.position.x * Skalar(winkel.cos())
                    + anchor_point.position.y * Skalar(winkel.sin()),
                y: target_anchor_point.position.y
                    - anchor_point.position.x * Skalar(winkel.sin())
                    - anchor_point.position.y * Skalar(winkel.cos()),
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
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
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

    /// Add a gleis at the last known mouse position
    /// capped at the last known canvas size.
    // FIXME not grabbed, since buttons only react on mouse button press+release
    // maybe simulate some drag&drop by making the list part of the canvas?
    // alternatively make drag&drop a toggle, so we can stay consistent with buttons behaviour
    // starting it?
    pub(crate) fn add_at_mouse<T>(&mut self, definition: T) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        GleisId<T>: Into<AnyId<Z>>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let mut canvas_position = self.last_mouse;
        canvas_position.x = canvas_position.x.max(&Skalar(0.)).min(&self.last_size.x);
        canvas_position.y = canvas_position.y.max(&Skalar(0.)).min(&self.last_size.y);
        self.add(Gleis {
            definition,
            position: Position {
                punkt: self.pivot.punkt + canvas_position,
                winkel: self.pivot.winkel,
            },
        })
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
        let Gleis { definition, position } = T::get_map_mut(&mut self.maps)
            .get_mut(&gleis_id)
            .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
        // calculate absolute position for current AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        // calculate absolute position for new AnchorPoints
        let anchor_points_neu = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position_neu.transformation(anchor_position),
                richtung: position_neu.winkel + richtung,
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
        let mut optional_id = gleis_id_lock.write();
        // only delete once
        if let Some(gleis_id) = optional_id.as_ref() {
            let Gleis { definition, position } = T::get_map_mut(&mut self.maps)
                .remove(gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            // delete from anchor_points
            definition.anchor_points().foreach(|_name, anchor| {
                self.anchor_points.remove(gleis_id.as_any(), &anchor::Anchor {
                    position: position.transformation(anchor.position),
                    richtung: position.winkel + anchor.richtung,
                });
            });
        }
        // make sure everyone knows about the deletion
        *optional_id = None;
        // trigger redraw
        self.canvas.clear();
    }
}

fn warn_poison<T: Debug>(poisoned: PoisonError<T>, beschreibung: &str) -> T {
    warn!("Poisoned {} RwLock: {:?}! Trying to continue anyway.", beschreibung, poisoned);
    poisoned.into_inner()
}
