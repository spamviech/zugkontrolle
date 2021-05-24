//! Anzeige der GleisDefinition auf einem Canvas

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use super::anchor::{self, Lookup};
use super::typen::*;
use crate::steuerung::{streckenabschnitt, Streckenabschnitt};

#[macro_use]
pub mod id;
pub use id::*;

pub mod maps;
pub use maps::*;

#[derive(zugkontrolle_derive::Debug)]
struct Grabbed<Z> {
    gleis_id: AnyId<Z>,
    grab_location: Vektor,
}
impl<Z> Grabbed<Z> {
    fn find_clicked<T>(grabbed: &mut Option<Grabbed<Z>>, map: &mut Map<T>, canvas_pos: Vektor)
    where
        T: Zeichnen,
        (GleisId<T>, GleisIdLock<T>): Into<AnyId<Z>>,
    {
        // TODO store bounding box in rstar as well, to avoid searching everything stored?
        take_mut::take(grabbed, |grabbed| {
            grabbed.or({
                let mut grabbed = None;
                for (gleis_id, (Gleis { definition, position, .. }, gleis_id_lock)) in map.iter() {
                    let relative_pos = canvas_pos - position.punkt;
                    let rotated_pos = relative_pos.rotiert(-position.winkel);
                    if definition.innerhalb(rotated_pos) {
                        grabbed = Some(Grabbed {
                            gleis_id: AnyId::from_refs(gleis_id, gleis_id_lock),
                            grab_location: relative_pos,
                        });
                        break
                    }
                }
                grabbed
            })
        })
    }
}

// Aktueller Modus von /Gleise/
#[zugkontrolle_derive::make_enum(pub, Modus)]
#[derive(zugkontrolle_derive::Debug)]
enum ModusDaten<Z> {
    Bauen { grabbed: Option<Grabbed<Z>>, streckenabschnitt: Option<streckenabschnitt::Name> },
    // TODO Funktionalität hinzufügen
    Fahren,
}

impl<Z> ModusDaten<Z> {
    fn streckenabschnitt(&self) -> Option<streckenabschnitt::Name> {
        if let ModusDaten::Bauen { streckenabschnitt, .. } = self {
            streckenabschnitt.clone()
        } else {
            None
        }
    }
}

/// Anzeige aller Gleise.
#[derive(zugkontrolle_derive::Debug)]
pub struct Gleise<Z> {
    canvas: canvas::Cache,
    pivot: Position,
    skalieren: Skalar,
    maps: GleiseMaps<Z>,
    anchor_points: anchor::rstar::RTree,
    next_id: u64,
    last_mouse: Vektor,
    last_size: Vektor,
    modus: ModusDaten<Z>,
}

impl<Z> Gleise<Z> {
    pub fn new() -> Self {
        Gleise {
            canvas: canvas::Cache::new(),
            pivot: Position { punkt: Vektor { x: Skalar(0.), y: Skalar(0.) }, winkel: Winkel(0.) },
            skalieren: Skalar(1.),
            maps: GleiseMaps::neu(),
            anchor_points: anchor::rstar::RTree::new(),
            next_id: 0,
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::Bauen { grabbed: None, streckenabschnitt: None },
        }
    }

    fn next_id<T: Debug>(&mut self) -> (u64, GleisIdLock<T>) {
        let gleis_id: u64 = self.next_id;
        let gleis_id_lock: GleisIdLock<T> = GleisIdLock::new(gleis_id);
        // increase next id
        self.next_id += 1;
        (gleis_id, gleis_id_lock)
    }

    fn relocate_grabbed<T: Debug + Zeichnen>(&mut self, gleis_id: GleisId<T>, punkt: Vektor)
    where
        Z: Zugtyp,
        T: GleiseMap<Z>,
    {
        let (Gleis { position, .. }, _id_lock) =
            T::get_map_mut(&mut self.maps).get(&gleis_id).expect("grabbed a non-existing gleis");
        let position_neu = Position { punkt, winkel: position.winkel };
        self.relocate(&gleis_id, position_neu);
    }

    fn snap_to_anchor<T: Debug + Zeichnen>(&mut self, gleis_id: GleisId<T>)
    where
        Z: Zugtyp,
        T: GleiseMap<Z>,
    {
        let (Gleis { definition, position, .. }, _id_lock) =
            T::get_map_mut(&mut self.maps).get(&gleis_id).expect("failed to lookup grabbed Gleis");
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
                snap = self
                    .anchor_points
                    .get_other_id_at_point(gleis_id.as_any(), anchor)
                    .map(|snap_anchor| (anchor_name, snap_anchor))
            }
        });
        if let Some((snap_name, snap_anchor)) = snap {
            self.relocate_attach(&gleis_id, snap_name, snap_anchor);
        };
    }

    /// Aktueller Modus.
    pub fn modus(&self) -> Modus {
        match &self.modus {
            ModusDaten::Bauen { .. } => Modus::Bauen,
            ModusDaten::Fahren => Modus::Fahren,
        }
    }

    /// Wechsel den aktuellen Modus zu /modus/.
    pub fn moduswechsel(&mut self, modus: Modus) {
        self.modus = match modus {
            Modus::Bauen => ModusDaten::Bauen { grabbed: None, streckenabschnitt: None },
            Modus::Fahren => ModusDaten::Fahren,
        };
    }

    /// Aktuelle Pivot-Punkt und Dreh-Winkel
    pub fn pivot(&self) -> &Position {
        &self.pivot
    }

    /// Bewege aktuellen Pivot-Punkt um /bewegung/.
    pub fn bewege_pivot(&mut self, bewegung: Vektor) {
        self.pivot.punkt += bewegung;
        self.canvas.clear();
    }

    /// Drehe die aktuelle Darstellung um /winkel/.
    pub fn drehen(&mut self, winkel: Winkel) {
        self.pivot.winkel += winkel;
        self.canvas.clear();
    }

    /// Aktueller Skalierfaktor zur Darstellung.
    pub fn skalierfaktor(&self) -> Skalar {
        self.skalieren
    }

    /// Skaliere die aktuelle Darstellung mit /skalieren/.
    pub fn skalieren(&mut self, skalieren: Skalar) {
        self.skalieren *= skalieren;
        self.canvas.clear();
    }
}

pub(crate) fn move_to_position(frame: &mut canvas::Frame, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
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
    map: &Map<T>,
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
    streckenabschnitte: &streckenabschnitt::Map,
) {
    for (gleis_id, (Gleis { definition, position, streckenabschnitt }, _id_lock)) in map.iter() {
        // TODO Farbe abhängig vom Streckenabschnitt
        if let Some(Streckenabschnitt { farbe, .. }) =
            streckenabschnitt.as_ref().map(|name| streckenabschnitte.get(name)).flatten()
        {
            frame.with_save(|frame| {
                move_to_position(frame, position);
                // einfärben
                for path in definition.fülle() {
                    frame.with_save(|frame| {
                        let mut color = *farbe;
                        color.a *= transparency(gleis_id, &is_grabbed);
                        frame.fill(&path, canvas::Fill { color, rule: canvas::FillRule::EvenOdd });
                    });
                }
            })
        }
    }
}

fn zeichne_alle_gleise<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, (Gleis { definition, position, .. }, _id_lock)) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    frame.stroke(&path, canvas::Stroke {
                        color: canvas::Color {
                            a: transparency(gleis_id, &is_grabbed),
                            ..canvas::Color::BLACK
                        },
                        width: 1.5,
                        ..Default::default()
                    });
                });
            }
        })
    }
}

fn zeichne_alle_anchor_points<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    has_other_and_grabbed_id_at_point: impl Fn(GleisId<Any>, anchor::Anchor) -> (bool, bool),
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, (Gleis { definition, position, .. }, _id_lock)) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
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
                    let direction_side: Vektor = Skalar(0.5) * direction.rotiert(winkel::FRAC_PI_2);
                    let anchor_position: Vektor = anchor.position;
                    let mut path_builder = pfad::Erbauer::neu();
                    path_builder.move_to(anchor_position + direction_side);
                    path_builder.line_to(anchor_position + direction);
                    path_builder.line_to(anchor_position - direction_side);
                    let path = path_builder.baue();
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
    map: &Map<T>,
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, (Gleis { definition, position, .. }, _id_lock)) in map.iter() {
        if let Some((relative_position, content)) = definition.beschreibung() {
            let punkt =
                position.punkt + Vektor::from(relative_position.punkt).rotiert(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = Position { punkt, winkel };
            frame.with_save(|frame| {
                move_to_position(frame, &absolute_position);
                frame.fill_text(canvas::Text {
                    content: content.to_string(),
                    position: iced::Point::ORIGIN,
                    color: canvas::Color {
                        a: transparency(gleis_id, &is_grabbed),
                        ..canvas::Color::BLACK
                    },
                    horizontal_alignment: canvas::HorizontalAlignment::Center,
                    vertical_alignment: canvas::VerticalAlignment::Center,
                    ..Default::default()
                });
            })
        }
    }
}

fn get_canvas_position(
    bounds: &iced::Rectangle,
    cursor: &iced::canvas::Cursor,
    pivot: &Position,
    skalieren: &Skalar,
) -> Option<Vektor> {
    // position_in only returns a Some-value if it is in-bounds
    // position doesn't have this restriction, so use it
    // and explicitly substract bounds-start instead
    cursor.position().map(|pos| {
        pivot.punkt
            + (Vektor { x: Skalar(pos.x - bounds.x), y: Skalar(pos.y - bounds.y) } / skalieren)
                .rotiert(-pivot.winkel)
    })
}

fn grab_gleis_an_position<Z: Zugtyp>(
    bounds: &iced::Rectangle,
    cursor: &iced::canvas::Cursor,
    modus: &mut ModusDaten<Z>,
    GleiseMaps {
        geraden,
        kurven,
        weichen,
        kurven_weichen,
        dreiwege_weichen,
        s_kurven_weichen,
        kreuzungen,
        ..
    }: &mut GleiseMaps<Z>,
    pivot: &Position,
    skalieren: &Skalar,
) -> iced::canvas::event::Status {
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = get_canvas_position(&bounds, &cursor, pivot, skalieren) {
            if let ModusDaten::Bauen { grabbed, .. } = modus {
                Grabbed::find_clicked(grabbed, geraden, canvas_pos);
                Grabbed::find_clicked(grabbed, kurven, canvas_pos);
                Grabbed::find_clicked(grabbed, weichen, canvas_pos);
                Grabbed::find_clicked(grabbed, dreiwege_weichen, canvas_pos);
                Grabbed::find_clicked(grabbed, kurven_weichen, canvas_pos);
                Grabbed::find_clicked(grabbed, s_kurven_weichen, canvas_pos);
                Grabbed::find_clicked(grabbed, kreuzungen, canvas_pos);
            }
        }
    }
    if let ModusDaten::Bauen { grabbed: None, .. } = modus {
        iced::canvas::event::Status::Ignored
    } else {
        iced::canvas::event::Status::Captured
    }
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
                    streckenabschnitte,
                },
            anchor_points,
            modus,
            ..
        } = self;
        vec![canvas.draw_skaliert_von_pivot(bounds.size(), &self.pivot, &self.skalieren, |frame| {
            // TODO don't draw out of bound Gleise
            // Zeichne Gleise
            let grabbed_id =
                if let ModusDaten::Bauen { grabbed: Some(Grabbed { gleis_id, .. }), .. } = modus {
                    Some(gleis_id.id_as_any())
                } else {
                    None
                };
            // TODO markiere grabbed als "wird-gelöscht", falls cursor out of bounds ist
            let is_grabbed = |parameter_id| Some(parameter_id) == grabbed_id;
            let has_other_and_grabbed_id_at_point = |gleis_id, position| {
                anchor_points.has_other_and_grabbed_id_at_point(
                    &gleis_id,
                    |id| is_grabbed(id.as_any()),
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
            mit_allen_gleisen!(fülle_alle_gleise, is_grabbed, streckenabschnitte);
            // Kontur
            mit_allen_gleisen!(zeichne_alle_gleise, is_grabbed);
            // AnchorPoints
            mit_allen_gleisen!(
                zeichne_alle_anchor_points,
                has_other_and_grabbed_id_at_point,
                &is_grabbed
            );
            // Beschreibung
            mit_allen_gleisen!(schreibe_alle_beschreibungen, is_grabbed);
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
            )) => {
                let Gleise { modus, maps, pivot, skalieren, .. } = self;
                grab_gleis_an_position(&bounds, &cursor, modus, maps, pivot, skalieren)
            },
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) => {
                let mut event_status = iced::canvas::event::Status::Ignored;
                if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
                    if let Some(Grabbed { gleis_id, .. }) = grabbed {
                        let gleis_id_clone = gleis_id.clone();
                        *grabbed = None;
                        if cursor.is_over(&bounds) {
                            with_any_id!(gleis_id_clone, Gleise::snap_to_anchor, self);
                        } else {
                            with_any_id_and_lock!(gleis_id_clone, Gleise::remove_grabbed, self);
                        }
                        event_status = iced::canvas::event::Status::Captured;
                    }
                }
                event_status
            },
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position: _ }) => {
                let mut event_status = iced::canvas::event::Status::Ignored;
                if let Some(canvas_pos) =
                    get_canvas_position(&bounds, &cursor, &self.pivot, &self.skalieren)
                {
                    self.last_mouse = canvas_pos;
                    if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
                        if let Some(Grabbed { gleis_id, grab_location }) = &*grabbed {
                            let point = canvas_pos - grab_location;
                            with_any_id!(gleis_id.clone(), Gleise::relocate_grabbed, self, point);
                            event_status = iced::canvas::event::Status::Captured
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
                x: target_anchor_point.position.x - anchor_point.position.x * winkel.cos()
                    + anchor_point.position.y * winkel.sin(),
                y: target_anchor_point.position.y
                    - anchor_point.position.x * winkel.sin()
                    - anchor_point.position.y * winkel.cos(),
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
        let Gleis { definition, position, .. } = &gleis;
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
        T::get_map_mut(&mut self.maps)
            .insert(GleisId::new(gleis_id), (gleis, gleis_id_lock.clone()));
        // trigger redraw
        self.canvas.clear();
        // return value
        (gleis_id_lock, anchor_points)
    }

    /// Add a gleis at the last known mouse position
    /// capped at the last known canvas size.
    pub(crate) fn add_grabbed_at_mouse<T>(
        &mut self,
        definition: T,
        grab_location: Vektor,
    ) -> (GleisIdLock<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        (GleisId<T>, GleisIdLock<T>): Into<AnyId<Z>>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let mut canvas_position = self.last_mouse;
        let ex = Vektor { x: Skalar(1.), y: Skalar(0.) }.rotiert(-self.pivot.winkel);
        let cp_x = canvas_position.skalarprodukt(&ex);
        if cp_x < Skalar(0.) {
            canvas_position -= cp_x * ex;
        } else if cp_x > self.last_size.x {
            canvas_position -= (cp_x - self.last_size.x) * ex;
        }
        let ey = Vektor { x: Skalar(0.), y: Skalar(1.) }.rotiert(-self.pivot.winkel);
        let cp_y = canvas_position.skalarprodukt(&ey);
        if cp_y < Skalar(0.) {
            canvas_position -= cp_y * ey;
        } else if cp_y > self.last_size.y {
            canvas_position -= (cp_y - self.last_size.y) * ey;
        }
        let result = self.add(Gleis {
            definition,
            position: Position {
                punkt: canvas_position - grab_location,
                winkel: -self.pivot.winkel,
            },
            streckenabschnitt: self.modus.streckenabschnitt(),
        });
        if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
            let gleis_id_lock = &result.0;
            if let Some(gleis_id) = gleis_id_lock
                .read()
                .as_ref()
                .map(|gleis_id| AnyId::from_refs(gleis_id, gleis_id_lock))
            {
                *grabbed = Some(Grabbed { gleis_id, grab_location });
            }
        }
        result
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn add_attach<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
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
        self.add(Gleis { definition, position, streckenabschnitt })
    }

    /// Move an existing gleis to the new position.
    ///
    /// This is called relocate instead of move since the latter is a reserved keyword.
    pub fn relocate<T>(&mut self, gleis_id: &GleisId<T>, position_neu: Position) -> T::AnchorPoints
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let (Gleis { definition, position, .. }, _id_lock) = T::get_map_mut(&mut self.maps)
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
            let (Gleis { definition, .. }, _id_lock) = T::get_map_mut(&mut self.maps)
                .get(&gleis_id)
                .expect(&format!("Gleis {:?} nicht mehr in HashMap", gleis_id));
            Position::attach_position(definition, anchor_name, target_anchor_point)
        };
        // move gleis to new position
        self.relocate(gleis_id, position)
    }

    /// wrapper um Gleise::remove mit ignoriertem Argument für with_any_id_and_lock-Macro
    #[inline]
    fn remove_grabbed<T, A>(&mut self, _ignored: A, gleis_id_lock: GleisIdLock<T>)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: Lookup<T::AnchorName>,
    {
        self.remove(gleis_id_lock)
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
            let (Gleis { definition, position, .. }, _id_lock) = T::get_map_mut(&mut self.maps)
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

impl<Z: Zugtyp + Serialize> Gleise<Z> {
    pub fn speichern(&self, pfad: impl AsRef<std::path::Path>) -> std::result::Result<(), Error> {
        let Gleise { maps, .. } = self;
        let vecs: GleiseVecs<Z> = maps.into();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &vecs)?;
        Ok(())
    }
}

impl<Z: Zugtyp + PartialEq + std::fmt::Debug + for<'de> Deserialize<'de>> Gleise<Z> {
    pub fn laden(&mut self, pfad: impl AsRef<std::path::Path>) -> std::result::Result<(), Error> {
        let file = std::fs::File::open(pfad)?;
        let GleiseVecs {
            name,
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        } = bincode::deserialize_from(file)?;

        if name != Z::NAME {
            return Err(Error::FalscherZugtyp(name))
        }

        // reset current state
        self.canvas.clear();
        // TODO pivot, skalieren?
        self.pivot = Position { punkt: Vektor::null_vektor(), winkel: winkel::ZERO };
        self.skalieren = Skalar::multiplikativ_neutral();
        self.maps = GleiseMaps::neu();
        self.anchor_points = anchor::rstar::RTree::new();
        self.next_id = 0;
        // don't reset last_mouse, last_size
        // TODO Modus?

        // restore state from data
        macro_rules! add_gleise {
            ($($gleise: ident,)*) => {
                $(
                    for gleis in $gleise {
                        self.add(gleis);
                    }
                );*
            }
        }
        add_gleise!(
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen,
        );

        Ok(())
    }
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Bincode(bincode::Error),
    FalscherZugtyp(String),
}
impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IO(error)
    }
}
impl From<bincode::Error> for Error {
    fn from(error: bincode::Error) -> Self {
        Error::Bincode(error)
    }
}
