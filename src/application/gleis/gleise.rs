//! Anzeige der GleisDefinition auf einem Canvas

use std::fmt::Debug;
use std::time::{Duration, Instant};

use log::error;
use serde::{Deserialize, Serialize};

use self::id::with_any_id;
use crate::{
    anschluss::{self, Anschlüsse, Fließend, Reserviere, ToSave},
    application::{anchor, typen::*},
    farbe::Farbe,
    lookup::Lookup,
    steuerung::{geschwindigkeit, streckenabschnitt, weiche, Streckenabschnitt},
};

pub mod id;
pub use id::*;

pub mod maps;
pub use maps::*;

#[derive(zugkontrolle_derive::Debug)]
struct Grabbed<Z> {
    gleis_id: AnyId<Z>,
    grab_location: Vektor,
    moved: bool,
}

// Aktueller Modus von /Gleise/
#[zugkontrolle_derive::make_enum(pub, Modus)]
#[derive(zugkontrolle_derive::Debug)]
enum ModusDaten<Z> {
    Bauen { grabbed: Option<Grabbed<Z>>, last: Instant },
    Fahren,
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
    pub fn neu() -> Self {
        Gleise {
            canvas: canvas::Cache::new(),
            pivot: Position { punkt: Vektor { x: Skalar(0.), y: Skalar(0.) }, winkel: Winkel(0.) },
            skalieren: Skalar(1.),
            maps: GleiseMaps::neu(),
            anchor_points: anchor::rstar::RTree::new(),
            next_id: 0,
            last_mouse: Vektor::null_vektor(),
            last_size: Vektor::null_vektor(),
            modus: ModusDaten::Bauen { grabbed: None, last: Instant::now() },
        }
    }

    pub(in crate::application) fn erzwinge_neuzeichnen(&mut self) {
        self.canvas.clear()
    }

    fn next_id<T: Debug>(&mut self) -> GleisId<T> {
        let gleis_id: u64 = self.next_id;
        // increase next id
        self.next_id += 1;
        GleisId::new(gleis_id)
    }

    fn relocate_grabbed<T: Debug + Zeichnen>(
        &mut self,
        gleis_id: GleisId<T>,
        punkt: Vektor,
    ) -> Result<(), GleisEntferntError>
    where
        Z: Zugtyp,
        T: GleiseMap<Z>,
    {
        let Gleis { position, .. } =
            T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
        let position_neu = Position { punkt, winkel: position.winkel };
        self.relocate(&gleis_id, position_neu)?;
        Ok(())
    }

    fn snap_to_anchor<T: Debug + Zeichnen>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<(), GleisEntferntError>
    where
        Z: Zugtyp,
        T: GleiseMap<Z>,
    {
        let Gleis { definition, position, .. } =
            T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
        // calculate absolute position for AnchorPoints
        let anchor_points = definition.anchor_points().map(
            |&anchor::Anchor { position: anchor_position, richtung }| anchor::Anchor {
                position: position.transformation(anchor_position),
                richtung: position.winkel + richtung,
            },
        );
        let mut snap = None;
        anchor_points.for_each(|anchor_name, anchor| {
            if snap.is_none() {
                snap = self
                    .anchor_points
                    .get_other_id_at_point(gleis_id.as_any(), anchor)
                    .map(|snap_anchor| (anchor_name, snap_anchor))
            }
        });
        if let Some((snap_name, snap_anchor)) = snap {
            self.relocate_attach(&gleis_id, &snap_name, snap_anchor)?;
        };
        Ok(())
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
            Modus::Bauen => ModusDaten::Bauen { grabbed: None, last: Instant::now() },
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

    /// Füge einen Streckenabschnitt hinzu.
    /// Ein vorher gespeicherter Streckenabschnitt mit identischem Namen wird zurückgegeben.
    pub fn neuer_streckenabschnitt(
        &mut self,
        name: streckenabschnitt::Name,
        streckenabschnitt: Streckenabschnitt,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.insert(name, (streckenabschnitt, Fließend::Gesperrt))
    }

    /// Erhalte eine Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt(
        &self,
        name: &streckenabschnitt::Name,
    ) -> Option<&(Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.get(name)
    }

    /// Erhalte eine veränderliche Referenz auf einen Streckenabschnitt (falls vorhanden).
    pub fn streckenabschnitt_mut<'s, 't>(
        &'s mut self,
        name: &'t streckenabschnitt::Name,
    ) -> Option<&'s mut (Streckenabschnitt, Fließend)> {
        self.maps.streckenabschnitte.get_mut(name)
    }

    /// Entferne einen Streckenabschnitt.
    /// Falls er vorhanden war wird er zurückgegeben.
    pub fn entferne_streckenabschnitt(
        &mut self,
        name: streckenabschnitt::Name,
    ) -> Option<(Streckenabschnitt, Fließend)> {
        macro_rules! clean_maps {
            ($($map:ident),*) => {
                $(
                    for Gleis { streckenabschnitt, .. } in self.maps.$map.values_mut() {
                        if streckenabschnitt.as_ref() == Some(&name) {
                            *streckenabschnitt = None;
                        }
                    }
                )*
            };
        }
        clean_maps! {
            geraden,
            kurven,
            weichen,
            dreiwege_weichen,
            kurven_weichen,
            s_kurven_weichen,
            kreuzungen
        }
        self.maps.streckenabschnitte.remove(&name)
    }

    /// Namen und Farbe aller aktuell bekannten Streckenabschnitte.
    pub(crate) fn streckenabschnitte(
        &self,
    ) -> impl Iterator<Item = (&streckenabschnitt::Name, &(Streckenabschnitt, Fließend))> {
        self.maps.streckenabschnitte.iter()
    }

    /// Setze den Streckenabschnitt für das spezifizierte Gleis.
    /// Der bisherige Wert wird zurückgegeben.
    pub fn setze_streckenabschnitt<T: GleiseMap<Z>>(
        &mut self,
        gleis_id: &GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<Option<streckenabschnitt::Name>, GleisEntferntError> {
        let gleis = T::get_map_mut(&mut self.maps).get_mut(gleis_id).ok_or(GleisEntferntError)?;
        Ok(std::mem::replace(&mut gleis.streckenabschnitt, name))
    }

    /// Wie setzte_streckenabschnitt, nur ohne Rückgabewert für Verwendung mit `with_any_id`
    #[inline]
    pub(in crate::application) fn setze_streckenabschnitt_unit<T: GleiseMap<Z>>(
        &mut self,
        gleis_id: &GleisId<T>,
        name: Option<streckenabschnitt::Name>,
    ) -> Result<(), GleisEntferntError> {
        self.setze_streckenabschnitt(gleis_id, name)?;
        Ok(())
    }
}

pub(crate) fn move_to_position(frame: &mut canvas::Frame, position: &Position) {
    // bewege Kontext zur Position
    frame.transformation(&Transformation::Translation(position.punkt));
    // drehe Kontext um (0,0)
    frame.transformation(&Transformation::Rotation(position.winkel));
}

fn transparency<T>(gleis_id: &GleisId<T>, transparent: &impl Fn(GleisId<Any>) -> bool) -> f32 {
    if transparent(gleis_id.as_any()) {
        0.5
    } else {
        1.
    }
}

fn fülle_alle_gleise<T: Zeichnen>(
    frame: &mut canvas::Frame,
    map: &Map<T>,
    transparent: impl Fn(GleisId<Any>, Fließend) -> bool,
    streckenabschnitte: &streckenabschnitt::Map,
) {
    for (gleis_id, Gleis { definition, position, streckenabschnitt }) in map.iter() {
        if let Some((Streckenabschnitt { farbe, .. }, fließend)) =
            streckenabschnitt.as_ref().and_then(|name| streckenabschnitte.get(name))
        {
            frame.with_save(|frame| {
                move_to_position(frame, position);
                // einfärben
                for path in definition.fülle() {
                    frame.with_save(|frame| {
                        let Farbe { r, g, b } = *farbe;
                        let color = iced::Color {
                            r,
                            g,
                            b,
                            a: transparency(gleis_id, &|gleis_id| transparent(gleis_id, *fließend)),
                        };
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
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne Kontur
            for path in definition.zeichne() {
                frame.with_save(|frame| {
                    // TODO aktuelle Richtung für Weichen-artige Gleise anzeigen
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
    map: &Map<T>,
    has_other_and_grabbed_id_at_point: impl Fn(GleisId<Any>, anchor::Anchor) -> (bool, bool),
    is_grabbed: impl Fn(GleisId<Any>) -> bool,
) {
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        frame.with_save(|frame| {
            move_to_position(frame, position);
            // zeichne anchor points
            definition.anchor_points().for_each(|_name, &anchor| {
                frame.with_save(|frame| {
                    let (opposing, grabbed) = has_other_and_grabbed_id_at_point(
                        gleis_id.as_any(),
                        anchor::Anchor {
                            position: position.transformation(anchor.position),
                            richtung: position.winkel + anchor.richtung,
                        },
                    );
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
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        let (relative_position, beschreibung, name) = definition.beschreibung_und_name();
        if let Some(content) = match (beschreibung, name) {
            (Some(beschreibung), Some(name)) => Some(format!("{} ({})", name, beschreibung)),
            (None, Some(name)) => Some(name.clone()),
            (Some(beschreibung), None) => Some(beschreibung.clone()),
            (None, None) => None,
        } {
            let punkt =
                position.punkt + Vektor::from(relative_position.punkt).rotiert(position.winkel);
            let winkel = position.winkel + relative_position.winkel;
            let absolute_position = Position { punkt, winkel };
            frame.with_save(|frame| {
                move_to_position(frame, &absolute_position);
                frame.fill_text(canvas::Text {
                    content,
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

const DOUBLE_CLICK_TIME: Duration = Duration::from_millis(200);

fn find_clicked<T, Z>(map: &mut Map<T>, canvas_pos: Vektor) -> Option<(AnyId<Z>, Vektor)>
where
    T: Zeichnen,
    GleisId<T>: Into<AnyId<Z>>,
{
    // TODO speichere bounding box ebenfalls in rstar, um nicht jedes Gleis durchsuchen zu müssen?
    for (gleis_id, Gleis { definition, position, .. }) in map.iter() {
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        if definition.innerhalb(rotated_pos) {
            return Some((AnyId::from_ref(gleis_id), relative_pos));
        }
    }
    None
}

fn aktion_gleis_an_position<Z>(
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
) -> (iced::canvas::event::Status, Option<Message<Z>>)
where
    Z: Zugtyp,
{
    let mut message = None;
    let mut status = iced::canvas::event::Status::Ignored;
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = get_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let find_clicked_result = find_clicked(geraden, canvas_pos)
                .or(find_clicked(kurven, canvas_pos))
                .or(find_clicked(weichen, canvas_pos))
                .or(find_clicked(dreiwege_weichen, canvas_pos))
                .or(find_clicked(kurven_weichen, canvas_pos))
                .or(find_clicked(s_kurven_weichen, canvas_pos))
                .or(find_clicked(kreuzungen, canvas_pos));
            match modus {
                ModusDaten::Bauen { grabbed, last } => {
                    let now = Instant::now();
                    let diff = now - *last;
                    *last = now;
                    take_mut::take(grabbed, |grabbed| {
                        grabbed.or({
                            if let Some((gleis_id, grab_location)) = find_clicked_result {
                                Some(Grabbed { gleis_id, grab_location, moved: false })
                            } else {
                                None
                            }
                        })
                    });
                    if let Some(Grabbed { gleis_id, .. }) = grabbed {
                        if diff < DOUBLE_CLICK_TIME {
                            message = Some(Message::AnschlüsseAnpassen(gleis_id.clone()))
                        }
                        status = iced::canvas::event::Status::Captured
                    }
                }
                ModusDaten::Fahren => {
                    if let Some((gleis_id, _grab_location)) = find_clicked_result {
                        message = Some(Message::FahrenAktion(gleis_id));
                        status = iced::canvas::event::Status::Captured
                    }
                }
            }
        }
    }
    (status, message)
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum Message<Z> {
    SetzeStreckenabschnitt(AnyId<Z>),
    AnschlüsseAnpassen(AnyId<Z>),
    FahrenAktion(AnyId<Z>),
}

impl<Z: Zugtyp> iced::canvas::Program<Message<Z>> for Gleise<Z> {
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
            // TODO zeichne keine out-of-bounds Gleise
            // Zeichne Gleise
            let grabbed_id: Option<GleisId<Any>>;
            let modus_bauen: bool;
            match modus {
                ModusDaten::Bauen { grabbed: Some(Grabbed { gleis_id, .. }), .. } => {
                    grabbed_id = Some(gleis_id.id_as_any());
                    modus_bauen = true;
                }
                ModusDaten::Bauen { grabbed: None, .. } => {
                    grabbed_id = None;
                    modus_bauen = true;
                }
                ModusDaten::Fahren => {
                    grabbed_id = None;
                    modus_bauen = false;
                }
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
            mit_allen_gleisen!(
                fülle_alle_gleise,
                |gleis_id, fließend| if modus_bauen {
                    is_grabbed(gleis_id)
                } else {
                    fließend == Fließend::Gesperrt
                },
                streckenabschnitte
            );
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
    ) -> (iced::canvas::event::Status, Option<Message<Z>>) {
        let mut event_status = iced::canvas::event::Status::Ignored;
        let mut message = None;
        self.last_size = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) => {
                let Gleise { modus, maps, pivot, skalieren, .. } = self;
                let click_result =
                    aktion_gleis_an_position(&bounds, &cursor, modus, maps, pivot, skalieren);
                event_status = click_result.0;
                message = click_result.1;
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) => {
                if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
                    if let Some(Grabbed { gleis_id, moved, .. }) = &*grabbed {
                        let gleis_id_clone = gleis_id.clone();
                        let moved_copy = *moved;
                        *grabbed = None;
                        if moved_copy {
                            if cursor.is_over(&bounds) {
                                if let Err(GleisEntferntError) =
                                    with_any_id!(gleis_id_clone, Gleise::snap_to_anchor, self)
                                {
                                    error!("Ende Drag&Drop für entferntes Gleis!")
                                }
                            } else {
                                with_any_id!(gleis_id_clone, Gleise::remove, self);
                            }
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            message = Some(Message::SetzeStreckenabschnitt(gleis_id_clone.into()));
                        }
                        event_status = iced::canvas::event::Status::Captured;
                    }
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position: _ }) => {
                if let Some(canvas_pos) =
                    get_canvas_position(&bounds, &cursor, &self.pivot, &self.skalieren)
                {
                    self.last_mouse = canvas_pos;
                    if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
                        if let Some(Grabbed { gleis_id, grab_location, moved }) = grabbed {
                            *moved = true;
                            let point = canvas_pos - grab_location;
                            if let Err(GleisEntferntError) = with_any_id!(
                                gleis_id.clone(),
                                Gleise::relocate_grabbed,
                                self,
                                point
                            ) {
                                error!("Drag&Drop für entferntes Gleis!")
                            }
                            event_status = iced::canvas::event::Status::Captured
                        }
                    }
                }
            }
            _otherwise => {}
        };
        if event_status == iced::canvas::event::Status::Captured {
            self.canvas.clear()
        }
        (event_status, message)
    }

    fn mouse_interaction(
        &self,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> iced::mouse::Interaction {
        match &self.modus {
            ModusDaten::Bauen { grabbed: Some(_grabbed), .. } if cursor.is_over(&bounds) => {
                iced::mouse::Interaction::Pointer
            }
            _ => iced::mouse::Interaction::default(),
        }
    }
}

impl Position {
    /// Position damit anchor::Anchor übereinander mit entgegengesetzter Richtung liegen
    fn attach_position<T>(
        definition: &T,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> Self
    where
        T: Zeichnen,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
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
    pub fn add<T>(&mut self, gleis: Gleis<T>) -> (GleisId<T>, T::AnchorPoints)
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
        let gleis_id = self.next_id();
        // increase next id
        self.next_id += 1;
        // add to anchor_points
        anchor_points
            .for_each(|_name, anchor| self.anchor_points.insert(gleis_id.as_any(), anchor.clone()));
        // add to HashMap
        T::get_map_mut(&mut self.maps).insert(gleis_id.clone(), gleis);
        // trigger redraw
        self.canvas.clear();
        // return value
        (gleis_id, anchor_points)
    }

    /// Add a gleis at the last known mouse position
    /// capped at the last known canvas size.
    pub(crate) fn add_grabbed_at_mouse<T>(
        &mut self,
        definition: T,
        grab_location: Vektor,
        streckenabschnitt: Option<streckenabschnitt::Name>,
    ) -> (GleisId<T>, T::AnchorPoints)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        GleisId<T>: Into<AnyId<Z>>,
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
            streckenabschnitt,
        });
        if let ModusDaten::Bauen { grabbed, .. } = &mut self.modus {
            let gleis_id = result.0.clone().into();
            *grabbed = Some(Grabbed { gleis_id, grab_location, moved: true });
        }
        result
    }

    /// Create a new gleis with anchor_name adjacent to the target_anchor_point.
    pub fn add_attach<T>(
        &mut self,
        definition: T,
        streckenabschnitt: Option<streckenabschnitt::Name>,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> (GleisId<T>, T::AnchorPoints)
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
    pub fn relocate<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        position_neu: Position,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let Gleis { definition, position, .. } =
            T::get_map_mut(&mut self.maps).get_mut(&gleis_id).ok_or(GleisEntferntError)?;
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
        anchor_points.for_each(|_name, anchor| {
            self.anchor_points.remove(gleis_id.as_any(), &anchor);
        });
        // add new to anchor_points
        anchor_points_neu
            .for_each(|_name, anchor| self.anchor_points.insert(gleis_id.as_any(), anchor.clone()));
        // trigger redraw
        self.canvas.clear();
        // return value
        Ok(anchor_points_neu)
    }

    /// Move an existing gleis gleis with anchor_name adjacent to the target_anchor_point.
    pub fn relocate_attach<T>(
        &mut self,
        gleis_id: &GleisId<T>,
        anchor_name: &T::AnchorName,
        target_anchor_point: anchor::Anchor,
    ) -> Result<T::AnchorPoints, GleisEntferntError>
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        let position = {
            let Gleis { definition, .. } =
                T::get_map_mut(&mut self.maps).get(&gleis_id).ok_or(GleisEntferntError)?;
            Position::attach_position(definition, anchor_name, target_anchor_point)
        };
        // move gleis to new position
        self.relocate(gleis_id, position)
    }

    /// Remove the Gleis associated the the GleisId.
    ///
    /// Only the first remove has an effect.
    /// Regardless, after a remove the associated Gleis is guaranteed to be removed.
    pub fn remove<T>(&mut self, gleis_id: GleisId<T>)
    where
        T: Debug + Zeichnen + GleiseMap<Z>,
        T::AnchorPoints: anchor::Lookup<T::AnchorName>,
    {
        if let Some(Gleis { definition, position, .. }) =
            T::get_map_mut(&mut self.maps).remove(&gleis_id)
        {
            // delete from anchor_points
            definition.anchor_points().for_each(|_name, anchor| {
                self.anchor_points.remove(
                    gleis_id.as_any(),
                    &anchor::Anchor {
                        position: position.transformation(anchor.position),
                        richtung: position.winkel + anchor.richtung,
                    },
                );
            });
            // trigger redraw
            self.canvas.clear();
        }
    }

    pub(crate) fn streckenabschnitt_für_id<T>(
        &mut self,
        gleis_id: GleisId<T>,
    ) -> Result<Option<&mut (Streckenabschnitt, Fließend)>, GleisEntferntError>
    where
        T: GleiseMap<Z>,
    {
        if let Some(Gleis { streckenabschnitt, .. }) = T::get_map_mut(&mut self.maps).get(&gleis_id)
        {
            Ok(if let Some(name) = streckenabschnitt {
                let name_clone = name.clone();
                drop(name);
                self.streckenabschnitt_mut(&name_clone)
            } else {
                None
            })
        } else {
            Err(GleisEntferntError)
        }
    }
}

impl<Z: Zugtyp + Serialize> Gleise<Z> {
    #[must_use]
    pub fn speichern(
        &self,
        pfad: impl AsRef<std::path::Path>,
        geschwindigkeiten: geschwindigkeit::Map<<Z::Leiter as ToSave>::Save>,
    ) -> std::result::Result<(), Error> {
        let Gleise { maps, .. } = self;
        let vecs: GleiseVecs<Z> = (maps, geschwindigkeiten).into();
        let file = std::fs::File::create(pfad)?;
        bincode::serialize_into(file, &vecs)?;
        Ok(())
    }
}

impl<Z: Zugtyp + PartialEq + std::fmt::Debug + for<'de> Deserialize<'de>> Gleise<Z> {
    #[must_use]
    pub fn laden(
        &mut self,
        anschlüsse: &mut Anschlüsse,
        pfad: impl AsRef<std::path::Path>,
    ) -> std::result::Result<geschwindigkeit::Map<Z::Leiter>, Error> {
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
            streckenabschnitte,
            geschwindigkeiten,
            pläne: _, // TODO verwenden, sobald Plan implementiert ist
        } = bincode::deserialize_from(file)?;

        if name != Z::NAME {
            return Err(Error::FalscherZugtyp(name));
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

        macro_rules! reserviere_anschlüsse {
            ($name:ident, $source:ident, $(:: $weiche:ident ::)? $module:ident, $data:ident {$steuerung:ident, $($data_feld:ident),*}) => {
                // collect to Vec to fail on first error
                // match to fix error type of closure
                let $name: Vec<_> = match $source
                    .into_iter()
                    .map(
                        |Gleis {
                            definition:
                                super::$($weiche::)?$module::$data {
                                    $steuerung,
                                    $($data_feld),*
                                },
                            position,
                            streckenabschnitt,
                        }| {
                            let steuerung_result: Option<Result<_, anschluss::Error>> = $steuerung.map(
                                |steuerung| Ok(steuerung.reserviere(anschlüsse)?)
                            );
                            let steuerung_reserviert = steuerung_result.transpose()?;
                            Ok(Gleis {
                                definition: super::$($weiche::)?$module::$data {
                                    $steuerung: steuerung_reserviert,
                                    $($data_feld),*
                                },
                                position,
                                streckenabschnitt,
                            })
                        },
                    )
                    .collect()
                {
                    Ok(vec) => vec,
                    Err(error) => return Err(error),
                };
            };
        }
        reserviere_anschlüsse!(
            geraden_reserviert,
            geraden,
            gerade,
            Gerade { kontakt, zugtyp, länge, beschreibung }
        );
        reserviere_anschlüsse!(
            kurven_reserviert,
            kurven,
            kurve,
            Kurve { kontakt, zugtyp, radius, winkel, beschreibung }
        );
        reserviere_anschlüsse!(
            weichen_reserviert,
            weichen,
            ::weiche::gerade,
            Weiche { steuerung, zugtyp, länge, radius, winkel, orientierung, beschreibung }
        );
        reserviere_anschlüsse!(
            dreiwege_weichen_reserviert,
            dreiwege_weichen,
            ::weiche::dreiwege,
            DreiwegeWeiche { steuerung, zugtyp, länge, radius, winkel, beschreibung }
        );
        reserviere_anschlüsse!(
            kurven_weichen_reserviert,
            kurven_weichen,
            ::weiche::kurve,
            KurvenWeiche { steuerung, zugtyp, länge, radius, winkel, orientierung, beschreibung }
        );
        reserviere_anschlüsse!(
            s_kurven_weichen_reserviert,
            s_kurven_weichen,
            ::weiche::s_kurve,
            SKurvenWeiche {
                steuerung,
                zugtyp,
                länge,
                radius,
                winkel,
                radius_reverse,
                winkel_reverse,
                orientierung,
                beschreibung
            }
        );
        reserviere_anschlüsse!(
            kreuzungen_reserviert,
            kreuzungen,
            kreuzung,
            Kreuzung { steuerung, zugtyp, länge, radius, variante, beschreibung }
        );
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
            geraden_reserviert,
            kurven_reserviert,
            weichen_reserviert,
            dreiwege_weichen_reserviert,
            kurven_weichen_reserviert,
            s_kurven_weichen_reserviert,
            kreuzungen_reserviert,
        );
        let streckenabschnitte_reserviert: Vec<_> = match streckenabschnitte
            .into_iter()
            .map(|(name, Streckenabschnitt { farbe, anschluss })| {
                Ok((
                    name,
                    Streckenabschnitt { farbe, anschluss: anschluss.reserviere(anschlüsse)? },
                ))
            })
            .collect()
        {
            Ok(map) => map,
            Err(error) => return Err(error),
        };
        for (name, streckenabschnitt) in streckenabschnitte_reserviert {
            self.neuer_streckenabschnitt(name, streckenabschnitt);
        }

        let geschwindigkeiten_reserviert = geschwindigkeiten
            .into_iter()
            .map(|(name, geschwindigkeit)| Ok((name, geschwindigkeit.reserviere(anschlüsse)?)))
            .collect::<Result<_, anschluss::Error>>()?;
        Ok(geschwindigkeiten_reserviert)
    }
}

macro_rules! steuerung {
    ($name:ident, $type:ty, $map:ident, $richtung:ty, $anschlüsse:ty) => {
        pub(in crate::application) fn $name(
            &mut self,
            gleis_id: &GleisId<$type>,
        ) -> Result<&mut Option<weiche::Weiche<$richtung, $anschlüsse>>, GleisEntferntError> {
            let Gleis { definition, .. } =
                self.maps.$map.get_mut(&gleis_id).ok_or(GleisEntferntError)?;
            Ok(&mut definition.steuerung)
        }
    };
}

impl<Z: Zugtyp> Gleise<Z> {
    steuerung! {
        steuerung_weiche,
        super::Weiche<Z>,
        weichen,
        super::weiche::gerade::Richtung,
        super::weiche::gerade::RichtungAnschlüsse
    }

    steuerung! {
        steuerung_dreiwege_weiche,
        super::DreiwegeWeiche<Z>,
        dreiwege_weichen,
        super::weiche::dreiwege::Richtung,
        super::weiche::dreiwege::RichtungAnschlüsse
    }

    steuerung! {
        steuerung_kurven_weiche,
        super::KurvenWeiche<Z>,
        kurven_weichen,
        super::weiche::kurve::Richtung,
        super::weiche::kurve::RichtungAnschlüsse
    }

    steuerung! {
        steuerung_s_kurven_weiche,
        super::SKurvenWeiche<Z>,
        s_kurven_weichen,
        super::weiche::gerade::Richtung,
        super::weiche::gerade::RichtungAnschlüsse
    }

    steuerung! {
        steuerung_kreuzung,
        super::Kreuzung<Z>,
        kreuzungen,
        super::weiche::gerade::Richtung,
        super::weiche::gerade::RichtungAnschlüsse
    }
}

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Bincode(bincode::Error),
    FalscherZugtyp(String),
    Anschluss(anschluss::Error),
    GleisEntfernt,
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
impl From<anschluss::Error> for Error {
    fn from(error: anschluss::Error) -> Self {
        Error::Anschluss(error)
    }
}

#[derive(Debug)]
pub struct GleisEntferntError;
impl From<GleisEntferntError> for Error {
    fn from(GleisEntferntError: GleisEntferntError) -> Self {
        Error::GleisEntfernt
    }
}
