//! update-Methode für Gleise

use std::time::{Duration, Instant};

use log::error;

use crate::{
    application::{
        gleis::gleise::{
            id::*, maps::*, GleisEntferntFehler, Gleise, Grabbed, Message, ModusDaten,
        },
        typen::*,
    },
    zugtyp::Zugtyp,
};

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

fn find_clicked<T, Z>(map: &Map<T>, canvas_pos: Vektor) -> Option<(AnyId<Z>, Vektor)>
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

fn aktion_gleis_an_position<'t, Z: 't>(
    bounds: &'t iced::Rectangle,
    cursor: &'t iced::canvas::Cursor,
    modus: &'t mut ModusDaten<Z>,
    maps_iter: impl Iterator<Item = &'t GleiseMaps<Z>>,
    pivot: &'t Position,
    skalieren: &'t Skalar,
) -> (iced::canvas::event::Status, Option<Message<Z>>)
where
    Z: Zugtyp,
{
    let mut message = None;
    let mut status = iced::canvas::event::Status::Ignored;
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = get_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let find_clicked_result = maps_iter.fold(None, |acc, maps| {
                let GleiseMaps {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                } = maps;
                acc.or_else(|| find_clicked(geraden, canvas_pos))
                    .or_else(|| find_clicked(kurven, canvas_pos))
                    .or_else(|| find_clicked(weichen, canvas_pos))
                    .or_else(|| find_clicked(dreiwege_weichen, canvas_pos))
                    .or_else(|| find_clicked(kurven_weichen, canvas_pos))
                    .or_else(|| find_clicked(s_kurven_weichen, canvas_pos))
                    .or_else(|| find_clicked(kreuzungen, canvas_pos))
            });
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

impl<Z: Zugtyp> Gleise<Z> {
    pub fn update(
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
                let Gleise { modus, zustand, pivot, skalieren, .. } = self;
                let click_result = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    modus,
                    zustand.alle_gleise_maps(),
                    pivot,
                    skalieren,
                );
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
                                if let Err(GleisEntferntFehler) =
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
                            if let Err(GleisEntferntFehler) = with_any_id!(
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
            self.canvas.leeren()
        }
        (event_status, message)
    }
}
