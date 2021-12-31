//! update-Methode für Gleise

use std::{
    marker::PhantomData,
    time::{Duration, Instant},
};

use log::error;

use crate::application::{
    gleis::gleise::{daten::*, id::*, Gehalten, Gleise, ModusDaten, Nachricht},
    typen::*,
};

/// Position des Cursors auf einem canvas mit `bounds`.
fn berechne_canvas_position(
    bounds: &iced::Rectangle,
    cursor: &iced::canvas::Cursor,
    pivot: &Position,
    skalieren: &Skalar,
) -> Option<Vektor> {
    // `position_in` gibt nur in-bounds einen `Some`-Wert zurück.
    // `position` hat diese Einschränkung nicht,
    // dafür muss die Position explizit abgezogen werden.
    cursor.position().map(|pos| {
        let relative_position = Vektor { x: Skalar(pos.x - bounds.x), y: Skalar(pos.y - bounds.y) };
        pivot.punkt + (relative_position / skalieren).rotiert(-pivot.winkel)
    })
}

const DOUBLE_CLICK_TIME: Duration = Duration::from_millis(200);

// TODO innerhalb auf enum umstellen, dass zwischen
// wirklich_innerhalb und innerhalb_toleranz unterscheidet?
const KLICK_GENAUIGKEIT: Skalar = Skalar(5.);

/// Erhalte die Id des Gleises an der gesuchten Position.
fn gleis_an_position<'t, T>(
    spurweite: Spurweite,
    streckenabschnitt: Option<StreckenabschnittIdRef<'t>>,
    rstern: &'t RStern<T>,
    canvas_pos: Vektor,
) -> Option<(AnyIdRef<'t>, Vektor, Winkel)>
where
    T: Zeichnen,
    AnyIdRef<'t>: From<GleisIdRef<'t, T>>,
{
    for geom_with_data in rstern.locate_all_at_point(&canvas_pos) {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        if definition.innerhalb(spurweite, rotated_pos, KLICK_GENAUIGKEIT) {
            let any_id = AnyIdRef::from(GleisIdRef {
                rectangle,
                streckenabschnitt,
                phantom: PhantomData::<fn() -> T>,
            });
            return Some((any_id, relative_pos, position.winkel));
        }
    }
    None
}

fn aktion_gleis_an_position<'t>(
    bounds: &'t iced::Rectangle,
    cursor: &'t iced::canvas::Cursor,
    spurweite: Spurweite,
    modus: &'t mut ModusDaten,
    daten_iter: impl Iterator<Item = (Option<StreckenabschnittIdRef<'t>>, &'t GleiseDaten)>,
    pivot: &'t Position,
    skalieren: &'t Skalar,
) -> (iced::canvas::event::Status, Option<Nachricht>) {
    let mut message = None;
    let mut status = iced::canvas::event::Status::Ignored;
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = berechne_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let gleis_an_position = daten_iter.fold(None, |acc, (streckenabschnitt, maps)| {
                let GleiseDaten {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                } = maps;
                acc.or_else(|| gleis_an_position(spurweite, streckenabschnitt, geraden, canvas_pos))
                    .or_else(|| gleis_an_position(spurweite, streckenabschnitt, kurven, canvas_pos))
                    .or_else(|| {
                        gleis_an_position(spurweite, streckenabschnitt, weichen, canvas_pos)
                    })
                    .or_else(|| {
                        gleis_an_position(
                            spurweite,
                            streckenabschnitt,
                            dreiwege_weichen,
                            canvas_pos,
                        )
                    })
                    .or_else(|| {
                        gleis_an_position(spurweite, streckenabschnitt, kurven_weichen, canvas_pos)
                    })
                    .or_else(|| {
                        gleis_an_position(
                            spurweite,
                            streckenabschnitt,
                            s_kurven_weichen,
                            canvas_pos,
                        )
                    })
                    .or_else(|| {
                        gleis_an_position(spurweite, streckenabschnitt, kreuzungen, canvas_pos)
                    })
            });
            match modus {
                ModusDaten::Bauen { gehalten, last } => {
                    let now = Instant::now();
                    let diff = now.checked_duration_since(*last).unwrap_or(Duration::MAX);
                    *last = now;
                    if gehalten.is_none() {
                        if let Some((any_id_ref, halte_position, winkel)) = gleis_an_position {
                            let gleis_id = any_id_ref.als_id();
                            *gehalten =
                                Some(Gehalten { gleis_id, halte_position, winkel, bewegt: false })
                        }
                    }
                    if let Some(Gehalten { gleis_id, .. }) = gehalten {
                        if diff < DOUBLE_CLICK_TIME {
                            message = Some(Nachricht::AnschlüsseAnpassen(gleis_id.klonen()));
                            *gehalten = None
                        }
                        status = iced::canvas::event::Status::Captured
                    }
                }
                ModusDaten::Fahren => {
                    if let Some((any_id_ref, _halte_position, _winkel)) = gleis_an_position {
                        let gleis_id = any_id_ref.als_id();
                        message = Some(Nachricht::FahrenAktion(gleis_id));
                        status = iced::canvas::event::Status::Captured
                    }
                }
            }
        }
    }
    (status, message)
}

impl<Leiter> Gleise<Leiter> {
    pub fn update(
        &mut self,
        event: iced::canvas::Event,
        bounds: iced::Rectangle,
        cursor: iced::canvas::Cursor,
    ) -> (iced::canvas::event::Status, Option<Nachricht>) {
        let mut event_status = iced::canvas::event::Status::Ignored;
        let mut message = None;
        self.last_size = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
        match event {
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonPressed(
                iced::mouse::Button::Left,
            )) => {
                let spurweite = self.spurweite();
                let Gleise { modus, zustand, pivot, skalieren, .. } = self;
                let click_result = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    spurweite,
                    modus,
                    zustand.alle_geschwindigkeit_streckenabschnitt_daten(),
                    pivot,
                    skalieren,
                );
                event_status = click_result.0;
                message = click_result.1;
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::ButtonReleased(
                iced::mouse::Button::Left,
            )) => {
                if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
                    if let Some(Gehalten { gleis_id, bewegt, .. }) = gehalten.take() {
                        if bewegt {
                            if !cursor.is_over(&bounds) {
                                if let Err(fehler) =
                                    mit_any_id!(gleis_id, Gleise::entfernen_unit, self)
                                {
                                    error!("Entfernen für entferntes Gleis: {:?}", fehler)
                                }
                            }
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            message = Some(Nachricht::SetzeStreckenabschnitt(gleis_id.klonen()));
                        }
                        event_status = iced::canvas::event::Status::Captured;
                    }
                }
            }
            iced::canvas::Event::Mouse(iced::mouse::Event::CursorMoved { position: _ }) => {
                if let Some(canvas_pos) =
                    berechne_canvas_position(&bounds, &cursor, &self.pivot, &self.skalieren)
                {
                    self.last_mouse = canvas_pos;
                    if let Err(fehler) = self.gehalten_bewegen(canvas_pos) {
                        error!("Drag&Drop für entferntes Gleis: {:?}", fehler)
                    }
                    event_status = iced::canvas::event::Status::Captured
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
