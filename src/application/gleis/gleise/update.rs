//! update-Methode für Gleise

use std::{
    marker::PhantomData,
    time::{Duration, Instant},
};

use log::error;

use crate::{
    application::{
        gleis::gleise::{daten::*, id::*, Gehalten, Gleise, ModusDaten, Nachricht},
        typen::*,
    },
    steuerung::streckenabschnitt,
    zugtyp::Zugtyp,
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
fn gleis_an_position<T, Z>(
    streckenabschnitt: Option<&streckenabschnitt::Name>,
    rstern: &RStern<T>,
    canvas_pos: Vektor,
) -> Option<(AnyId<Z>, Vektor)>
where
    T: Zeichnen,
    AnyId<Z>: From<GleisId<T>>,
{
    for geom_with_data in rstern.locate_all_at_point(&canvas_pos) {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        if definition.innerhalb(rotated_pos, KLICK_GENAUIGKEIT) {
            let any_id = AnyId::from(GleisId {
                rectangle: *rectangle,
                streckenabschnitt: streckenabschnitt.cloned(),
                phantom: PhantomData::<fn() -> T>,
            });
            return Some((any_id, relative_pos));
        }
    }
    None
}

fn aktion_gleis_an_position<'t, Z: 't>(
    bounds: &'t iced::Rectangle,
    cursor: &'t iced::canvas::Cursor,
    modus: &'t mut ModusDaten<Z>,
    daten_iter: impl Iterator<Item = (Option<&'t streckenabschnitt::Name>, &'t GleiseDaten<Z>)>,
    pivot: &'t Position,
    skalieren: &'t Skalar,
) -> (iced::canvas::event::Status, Option<Nachricht<Z>>)
where
    Z: Zugtyp,
{
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
                acc.or_else(|| gleis_an_position(streckenabschnitt, geraden, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, kurven, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, weichen, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, dreiwege_weichen, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, kurven_weichen, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, s_kurven_weichen, canvas_pos))
                    .or_else(|| gleis_an_position(streckenabschnitt, kreuzungen, canvas_pos))
            });
            match modus {
                ModusDaten::Bauen { gehalten, last } => {
                    let now = Instant::now();
                    let diff = now.checked_duration_since(*last).unwrap_or(Duration::MAX);
                    *last = now;
                    if gehalten.is_none() {
                        if let Some((gleis_id, halte_position)) = gleis_an_position {
                            *gehalten = Some(Gehalten { gleis_id, halte_position, bewegt: false })
                        }
                    }
                    if let Some(Gehalten { gleis_id, .. }) = gehalten {
                        if diff < DOUBLE_CLICK_TIME {
                            message = Some(Nachricht::AnschlüsseAnpassen(gleis_id.clone()))
                        }
                        status = iced::canvas::event::Status::Captured
                    }
                }
                ModusDaten::Fahren => {
                    if let Some((gleis_id, _halte_position)) = gleis_an_position {
                        message = Some(Nachricht::FahrenAktion(gleis_id));
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
    ) -> (iced::canvas::event::Status, Option<Nachricht<Z>>) {
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
                    zustand.alle_streckenabschnitt_daten(),
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
                            if cursor.is_over(&bounds) {
                                if let Err(fehler) =
                                    mit_any_id!(gleis_id, Gleise::einrasten_an_verbindung, self)
                                {
                                    error!("Ende Drag&Drop für entferntes Gleis: {:?}", fehler)
                                }
                            } else {
                                if let Err(fehler) =
                                    mit_any_id!(gleis_id, Gleise::entfernen_unit, self)
                                {
                                    error!("Entfernen für entferntes Gleis: {:?}", fehler)
                                }
                            }
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            message = Some(Nachricht::SetzeStreckenabschnitt(gleis_id.into()));
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
                    if let ModusDaten::Bauen { gehalten, last } = &mut self.modus {
                        if let Some(Gehalten { gleis_id, halte_position, bewegt: _ }) =
                            gehalten.take()
                        {
                            let last_clone = last.clone();
                            let point = canvas_pos - halte_position;
                            match mit_any_id!(
                                gleis_id.clone(),
                                Gleise::bewegen_gehalten,
                                self,
                                point
                            ) {
                                Ok(gleis_id_neu) => {
                                    self.modus = ModusDaten::Bauen {
                                        gehalten: Some(Gehalten {
                                            gleis_id: gleis_id_neu,
                                            halte_position,
                                            bewegt: true,
                                        }),
                                        last: last_clone,
                                    };
                                }
                                Err(fehler) => {
                                    error!("Drag&Drop für entferntes Gleis: {:?}", fehler)
                                }
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
