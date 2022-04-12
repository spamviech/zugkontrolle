//! [update](iced::Application::update)-Methode für [Gleise].

use std::{
    marker::PhantomData,
    time::{Duration, Instant},
};

use iced::{
    canvas::{event, Cursor, Event},
    mouse, Rectangle,
};
use log::error;

use crate::{
    anschluss::polarität::Fließend,
    gleis::{
        gleise::{
            daten::{Gleis, GleiseDaten, RStern},
            id::{mit_any_id, AnyId, AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
            Gehalten, Gleise, ModusDaten, Nachricht,
        },
        weiche::{self, dreiwege::DreiwegeWeiche, kurve::KurvenWeiche},
    },
    steuerung::{
        geschwindigkeit::Leiter,
        plan::{self, AktionSchalten, AktionStreckenabschnitt},
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{
        canvas::Position, mm::Spurweite, skalar::Skalar, vektor::Vektor, winkel::Winkel, Zeichnen,
    },
};

/// Position des Cursors auf einem canvas mit `bounds`.
fn berechne_canvas_position(
    bounds: &Rectangle,
    cursor: &Cursor,
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

enum GleisSteuerung<'t> {
    Streckenabschnitt(Option<&'t Streckenabschnitt>),
    GeradeWeiche(&'t Option<plan::GeradeWeiche>),
    KurvenWeiche(&'t Option<plan::KurvenWeiche>),
    DreiwegeWeiche(&'t Option<plan::DreiwegeWeiche>),
}

/// Erhalte die Id des Gleises an der gesuchten Position.
fn gleis_an_position<'t, T>(
    spurweite: Spurweite,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt, &'t Fließend)>,
    rstern: &'t RStern<T>,
    canvas_pos: Vektor,
    erhalte_steuerung: impl FnOnce(&'t T, Option<&'t Streckenabschnitt>) -> GleisSteuerung<'t>,
) -> Option<(AnyIdRef<'t>, Vektor, Winkel, GleisSteuerung<'t>)>
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
                streckenabschnitt: streckenabschnitt.map(|(id, _, _)| id),
                phantom: PhantomData::<fn() -> T>,
            });
            return Some((
                any_id,
                relative_pos,
                position.winkel,
                erhalte_steuerung(
                    definition,
                    streckenabschnitt.map(|(_, streckenabschnitt, _)| streckenabschnitt),
                ),
            ));
        }
    }
    None
}

fn aktion_gleis_an_position<'t>(
    bounds: &'t Rectangle,
    cursor: &'t Cursor,
    spurweite: Spurweite,
    modus: &'t mut ModusDaten,
    daten_iter: impl Iterator<
        Item = (
            Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt, &'t Fließend)>,
            &'t GleiseDaten,
        ),
    >,
    pivot: &'t Position,
    skalieren: &'t Skalar,
) -> (event::Status, Option<Nachricht>) {
    let mut message = None;
    let mut status = event::Status::Ignored;
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = berechne_canvas_position(&bounds, &cursor, pivot, skalieren) {
            macro_rules! streckenabschnitt_steuerung {
                () => {
                    |_gleis, streckenabschnitt| GleisSteuerung::Streckenabschnitt(streckenabschnitt)
                };
            }
            macro_rules! gerade_weiche_steuerung {
                () => {
                    |weiche, _streckenabschnitt| GleisSteuerung::GeradeWeiche(&weiche.steuerung)
                };
            }
            let dreiwege_weiche_steuerung =
                |dreiwege_weiche: &'t DreiwegeWeiche, _streckenabschnitt| {
                    GleisSteuerung::DreiwegeWeiche(&dreiwege_weiche.steuerung)
                };
            let kurven_weiche_steuerung = |kurven_weiche: &'t KurvenWeiche, _streckenabschnitt| {
                GleisSteuerung::KurvenWeiche(&kurven_weiche.steuerung)
            };
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
                acc.or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        geraden,
                        canvas_pos,
                        streckenabschnitt_steuerung!(),
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        kurven,
                        canvas_pos,
                        streckenabschnitt_steuerung!(),
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        weichen,
                        canvas_pos,
                        gerade_weiche_steuerung!(),
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        dreiwege_weichen,
                        canvas_pos,
                        dreiwege_weiche_steuerung,
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        kurven_weichen,
                        canvas_pos,
                        kurven_weiche_steuerung,
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        s_kurven_weichen,
                        canvas_pos,
                        gerade_weiche_steuerung!(),
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        kreuzungen,
                        canvas_pos,
                        gerade_weiche_steuerung!(),
                    )
                })
            });
            match modus {
                ModusDaten::Bauen { gehalten, last } => {
                    let now = Instant::now();
                    let diff = now.checked_duration_since(*last).unwrap_or(Duration::MAX);
                    *last = now;
                    if gehalten.is_none() {
                        if let Some((any_id_ref, halte_position, winkel, _steuerung)) =
                            gleis_an_position
                        {
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
                        status = event::Status::Captured
                    }
                },
                ModusDaten::Fahren => {
                    if let Some((_any_id_ref, _halte_position, _winkel, steuerung)) =
                        gleis_an_position
                    {
                        use GleisSteuerung::*;
                        message = match steuerung {
                            Streckenabschnitt(Some(streckenabschnitt)) => {
                                let fließend = !streckenabschnitt.fließend();
                                Some(Nachricht::StreckenabschnittUmschalten(
                                    AktionStreckenabschnitt::Strom {
                                        streckenabschnitt: streckenabschnitt.clone(),
                                        fließend,
                                    },
                                ))
                            },
                            GeradeWeiche(Some(weiche)) => {
                                use weiche::gerade::Richtung::*;
                                let richtung = match weiche.aktuelle_richtung {
                                    Gerade => Kurve,
                                    Kurve => Gerade,
                                };
                                Some(Nachricht::WeicheSchalten(AktionSchalten::SchalteGerade {
                                    weiche: weiche.clone(),
                                    richtung,
                                }))
                            },
                            KurvenWeiche(Some(weiche)) => {
                                use weiche::kurve::Richtung::*;
                                let richtung = match weiche.aktuelle_richtung {
                                    Innen => Außen,
                                    Außen => Innen,
                                };
                                Some(Nachricht::WeicheSchalten(AktionSchalten::SchalteKurve {
                                    weiche: weiche.clone(),
                                    richtung,
                                }))
                            },
                            DreiwegeWeiche(Some(weiche)) => {
                                use weiche::dreiwege::Richtung::*;
                                let richtung = match (
                                    weiche.aktuelle_richtung,
                                    weiche.letzte_richtung,
                                ) {
                                    (Gerade, Links) => Rechts,
                                    (Gerade, Rechts) => Links,
                                    (Gerade, Gerade) => {
                                        error!("Letzte und aktuelle Richtung für Dreiwege-Weiche {} sind beide Gerade!", weiche.name.0);
                                        Links
                                    },
                                    (Links, _letzte) => Gerade,
                                    (Rechts, _letzte) => Gerade,
                                };
                                Some(Nachricht::WeicheSchalten(AktionSchalten::SchalteDreiwege {
                                    weiche: weiche.clone(),
                                    richtung,
                                }))
                            },
                            _ => None,
                        };
                        if message.is_some() {
                            status = event::Status::Captured
                        }
                    }
                },
            }
        }
    }
    (status, message)
}

impl<L: Leiter> Gleise<L> {
    /// [update](iced::Application::update)-Methode für [Gleise]
    pub fn update(
        &mut self,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<Nachricht>) {
        let mut event_status = event::Status::Ignored;
        let mut message = None;
        self.last_size = Vektor { x: Skalar(bounds.width), y: Skalar(bounds.height) };
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                let spurweite = self.spurweite();
                let Gleise { modus, zustand, pivot, skalieren, .. } = self;
                let click_result = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    spurweite,
                    modus,
                    zustand.alle_streckenabschnitte_und_daten(),
                    pivot,
                    skalieren,
                );
                event_status = click_result.0;
                message = click_result.1;
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
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
                        event_status = event::Status::Captured;
                    }
                }
            },
            Event::Mouse(mouse::Event::CursorMoved { position: _ }) => {
                if let Some(canvas_pos) =
                    berechne_canvas_position(&bounds, &cursor, &self.pivot, &self.skalieren)
                {
                    self.last_mouse = canvas_pos;
                    if let Err(fehler) = self.gehalten_bewegen(canvas_pos) {
                        error!("Drag&Drop für entferntes Gleis: {:?}", fehler)
                    }
                    event_status = event::Status::Captured
                }
            },
            _otherwise => {},
        };
        if event_status == event::Status::Captured {
            self.canvas.lock().leeren()
        }
        (event_status, message)
    }
}
