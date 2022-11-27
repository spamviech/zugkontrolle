//! [update](iced::Application::update)-Methode für [Gleise].

use std::{
    marker::PhantomData,
    sync::Arc,
    time::{Duration, Instant},
};

use iced::{
    mouse,
    widget::canvas::{event, Cursor, Event},
    Rectangle,
};
use log::error;
use parking_lot::Mutex;

use crate::{
    gleis::{
        gerade::Gerade,
        gleise::{
            daten::{Gleis, GleiseDaten, RStern},
            id::{mit_any_id, AnyId, AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
            steuerung::{MitSteuerung, Steuerung},
            Gehalten, Gleise, ModusDaten, Nachricht,
        },
        kreuzung::Kreuzung,
        kurve::Kurve,
        weiche::{
            self, dreiwege::DreiwegeWeiche, gerade::Weiche, kurve::KurvenWeiche,
            s_kurve::SKurvenWeiche,
        },
    },
    steuerung::{
        geschwindigkeit::Leiter,
        plan::{AktionSchalten, AktionStreckenabschnitt, AnyAktionSchalten},
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{
        canvas::{Cache, Position},
        mm::Spurweite,
        skalar::Skalar,
        vektor::Vektor,
        winkel::Winkel,
        Zeichnen,
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

type IdUndSteuerung<'t, T> = (GleisIdRef<'t, T>, Steuerung<&'t <T as MitSteuerung<'t>>::Steuerung>);

#[derive(zugkontrolle_macros::From)]
enum GleisSteuerung<'t> {
    Gerade(IdUndSteuerung<'t, Gerade>),
    Kurve(IdUndSteuerung<'t, Kurve>),
    Weiche(IdUndSteuerung<'t, Weiche>),
    KurvenWeiche(IdUndSteuerung<'t, KurvenWeiche>),
    DreiwegeWeiche(IdUndSteuerung<'t, DreiwegeWeiche>),
    SKurvenWeiche(IdUndSteuerung<'t, SKurvenWeiche>),
    Kreuzung(IdUndSteuerung<'t, Kreuzung>),
}

impl<'t> GleisSteuerung<'t> {
    fn id(self) -> AnyIdRef<'t> {
        match self {
            GleisSteuerung::Gerade((id, _steuerung)) => id.into(),
            GleisSteuerung::Kurve((id, _steuerung)) => id.into(),
            GleisSteuerung::Weiche((id, _steuerung)) => id.into(),
            GleisSteuerung::KurvenWeiche((id, _steuerung)) => id.into(),
            GleisSteuerung::DreiwegeWeiche((id, _steuerung)) => id.into(),
            GleisSteuerung::SKurvenWeiche((id, _steuerung)) => id.into(),
            GleisSteuerung::Kreuzung((id, _steuerung)) => id.into(),
        }
    }
}

/// Erhalte die Id, Steuerung und Streckenabschnitt des Gleises an der gesuchten Position.
fn gleis_an_position<'t, T>(
    spurweite: Spurweite,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>,
    rstern: &'t RStern<T>,
    canvas_pos: Vektor,
    canvas: &Arc<Mutex<Cache>>,
) -> Option<(GleisSteuerung<'t>, Vektor, Winkel, Option<&'t Streckenabschnitt>)>
where
    T: Zeichnen + MitSteuerung<'t>,
    GleisSteuerung<'t>:
        From<(GleisIdRef<'t, T>, Steuerung<&'t <T as MitSteuerung<'t>>::Steuerung>)>,
{
    for geom_with_data in rstern.locate_all_at_point(&canvas_pos) {
        let rectangle = geom_with_data.geom();
        let Gleis { definition, position } = &geom_with_data.data;
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        if definition.innerhalb(spurweite, rotated_pos, KLICK_GENAUIGKEIT) {
            let (streckenabschnitt_id, streckenabschnitt) =
                if let Some((id, streckenabschnitt)) = streckenabschnitt {
                    (Some(id), Some(streckenabschnitt))
                } else {
                    (None, None)
                };
            let gleis_id_ref: GleisIdRef<'t, T> = GleisIdRef {
                rectangle,
                streckenabschnitt: streckenabschnitt_id,
                phantom: PhantomData,
            };
            return Some((
                (gleis_id_ref, definition.steuerung(canvas.clone())).into(),
                relative_pos,
                position.winkel,
                streckenabschnitt,
            ));
        }
    }
    None
}

/// Aktion für ein im Modus "Fahren" angeklicktes Gleis.
fn aktion_fahren(
    gleis_steuerung: GleisSteuerung<'_>,
    streckenabschnitt: Option<&Streckenabschnitt>,
    canvas: &Arc<Mutex<Cache>>,
) -> Option<Nachricht> {
    use GleisSteuerung::*;
    match gleis_steuerung {
        Gerade(_) | Kurve(_) => streckenabschnitt.map(|streckenabschnitt| {
            let fließend = !streckenabschnitt.fließend();
            Nachricht::StreckenabschnittUmschalten(AktionStreckenabschnitt::Strom {
                streckenabschnitt: Steuerung::neu(streckenabschnitt.clone(), canvas.clone()),
                fließend,
            })
        }),
        Weiche((_id, steuerung)) => steuerung.nur_some().map(|steuerung| {
            use weiche::gerade::Richtung::*;
            let richtung = match steuerung.as_ref().richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: steuerung.konvertiere(|&weiche| weiche.clone()),
                richtung,
            }))
        }),
        KurvenWeiche((_id, steuerung)) => steuerung.nur_some().map(|steuerung| {
            use weiche::kurve::Richtung::*;
            let richtung = match steuerung.as_ref().richtung() {
                Innen => Außen,
                Außen => Innen,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteKurve(AktionSchalten {
                weiche: steuerung.konvertiere(|&weiche| weiche.clone()),
                richtung,
            }))
        }),
        DreiwegeWeiche((_id, steuerung)) => steuerung.nur_some().map(|steuerung| {
            use weiche::dreiwege::{Richtung::*, RichtungInformation};
            let weiche = steuerung.as_ref();
            let richtung = match weiche.richtung() {
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Links } => Rechts,
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Rechts } => Links,
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Gerade } => {
                    error!(
                        "Letzte und aktuelle Richtung für Dreiwege-Weiche {} sind beide Gerade!",
                        weiche.name.0
                    );
                    Links
                },
                RichtungInformation { aktuelle_richtung: Links | Rechts, .. } => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteDreiwege(AktionSchalten {
                weiche: steuerung.konvertiere(|&weiche| weiche.clone()),
                richtung,
            }))
        }),
        SKurvenWeiche((_id, steuerung)) => steuerung.nur_some().map(|steuerung| {
            use weiche::gerade::Richtung::*;
            let richtung = match steuerung.as_ref().richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: steuerung.konvertiere(|&weiche| weiche.clone()),
                richtung,
            }))
        }),
        Kreuzung((_id, steuerung)) => steuerung.nur_some().map(|steuerung| {
            use weiche::gerade::Richtung::*;
            let richtung = match steuerung.as_ref().richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: steuerung.konvertiere(|&weiche| weiche.clone()),
                richtung,
            }))
        }),
    }
}

fn aktion_gleis_an_position<'t>(
    bounds: &'t Rectangle,
    cursor: &'t Cursor,
    spurweite: Spurweite,
    modus: &'t mut ModusDaten,
    daten_iter: impl Iterator<
        Item = (Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>, &'t GleiseDaten),
    >,
    pivot: &'t Position,
    skalieren: &'t Skalar,
    canvas: &Arc<Mutex<Cache>>,
) -> (event::Status, Option<Nachricht>) {
    let mut message = None;
    let mut status = event::Status::Ignored;
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
                acc.or_else(|| {
                    gleis_an_position(spurweite, streckenabschnitt, geraden, canvas_pos, canvas)
                })
                .or_else(|| {
                    gleis_an_position(spurweite, streckenabschnitt, kurven, canvas_pos, canvas)
                })
                .or_else(|| {
                    gleis_an_position(spurweite, streckenabschnitt, weichen, canvas_pos, canvas)
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        dreiwege_weichen,
                        canvas_pos,
                        canvas,
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        kurven_weichen,
                        canvas_pos,
                        canvas,
                    )
                })
                .or_else(|| {
                    gleis_an_position(
                        spurweite,
                        streckenabschnitt,
                        s_kurven_weichen,
                        canvas_pos,
                        canvas,
                    )
                })
                .or_else(|| {
                    gleis_an_position(spurweite, streckenabschnitt, kreuzungen, canvas_pos, canvas)
                })
            });
            match modus {
                ModusDaten::Bauen { gehalten, last } => {
                    let now = Instant::now();
                    let diff = now.checked_duration_since(*last).unwrap_or(Duration::MAX);
                    *last = now;
                    if gehalten.is_none() {
                        if let Some((gleis_steuerung, halte_position, winkel, _streckenabschnitt)) =
                            gleis_an_position
                        {
                            let gleis_id = gleis_steuerung.id().als_id();
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
                    if let Some((gleis_steuerung, _halte_position, _winkel, streckenabschnitt)) =
                        gleis_an_position
                    {
                        message = aktion_fahren(gleis_steuerung, streckenabschnitt, canvas);

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
        &self,
        state: &mut (),
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
                let Gleise { modus, zustand, pivot, skalieren, canvas, .. } = self;
                let click_result = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    spurweite,
                    todo!("modus"),
                    zustand.alle_streckenabschnitte_und_daten(),
                    pivot,
                    skalieren,
                    canvas,
                );
                event_status = click_result.0;
                message = click_result.1;
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
                    if let Some(Gehalten { gleis_id, bewegt, .. }) = gehalten.take() {
                        if bewegt {
                            if !cursor.is_over(&bounds) {
                                let mut_ref: &mut Gleise<L> = todo!("self");
                                let _ = ();

                                if let Err(fehler) =
                                    mit_any_id!(gleis_id, Gleise::entfernen_unit, mut_ref)
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
