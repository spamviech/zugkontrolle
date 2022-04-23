//! [update](iced::Application::update)-Methode für [Gleise].

use std::{
    marker::PhantomData,
    sync::{mpsc::Sender, Arc},
    time::{Duration, Instant},
};

use iced::{
    canvas::{event, Cursor, Event},
    mouse, Rectangle,
};
use log::error;
use parking_lot::Mutex;
use rstar::{
    primitives::{GeomWithData, Rectangle as RStarRectangle},
    RTreeObject, SelectionFunction, AABB,
};

use crate::{
    application::steuerung::{AsyncAktualisieren, MitSteuerung, Steuerung},
    gleis::{
        gerade::Gerade,
        gleise::{
            daten::{AnyGleis, Gleis, GleiseDaten, RStern},
            id::{mit_any_id, AnyId, AnyIdRef, GleisIdRef, StreckenabschnittIdRef},
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
        self,
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

/// SelectionFunction, die ein angeklicktes Gleis sucht.
struct KlickInnerhalb {
    spurweite: Spurweite,
    canvas_pos: Vektor,
}

impl<T: Zeichnen> SelectionFunction<GeomWithData<RStarRectangle<Vektor>, Gleis<T>>>
    for KlickInnerhalb
{
    fn should_unpack_parent(&self, envelope: &AABB<Vektor>) -> bool {
        let Vektor { x, y } = self.canvas_pos;
        let upper = envelope.upper();
        let lower = envelope.lower();
        lower.x <= x && x <= upper.y && lower.y <= y && y <= upper.y
    }

    fn should_unpack_leaf(&self, leaf: &GeomWithData<RStarRectangle<Vektor>, Gleis<T>>) -> bool {
        let KlickInnerhalb { spurweite, canvas_pos } = *self;
        let Gleis { definition, position } = &leaf.data;
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        definition.innerhalb(spurweite, rotated_pos, KLICK_GENAUIGKEIT)
    }
}

/// Erhalte das Gleis an der gesuchten Position.
fn entferne_gleis_an_position<'t, T>(
    spurweite: Spurweite,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t mut Streckenabschnitt)>,
    rstern: &'t RStern<T>,
    canvas_pos: Vektor,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> Option<Gehalten>
where
    T: Zeichnen + MitSteuerung<'t>,
    AnyGleis: From<Gleis<T>>,
{
    rstern.remove_with_selection_function(KlickInnerhalb { spurweite, canvas_pos }).map(
        |geom_with_data| {
            let gleis = geom_with_data.data;
            let halte_position = canvas_pos - gleis.position.punkt;
            let winkel = gleis.position.winkel;
            Gehalten {
                gleis: gleis.into(),
                streckenabschnitt: streckenabschnitt
                    .map(|(id_ref, streckenabschnitt)| (id_ref.als_id(), streckenabschnitt.farbe)),
                halte_position,
                winkel,
                bewegt: false,
            }
        },
    )
}

/// Erhalte die Id, Steuerung und Streckenabschnitt des Gleises an der gesuchten Position.
fn gleis_an_position<'t, T>(
    spurweite: Spurweite,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t mut Streckenabschnitt)>,
    rstern: &'t RStern<T>,
    canvas_pos: Vektor,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> Option<(GleisSteuerung<'t>, Vektor, Winkel, Option<&'t Streckenabschnitt>)>
where
    T: Zeichnen + MitSteuerung<'t>,
    GleisSteuerung<'t>: From<IdUndSteuerung<'t, T>>,
{
    rstern.locate_with_selection_function(KlickInnerhalb { spurweite, canvas_pos }).next().map(
        |geom_with_data| {
            let rectangle = geom_with_data.geom();
            let Gleis { definition, position } = &geom_with_data.data;
            let relative_pos = canvas_pos - position.punkt;
            let rotated_pos = relative_pos.rotiert(-position.winkel);
            let (streckenabschnitt_id, streckenabschnitt) =
                if let Some((id, streckenabschnitt)) = streckenabschnitt {
                    (Some(id), Some(&*streckenabschnitt))
                } else {
                    (None, None)
                };
            let gleis_id_ref: GleisIdRef<'t, T> = GleisIdRef {
                rectangle,
                streckenabschnitt: streckenabschnitt_id,
                phantom: PhantomData,
            };
            let gleis_steuerung =
                (gleis_id_ref, definition.steuerung(canvas.clone(), sender.clone())).into();
            (gleis_steuerung, relative_pos, position.winkel, streckenabschnitt)
        },
    )
}

fn erzeuge_weiche_schalten_nachricht<Richtung, Weiche>(
    steuerung: Steuerung<&Arc<Mutex<Option<Weiche>>>, AsyncAktualisieren>,
    nächste_richtung: impl FnOnce(&Weiche) -> Richtung,
) -> Option<Nachricht>
where
    Weiche: Clone,
    Richtung: Clone,
    AnyAktionSchalten: From<AktionSchalten<Steuerung<Weiche>, Richtung>>,
{
    let steuerung = steuerung.konvertiere(|mutex| mutex.lock().clone());
    steuerung.nur_some().map(|steuerung| {
        let weiche = steuerung.as_ref();
        let richtung = nächste_richtung(&weiche);
        Nachricht::WeicheSchalten(AnyAktionSchalten::from(AktionSchalten {
            weiche: steuerung,
            richtung,
        }))
    })
}

type StreckenabschnittUndDatenMut<'t> =
    (Option<(StreckenabschnittIdRef<'t>, &'t mut Streckenabschnitt)>, &'t mut GleiseDaten);

fn aktion_gleis_an_position<'t>(
    bounds: &'t Rectangle,
    cursor: &'t Cursor,
    spurweite: Spurweite,
    modus: &'t mut ModusDaten,
    daten_iter: impl Iterator<Item = StreckenabschnittUndDatenMut<'t>>,
    pivot: &'t Position,
    skalieren: &'t Skalar,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> (event::Status, Option<Nachricht>) {
    let mut message = None;
    let mut status = event::Status::Ignored;
    if cursor.is_over(&bounds) {
        if let Some(canvas_pos) = berechne_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let gleis_an_position = daten_iter.fold(None, |acc, (streckenabschnitt, maps)| {
                macro_rules! gleis_an_position {
                    ($($map: ident),* $(,)?) => {{
                        let GleiseDaten { $($map),* } = maps;
                        acc $(.or_else(|| gleis_an_position(
                            spurweite,
                            streckenabschnitt,
                            $map,
                            canvas_pos,
                            canvas,
                            sender,
                        )))*
                    }};
                }
                gleis_an_position!(
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen,
                )
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
                            *gehalten = Some(Gehalten {
                                gleis: todo!(),
                                streckenabschnitt: todo!(),
                                halte_position,
                                winkel,
                                bewegt: false,
                            })
                        }
                    }
                    if let Some(Gehalten { gleis, .. }) = gehalten {
                        if diff < DOUBLE_CLICK_TIME {
                            // message = Some(Nachricht::AnschlüsseAnpassen(gleis_id.klonen()));
                            message = Some(todo!());
                            *gehalten = None
                        }
                        status = event::Status::Captured
                    }
                },
                ModusDaten::Fahren => {
                    if let Some((gleis_steuerung, _halte_position, _winkel, streckenabschnitt)) =
                        gleis_an_position
                    {
                        use GleisSteuerung::*;
                        message = match gleis_steuerung {
                            Gerade(_) | Kurve(_) => streckenabschnitt.map(|streckenabschnitt| {
                                let fließend = !streckenabschnitt.fließend();
                                Nachricht::StreckenabschnittUmschalten(
                                    AktionStreckenabschnitt::Strom {
                                        streckenabschnitt: Steuerung::neu_mit_canvas(
                                            streckenabschnitt.clone(),
                                            canvas.clone(),
                                            sender.clone(),
                                        ),
                                        fließend,
                                    },
                                )
                            }),
                            Weiche((_id, steuerung)) => {
                                erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
                                    use weiche::gerade::Richtung::*;
                                    match weiche.aktuelle_richtung() {
                                        Gerade => Kurve,
                                        Kurve => Gerade,
                                    }
                                })
                            },
                            KurvenWeiche((_id, steuerung)) => {
                                erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
                                    use weiche::kurve::Richtung::*;
                                    match weiche.aktuelle_richtung() {
                                        Innen => Außen,
                                        Außen => Innen,
                                    }
                                })
                            },
                            DreiwegeWeiche((_id, steuerung)) => {
                                erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
                                    use weiche::dreiwege::Richtung::*;
                                    match weiche.aktuelle_und_letzte_richtung() {
                                        (Gerade, Links) => Rechts,
                                        (Gerade, Rechts) => Links,
                                        (Gerade, Gerade) => {
                                            error!("Letzte und aktuelle Richtung für Dreiwege-Weiche {} sind beide Gerade!", weiche.name.0);
                                            Links
                                        },
                                        (Links | Rechts, _letzte) => Gerade,
                                    }
                                })
                            },
                            SKurvenWeiche((_id, steuerung)) => {
                                erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
                                    use weiche::gerade::Richtung::*;
                                    match weiche.aktuelle_richtung() {
                                        Gerade => Kurve,
                                        Kurve => Gerade,
                                    }
                                })
                            },
                            Kreuzung((_id, steuerung)) => {
                                erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
                                    use weiche::gerade::Richtung::*;
                                    match weiche.aktuelle_richtung() {
                                        Gerade => Kurve,
                                        Kurve => Gerade,
                                    }
                                })
                            },
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
                let Gleise { modus, zustand, pivot, skalieren, canvas, sender, .. } = self;
                let click_result = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    spurweite,
                    modus,
                    zustand.alle_streckenabschnitte_und_daten_mut(),
                    pivot,
                    skalieren,
                    canvas,
                    sender,
                );
                event_status = click_result.0;
                message = click_result.1;
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
                    if let Some(Gehalten { gleis, bewegt, .. }) = gehalten.take() {
                        if bewegt {
                            if !cursor.is_over(&bounds) {
                                todo!()
                                // if let Err(fehler) =
                                //     mit_any_id!(gleis_id, Gleise::entfernen_unit, self)
                                // {
                                //     error!("Entfernen für entferntes Gleis: {:?}", fehler)
                                // }
                            }
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            message = Some(todo!());
                            // message = Some(Nachricht::SetzeStreckenabschnitt(gleis_id.klonen()));
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
