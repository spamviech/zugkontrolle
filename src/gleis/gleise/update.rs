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
            daten::{mit_any_gleis, AnyGleis, DatenAuswahl, Gleis, GleiseDaten, RStern},
            id::{
                mit_any_id, AnyId, AnyIdRef, GleisIdRef, StreckenabschnittId,
                StreckenabschnittIdRef,
            },
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
        kontakt::Kontakt,
        plan::{self, AktionSchalten, AktionStreckenabschnitt, AnyAktionSchalten},
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

#[derive(zugkontrolle_macros::From)]
enum GleisSteuerung<'t> {
    Kontakt(Steuerung<&'t Arc<Mutex<Option<Kontakt>>>>),
    GeradeWeiche(Steuerung<&'t Arc<Mutex<Option<plan::GeradeWeiche>>>>),
    KurvenWeiche(Steuerung<&'t Arc<Mutex<Option<plan::KurvenWeiche>>>>),
    DreiwegeWeiche(Steuerung<&'t Arc<Mutex<Option<plan::DreiwegeWeiche>>>>),
}

/// SelectionFunction, die ein angeklicktes Gleis sucht.
struct KlickInnerhalb {
    spurweite: Spurweite,
    canvas_pos: Vektor,
}

impl KlickInnerhalb {
    fn ist_innerhalb<T: Zeichnen>(&self, gleis: &Gleis<T>) -> bool {
        let KlickInnerhalb { spurweite, canvas_pos } = *self;
        let Gleis { definition, position } = gleis;
        let relative_pos = canvas_pos - position.punkt;
        let rotated_pos = relative_pos.rotiert(-position.winkel);
        definition.innerhalb(spurweite, rotated_pos, KLICK_GENAUIGKEIT)
    }
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
        self.ist_innerhalb(&leaf.data)
    }
}

/// Erhalte das Gleis an der gesuchten Position.
fn entferne_gleis_an_position<'t, T>(
    klick_innerhalb: KlickInnerhalb,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>,
    rstern: &'t mut RStern<T>,
    _canvas: &Arc<Mutex<Cache>>,
    _sender: &Sender<AsyncAktualisieren>,
) -> Option<Gehalten>
where
    T: Zeichnen,
    AnyGleis: From<Gleis<T>>,
{
    let canvas_pos = klick_innerhalb.canvas_pos;
    rstern.remove_with_selection_function(klick_innerhalb).map(|geom_with_data| {
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
    })
}

/// Erhalte die Id, Steuerung und Streckenabschnitt des Gleises an der gesuchten Position.
fn gleis_an_position<'t, T>(
    klick_innerhalb: KlickInnerhalb,
    streckenabschnitt: Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>,
    rstern: &'t RStern<T>,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> Option<(GleisSteuerung<'t>, Option<&'t Streckenabschnitt>)>
where
    T: Zeichnen + MitSteuerung<'t>,
    GleisSteuerung<'t>: From<Steuerung<&'t <T as MitSteuerung<'t>>::Steuerung>>,
{
    rstern.locate_with_selection_function(klick_innerhalb).next().map(|geom_with_data| {
        let Gleis { definition, position: _ } = &geom_with_data.data;
        let gleis_steuerung = definition.steuerung(Some(canvas.clone()), sender.clone()).into();
        let streckenabschnitt = streckenabschnitt.map(|(_id, streckenabschnitt)| streckenabschnitt);
        (gleis_steuerung, streckenabschnitt)
    })
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

macro_rules! gleis_an_position {
    (
        $spurweite: expr,
        $canvas_pos: expr,
        $canvas: expr,
        $sender: expr,
        $f: expr,
        $acc: expr,
        $streckenabschnitt: expr,
        $maps: ident { $($map: ident),*} $(,)?
    ) => {{
        let GleiseDaten { $($map),* } = $maps;
        $acc $(.or_else(|| $f(
            KlickInnerhalb {spurweite: $spurweite, canvas_pos: $canvas_pos},
            $streckenabschnitt,
            $map,
            $canvas,
            $sender,
        )))*
    }};
}

macro_rules! finde_gleis_an_position {
    (
        $spurweite: expr,
        $canvas_pos: expr,
        $canvas: expr,
        $sender: expr,
        $daten_iter: expr,
        $f: expr
    ) => {
        $daten_iter.fold(None, |acc, (streckenabschnitt, maps)| {
            // Option ist Copy, falls nur unveränderliche Referenzen involviert sind.
            let streckenabschnitt =
                streckenabschnitt.map(|(id, streckenabschnitt)| (id, &*streckenabschnitt));
            gleis_an_position!(
                $spurweite,
                $canvas_pos,
                $canvas,
                $sender,
                $f,
                acc,
                streckenabschnitt,
                maps {
                    geraden,
                    kurven,
                    weichen,
                    kurven_weichen,
                    dreiwege_weichen,
                    s_kurven_weichen,
                    kreuzungen
                }
            )
        })
    };
}

fn erzeuge_anpassen_nachricht(
    gleis: &AnyGleis,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> Nachricht {
    match gleis {
        AnyGleis::Gerade(Gleis { definition, .. }) => Nachricht::KontaktAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::Kurve(Gleis { definition, .. }) => Nachricht::KontaktAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::Weiche(Gleis { definition, .. }) => Nachricht::GeradeWeicheAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::DreiwegeWeiche(Gleis { definition, .. }) => Nachricht::DreiwegeWeicheAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::KurvenWeiche(Gleis { definition, .. }) => Nachricht::KurvenWeicheAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::SKurvenWeiche(Gleis { definition, .. }) => Nachricht::GeradeWeicheAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
        AnyGleis::Kreuzung(Gleis { definition, .. }) => Nachricht::GeradeWeicheAnpassen(
            definition.steuerung(Some(canvas.clone()), sender.clone()).konvertiere(Clone::clone),
        ),
    }
}

fn erzeuge_steuerung_nachricht(
    gleis_steuerung: GleisSteuerung<'_>,
    streckenabschnitt: Option<&Streckenabschnitt>,
    canvas: &Arc<Mutex<Cache>>,
    sender: &Sender<AsyncAktualisieren>,
) -> Option<Nachricht> {
    use GleisSteuerung::*;
    match gleis_steuerung {
        Kontakt(_) => streckenabschnitt.map(|streckenabschnitt| {
            let fließend = !streckenabschnitt.fließend();
            Nachricht::StreckenabschnittUmschalten(AktionStreckenabschnitt::Strom {
                streckenabschnitt: Steuerung::neu_mit_canvas(
                    streckenabschnitt.clone(),
                    canvas.clone(),
                    sender.clone(),
                ),
                fließend,
            })
        }),
        GeradeWeiche(steuerung) => erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
            use weiche::gerade::Richtung::*;
            match weiche.aktuelle_richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            }
        }),
        KurvenWeiche(steuerung) => erzeuge_weiche_schalten_nachricht(steuerung, |weiche| {
            use weiche::kurve::Richtung::*;
            match weiche.aktuelle_richtung() {
                Innen => Außen,
                Außen => Innen,
            }
        }),
        DreiwegeWeiche(steuerung) => {
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
    }
}

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
            match modus {
                ModusDaten::Bauen { gehalten, last } => {
                    let now = Instant::now();
                    let diff = now.checked_duration_since(*last).unwrap_or(Duration::MAX);
                    *last = now;
                    if gehalten.is_none() {
                        *gehalten = finde_gleis_an_position!(
                            spurweite,
                            canvas_pos,
                            canvas,
                            sender,
                            daten_iter,
                            entferne_gleis_an_position
                        );
                    }
                    if let Some(Gehalten { gleis, .. }) = gehalten {
                        if diff < DOUBLE_CLICK_TIME {
                            let nachricht = erzeuge_anpassen_nachricht(gleis, canvas, sender);
                            message = Some(nachricht);
                            *gehalten = None
                        }
                        status = event::Status::Captured
                    }
                },
                ModusDaten::Fahren => {
                    let gleis_an_position = finde_gleis_an_position!(
                        spurweite,
                        canvas_pos,
                        canvas,
                        sender,
                        daten_iter,
                        gleis_an_position
                    );
                    if let Some((gleis_steuerung, streckenabschnitt)) = gleis_an_position {
                        message = erzeuge_steuerung_nachricht(
                            gleis_steuerung,
                            streckenabschnitt,
                            canvas,
                            sender,
                        );

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

/// Synonym von [GleiseDaten::hinzufügen], zur Verwendung mit [mit_any_gleis].
#[inline(always)]
fn gleis_hinzufügen<T: Zeichnen + DatenAuswahl>(
    daten: &mut GleiseDaten,
    gleis: Gleis<T>,
    spurweite: Spurweite,
    streckenabschnitt: Option<StreckenabschnittId>,
) {
    let Gleis { definition, position } = gleis;
    let _ = daten.hinzufügen(definition, spurweite, position, streckenabschnitt);
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
                    if let Some(Gehalten { gleis, bewegt, streckenabschnitt, .. }) = gehalten.take()
                    {
                        let hinzufügen = if bewegt {
                            // Entferne Gleis, wenn es aus dem canvas bewegt wurde.
                            cursor.is_over(&bounds)
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            message = Some(Nachricht::SetzeStreckenabschnittGehalten);
                            true
                        };
                        let streckenabschnitt_id = streckenabschnitt.map(|(id, _farbe)| id);
                        if hinzufügen {
                            let spurweite = self.zustand.zugtyp.spurweite;
                            let daten = match self.zustand.daten_mut(&streckenabschnitt_id) {
                                Ok(daten) => daten,
                                Err(fehler) => {
                                    error!("Streckenabschnitt des gehaltenes Gleises entfernt: {fehler:?}");
                                    &mut self.zustand.ohne_streckenabschnitt
                                },
                            };
                            mit_any_gleis!(
                                gleis,
                                gleis_hinzufügen,
                                daten,
                                spurweite,
                                streckenabschnitt_id
                            );
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
