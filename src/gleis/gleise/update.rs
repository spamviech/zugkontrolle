//! [update](iced::widget::canvas::Program::update)-Methode für [Gleise].

use std::{
    marker::PhantomData,
    sync::Arc,
    time::{Duration, Instant},
};

use iced::{
    mouse,
    widget::canvas::{event, Cursor, Event, Program},
    Rectangle,
};
use log::error;
use nonempty::{nonempty, NonEmpty};
use parking_lot::Mutex;

use crate::{
    anschluss::Serialisiere,
    application::style::thema::Thema,
    gleis::{
        gerade::Gerade,
        gleise::{
            daten::{Gleis, GleiseDaten, RStern},
            id::{mit_any_id, GleisIdRef, StreckenabschnittIdRef},
            nachricht::{
                GleisSteuerung, IdUndSteuerungSerialisiert, Nachricht, ZustandAktualisieren,
                ZustandAktualisierenEnum,
            },
            steuerung::{MitSteuerung, Steuerung},
            Gehalten, GleisIdFehler, Gleise, ModusDaten,
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
enum GleisSteuerungRef<'t> {
    Gerade(IdUndSteuerung<'t, Gerade>),
    Kurve(IdUndSteuerung<'t, Kurve>),
    Weiche(IdUndSteuerung<'t, Weiche>),
    KurvenWeiche(IdUndSteuerung<'t, KurvenWeiche>),
    DreiwegeWeiche(IdUndSteuerung<'t, DreiwegeWeiche>),
    SKurvenWeiche(IdUndSteuerung<'t, SKurvenWeiche>),
    Kreuzung(IdUndSteuerung<'t, Kreuzung>),
}

fn klone_und_serialisiere<'t, T, R, S>(
    (id_ref, steuerung): IdUndSteuerung<'t, T>,
) -> IdUndSteuerungSerialisiert<T, Option<S>>
where
    T: MitSteuerung<'t, Steuerung = Option<R>>,
    R: Serialisiere<S>,
{
    (id_ref.als_id(), steuerung.opt_as_ref().map(Serialisiere::serialisiere))
}

impl From<GleisSteuerungRef<'_>> for GleisSteuerung {
    fn from(gleis_steuerung_ref: GleisSteuerungRef<'_>) -> Self {
        match gleis_steuerung_ref {
            GleisSteuerungRef::Gerade(steuerung_ref) => {
                GleisSteuerung::Gerade(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::Kurve(steuerung_ref) => {
                GleisSteuerung::Kurve(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::Weiche(steuerung_ref) => {
                GleisSteuerung::Weiche(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::KurvenWeiche(steuerung_ref) => {
                GleisSteuerung::KurvenWeiche(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::DreiwegeWeiche(steuerung_ref) => {
                GleisSteuerung::DreiwegeWeiche(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::SKurvenWeiche(steuerung_ref) => {
                GleisSteuerung::SKurvenWeiche(klone_und_serialisiere(steuerung_ref))
            },
            GleisSteuerungRef::Kreuzung(steuerung_ref) => {
                GleisSteuerung::Kreuzung(klone_und_serialisiere(steuerung_ref))
            },
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
) -> Option<(GleisSteuerungRef<'t>, Vektor, Winkel, Option<&'t Streckenabschnitt>)>
where
    T: Zeichnen + MitSteuerung<'t>,
    GleisSteuerungRef<'t>:
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
    gleis_steuerung_ref: GleisSteuerungRef<'_>,
    streckenabschnitt: Option<&Streckenabschnitt>,
    canvas: &Arc<Mutex<Cache>>,
) -> Option<Nachricht> {
    use GleisSteuerungRef::*;
    match gleis_steuerung_ref {
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
    modus: &'t ModusDaten,
    daten_iter: impl Iterator<
        Item = (Option<(StreckenabschnittIdRef<'t>, &'t Streckenabschnitt)>, &'t GleiseDaten),
    >,
    pivot: &'t Position,
    skalieren: &'t Skalar,
    canvas: &Arc<Mutex<Cache>>,
) -> (event::Status, Vec<Nachricht>) {
    let mut messages = Vec::new();
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
                ModusDaten::Bauen { gehalten, letzter_klick } => {
                    let now = Instant::now();
                    messages.push(Nachricht::from(ZustandAktualisierenEnum::LetzterKlick(now)));
                    let diff = now.checked_duration_since(*letzter_klick).unwrap_or(Duration::MAX);
                    if gehalten.is_none() {
                        if let Some((
                            gleis_steuerung_ref,
                            halte_position,
                            winkel,
                            _streckenabschnitt,
                        )) = gleis_an_position
                        {
                            let gleis_steuerung = GleisSteuerung::from(gleis_steuerung_ref);
                            messages.push(Nachricht::from(
                                ZustandAktualisierenEnum::GehaltenAktualisieren(Some(Gehalten {
                                    gleis_steuerung,
                                    halte_position,
                                    winkel,
                                    bewegt: false,
                                })),
                            ))
                        }
                    }
                    if let Some(Gehalten { gleis_steuerung, .. }) = gehalten {
                        if diff < DOUBLE_CLICK_TIME {
                            messages.push(Nachricht::AnschlüsseAnpassen(gleis_steuerung.klonen()));
                            messages.push(Nachricht::from(
                                ZustandAktualisierenEnum::GehaltenAktualisieren(None),
                            ))
                        }
                        status = event::Status::Captured
                    }
                },
                ModusDaten::Fahren => {
                    if let Some((
                        gleis_steuerung_ref,
                        _halte_position,
                        _winkel,
                        streckenabschnitt,
                    )) = gleis_an_position
                    {
                        let message = aktion_fahren(gleis_steuerung_ref, streckenabschnitt, canvas);

                        if let Some(message) = message {
                            messages.push(message);
                            status = event::Status::Captured
                        }
                    }
                },
            }
        }
    }
    (status, messages)
}

impl<L: Leiter> Gleise<L> {
    /// [update](iced::widget::canvas::Program::update)-Methode für [Gleise]
    pub fn update(
        &self,
        _state: &mut <Self as Program<NonEmpty<Nachricht>, Thema>>::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<NonEmpty<Nachricht>>) {
        let mut event_status = event::Status::Ignored;
        let mut messages =
            nonempty![Nachricht::from(ZustandAktualisierenEnum::LetzteCanvasGröße(Vektor {
                x: Skalar(bounds.width),
                y: Skalar(bounds.height),
            }))];
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                let spurweite = self.spurweite();
                let Gleise { zustand, pivot, skalieren, canvas, modus, .. } = self;
                let (status, nachrichten) = aktion_gleis_an_position(
                    &bounds,
                    &cursor,
                    spurweite,
                    modus,
                    zustand.alle_streckenabschnitte_und_daten(),
                    pivot,
                    skalieren,
                    canvas,
                );
                event_status = status;
                messages.extend(nachrichten);
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                if let ModusDaten::Bauen { gehalten, .. } = &self.modus {
                    if let Some(Gehalten { gleis_steuerung, bewegt, .. }) = gehalten {
                        let gleis_id = gleis_steuerung.id().als_id();
                        if *bewegt {
                            if !cursor.is_over(&bounds) {
                                messages.push(Nachricht::from(
                                    ZustandAktualisierenEnum::GleisEntfernen(gleis_id),
                                ));
                            }
                        } else {
                            // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                            messages.push(Nachricht::SetzeStreckenabschnitt(gleis_id));
                        }
                        messages.push(Nachricht::from(
                            ZustandAktualisierenEnum::GehaltenAktualisieren(None),
                        ));
                        event_status = event::Status::Captured;
                    }
                }
            },
            Event::Mouse(mouse::Event::CursorMoved { position: _ }) => {
                if let Some(canvas_pos) =
                    berechne_canvas_position(&bounds, &cursor, &self.pivot, &self.skalieren)
                {
                    messages.push(Nachricht::from(ZustandAktualisierenEnum::LetzteMausPosition(
                        canvas_pos,
                    )));
                    messages.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenBewegen(
                        canvas_pos,
                    )));
                    event_status = event::Status::Captured
                }
            },
            _otherwise => {},
        };
        if event_status == event::Status::Captured {
            self.canvas.lock().leeren()
        }
        (event_status, Some(messages))
    }

    /// Folge-Method für [update](Gleise::update), in der die notwendigen
    /// Zustands-Änderungen durchgeführt werden.
    pub fn zustand_aktualisieren(
        &mut self,
        nachricht: ZustandAktualisieren,
    ) -> Result<(), GleisIdFehler> {
        match nachricht.0 {
            ZustandAktualisierenEnum::LetzteMausPosition(position) => {
                self.letzte_maus_position = position;
                Ok(())
            },
            ZustandAktualisierenEnum::LetzterKlick(zeitpunkt) => {
                if let ModusDaten::Bauen { letzter_klick, .. } = &mut self.modus {
                    *letzter_klick = zeitpunkt;
                }
                Ok(())
            },
            ZustandAktualisierenEnum::LetzteCanvasGröße(größe) => {
                self.letzte_canvas_größe = größe;
                Ok(())
            },
            ZustandAktualisierenEnum::GehaltenAktualisieren(wert) => {
                if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
                    *gehalten = wert;
                }
                Ok(())
            },
            ZustandAktualisierenEnum::GehaltenBewegen(canvas_pos) => {
                self.gehalten_bewegen(canvas_pos)
            },
            ZustandAktualisierenEnum::GleisEntfernen(gleis_id) => {
                mit_any_id!(gleis_id, Gleise::entfernen_unit, self)
            },
        }
    }
}
