//! [update](iced::widget::canvas::Program::update)-Methode für [`Gleise`].

use std::{
    sync::mpsc::Sender,
    time::{Duration, Instant},
};

use either::Either;
use iced::{
    mouse::{self, Cursor},
    touch::{self, Finger},
    widget::canvas::{event, Event, Program},
    Point, Rectangle, Renderer,
};
use log::{debug, error, info, trace, warn};
use nonempty::{nonempty, NonEmpty};

use zugkontrolle_gleis::{
    id::AnyIdSteuerung,
    steuerung::{
        aktualisieren::{Aktualisieren, Steuerung},
        geschwindigkeit::Leiter,
        plan::{AktionSchalten, AktionStreckenabschnitt, AnyAktionSchalten},
        streckenabschnitt::Streckenabschnitt,
    },
    weiche,
};
use zugkontrolle_typen::{canvas::Position, skalar::Skalar, vektor::Vektor, winkel::Winkel};

use crate::{
    daten::{
        AssoziierterStreckenabschnitt, BewegenFehler, EntfernenFehler, GleisAnPosition, Zustand,
    },
    nachricht::{Gehalten, Nachricht, ZustandAktualisieren, ZustandAktualisierenEnum},
    Gleise, KlickQuelle, ModusDaten,
};

/// Position des Cursors auf einem canvas mit `bounds`.
fn berechne_canvas_position(
    bounds: &Rectangle,
    cursor: &Cursor,
    pivot: &Position,
    skalieren: Skalar,
) -> Option<Vektor> {
    // `position_in` gibt nur in-bounds einen `Some`-Wert zurück.
    // `position` hat diese Einschränkung nicht,
    // dafür muss die Position explizit abgezogen werden.
    cursor.position().map(|pos| {
        let relative_position = Vektor { x: Skalar(pos.x - bounds.x), y: Skalar(pos.y - bounds.y) };
        // Wie f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            pivot.punkt + (relative_position / skalieren).rotiert(-pivot.winkel)
        }
    })
}

/// Maximale Zeit, innerhalb der ein zweiter Klick als Doppelklick gewertet wird.
const DOUBLE_CLICK_TIME: Duration = Duration::from_millis(200);

/// Aktion für ein im Modus "Bauen" angeklicktes Gleis.
fn aktion_bauen(
    nachrichten: &mut Vec<Nachricht>,
    gleis_steuerung: AnyIdSteuerung,
    quelle: KlickQuelle,
    now: Instant,
    letzter_klick: &Option<(KlickQuelle, Instant)>,
    halte_position: Vektor,
    winkel: Winkel,
) {
    let diff = letzter_klick
        .as_ref()
        .and_then(|(letzte_quelle, letzte_zeit)| {
            let selbe_klick_art = matches!(
                (letzte_quelle, quelle),
                (KlickQuelle::Maus, KlickQuelle::Maus)
                    | (KlickQuelle::Touch(_), KlickQuelle::Touch(_))
            );
            if selbe_klick_art {
                now.checked_duration_since(*letzte_zeit)
            } else {
                None
            }
        })
        .unwrap_or(Duration::MAX);
    let gleis_steuerung_serialisiert = gleis_steuerung.serialisiere();
    if diff < DOUBLE_CLICK_TIME {
        nachrichten.push(Nachricht::AnschlüsseAnpassen(gleis_steuerung_serialisiert));
    } else {
        nachrichten.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenAktualisieren(
            quelle,
            Some(Gehalten { gleis_steuerung, halte_position, winkel, bewegt: false }),
        )));
    }
}

/// Aktion für ein im Modus "Fahren" angeklicktes Gleis.
fn aktion_fahren<AktualisierenNachricht>(
    gleis_steuerung: AnyIdSteuerung,
    streckenabschnitt: Option<&Streckenabschnitt>,
    sender: Sender<AktualisierenNachricht>,
) -> Option<Nachricht>
where
    AktualisierenNachricht: 'static + From<Aktualisieren> + Send,
{
    use AnyIdSteuerung::{
        DreiwegeWeiche, Gerade, Kreuzung, Kurve, KurvenWeiche, SKurvenWeiche, Weiche,
    };
    match gleis_steuerung {
        // streckenabschnitt related über `map`
        #[allow(clippy::shadow_unrelated)]
        Gerade(_, _) | Kurve(_, _) => streckenabschnitt.map(|streckenabschnitt| {
            let fließend = !streckenabschnitt.fließend();
            Nachricht::StreckenabschnittUmschalten(AktionStreckenabschnitt::Strom {
                streckenabschnitt: Steuerung::neu(
                    streckenabschnitt.clone(),
                    (sender, AktualisierenNachricht::from),
                ),
                fließend,
            })
        }),
        // steuerung related über `map`
        #[allow(clippy::shadow_unrelated)]
        Weiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::{Gerade, Kurve};
            let richtung = match steuerung.richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        // steuerung related über `map`
        #[allow(clippy::shadow_unrelated)]
        KurvenWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::kurve::Richtung::{Außen, Innen};
            let richtung = match steuerung.richtung() {
                Innen => Außen,
                Außen => Innen,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteKurve(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        // steuerung related über `map`
        #[allow(clippy::shadow_unrelated)]
        DreiwegeWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::dreiwege::{
                Richtung::{Gerade, Links, Rechts},
                RichtungInformation,
            };
            let richtung = match steuerung.richtung() {
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Links } => Rechts,
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Rechts } => Links,
                RichtungInformation { aktuelle_richtung: Gerade, letzte_richtung: Gerade } => {
                    error!(
                        "Letzte und aktuelle Richtung für Dreiwege-Weiche {} sind beide Gerade!",
                        steuerung.name.0
                    );
                    Links
                },
                RichtungInformation { aktuelle_richtung: Links | Rechts, .. } => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteDreiwege(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        // steuerung related über `map`
        #[allow(clippy::shadow_unrelated)]
        SKurvenWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::{Gerade, Kurve};
            let richtung = match steuerung.richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        // steuerung related über `map`
        #[allow(clippy::shadow_unrelated)]
        Kreuzung(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::{Gerade, Kurve};
            let richtung = match steuerung.richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
    }
}

/// Führe die Aktion für das Gleis an der Position des `cursor_oder_finger` aus.
fn aktion_gleis_an_position<L, AktualisierenNachricht>(
    bounds: Rectangle,
    cursor_or_finger: Either<Cursor, (Finger, Point)>,
    modus: &ModusDaten,
    zustand: &Zustand<L>,
    pivot: &Position,
    skalieren: Skalar,
    sender: &Sender<AktualisierenNachricht>,
) -> (event::Status, Vec<Nachricht>)
where
    L: Leiter,
    AktualisierenNachricht: 'static + From<Aktualisieren> + Send,
{
    let mut nachrichten = Vec::new();
    let mut status = event::Status::Ignored;
    let (cursor, aktueller_klick) = match cursor_or_finger {
        Either::Left(cursor) => (cursor, KlickQuelle::Maus),
        Either::Right((finger, position)) => {
            (Cursor::Available(position), KlickQuelle::Touch(finger))
        },
    };
    if cursor.is_over(bounds) {
        if let Some(canvas_pos) = berechne_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let gleis_an_position = zustand.gleis_an_position(canvas_pos);
            match modus {
                ModusDaten::Bauen { gehalten: _, letzter_klick } => {
                    let now = Instant::now();
                    nachrichten.push(Nachricht::from(ZustandAktualisierenEnum::LetzterKlick(
                        aktueller_klick,
                        now,
                    )));
                    if let Some(GleisAnPosition {
                        id_steuerung,
                        position: halte_position,
                        winkel,
                        streckenabschnitt: _,
                    }) = gleis_an_position
                    {
                        aktion_bauen(
                            &mut nachrichten,
                            id_steuerung,
                            aktueller_klick,
                            now,
                            letzter_klick,
                            halte_position,
                            winkel,
                        );
                        status = event::Status::Captured;
                    }
                },
                ModusDaten::Fahren => {
                    if let Some(GleisAnPosition {
                        id_steuerung,
                        position: _,
                        winkel: _,
                        streckenabschnitt,
                    }) = gleis_an_position
                    {
                        let nachricht = aktion_fahren(
                            id_steuerung,
                            // streckenabschnitt related über `map`
                            #[allow(clippy::shadow_unrelated)]
                            streckenabschnitt.map(
                                |AssoziierterStreckenabschnitt {
                                     name: _,
                                     streckenabschnitt,
                                     geschwindigkeit: _,
                                 }| streckenabschnitt,
                            ),
                            sender.clone(),
                        );

                        if let Some(nachricht) = nachricht {
                            nachrichten.push(nachricht);
                            status = event::Status::Captured;
                        }
                    }
                },
            }
        }
    }
    (status, nachrichten)
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Behandle ein [`mouse::Event::ButtonPressed`]-, oder [`touch::Event::FingerPressed`]-Event.
    fn maus_oder_touch_pressed(
        &self,
        cursor_oder_finger: Either<Cursor, (Finger, Point)>,
        bounds: Rectangle,
        event_status: &mut event::Status,
        messages: &mut NonEmpty<Nachricht>,
    ) where
        AktualisierenNachricht: 'static + From<Aktualisieren> + Send,
    {
        let Gleise { zustand, pivot, skalieren, modus, sender, .. } = self;
        let (status, nachrichten) = aktion_gleis_an_position(
            bounds,
            cursor_oder_finger,
            modus,
            zustand,
            pivot,
            *skalieren,
            sender,
        );
        *event_status = status;
        messages.extend(nachrichten);
    }

    /// Behandle ein [`mouse::Event::ButtonReleased`]-,
    /// oder [`touch::Event::FingerLifted`]/[`touch::Event::FingerLost`]-Event.
    fn maus_oder_touch_released(
        &self,
        cursor_oder_finger: Either<Cursor, (Finger, Point)>,
        bounds: Rectangle,
        event_status: &mut event::Status,
        messages: &mut NonEmpty<Nachricht>,
    ) {
        let (cursor, quelle) = match cursor_oder_finger {
            Either::Left(cursor) => (cursor, KlickQuelle::Maus),
            Either::Right((finger, position)) => {
                (Cursor::Available(position), KlickQuelle::Touch(finger))
            },
        };
        if let ModusDaten::Bauen { gehalten, .. } = &self.modus {
            if let Some(Gehalten { gleis_steuerung, bewegt, .. }) = gehalten.get(&quelle) {
                let gleis_id = gleis_steuerung.id();
                if *bewegt {
                    if !cursor.is_over(bounds) {
                        messages.push(Nachricht::from(ZustandAktualisierenEnum::GleisEntfernen(
                            gleis_id,
                        )));
                    }
                } else {
                    // setze Streckenabschnitt, falls Maus (von ButtonPressed) nicht bewegt
                    messages.push(Nachricht::SetzeStreckenabschnitt(gleis_id));
                }
                messages.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenAktualisieren(
                    quelle, None,
                )));
                *event_status = event::Status::Captured;
            }
        }
    }

    /// Behandle ein [`mouse::Event::CursorMoved`]-, oder [`touch::Event::FingerMoved`]-Event.
    fn maus_oder_touch_moved(
        &self,
        cursor_oder_finger: Either<Point, (Finger, Point)>,
        bounds: Rectangle,
        event_status: &mut event::Status,
        messages: &mut NonEmpty<Nachricht>,
    ) {
        let (position, quelle) = match cursor_oder_finger {
            Either::Left(position) => (position, KlickQuelle::Maus),
            Either::Right((finger, position)) => (position, KlickQuelle::Touch(finger)),
        };
        if let Some(canvas_pos) = berechne_canvas_position(
            &bounds,
            &Cursor::Available(position),
            &self.pivot,
            self.skalieren,
        ) {
            messages
                .push(Nachricht::from(ZustandAktualisierenEnum::LetzteMausPosition(canvas_pos)));
            if let ModusDaten::Bauen { gehalten, .. } = &self.modus {
                if gehalten.contains_key(&quelle) {
                    messages.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenBewegen(
                        quelle, canvas_pos,
                    )));
                }
            }
            *event_status = event::Status::Captured;
        }
    }

    // TODO Behandeln erfordert Anpassen des public API.
    #[allow(clippy::same_name_method)]
    /// [update](iced::widget::canvas::Program::update)-Methode für [`Gleise`]
    pub fn update<Thema>(
        &self,
        _state: &mut <Self as Program<NonEmpty<Nachricht>, Renderer<Thema>>>::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<NonEmpty<Nachricht>>)
    where
        AktualisierenNachricht: 'static + From<Aktualisieren> + Send,
        Gleise<L, AktualisierenNachricht>: Program<NonEmpty<Nachricht>, Renderer<Thema>>,
    {
        let mut event_status = event::Status::Ignored;
        let mut messages =
            nonempty![Nachricht::from(ZustandAktualisierenEnum::LetzteCanvasGröße(Vektor {
                x: Skalar(bounds.width),
                y: Skalar(bounds.height),
            }))];
        match &event {
            Event::Mouse(_) | Event::Touch(_) => trace!("{event:?}"),
            Event::Keyboard(_) => {},
        }
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => self
                .maus_oder_touch_pressed(
                    Either::Left(cursor),
                    bounds,
                    &mut event_status,
                    &mut messages,
                ),
            Event::Touch(touch::Event::FingerPressed { id, position }) => self
                .maus_oder_touch_pressed(
                    Either::Right((id, position)),
                    bounds,
                    &mut event_status,
                    &mut messages,
                ),
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => self
                .maus_oder_touch_released(
                    Either::Left(cursor),
                    bounds,
                    &mut event_status,
                    &mut messages,
                ),
            Event::Touch(
                touch::Event::FingerLifted { id, position }
                | touch::Event::FingerLost { id, position },
            ) => self.maus_oder_touch_released(
                Either::Right((id, position)),
                bounds,
                &mut event_status,
                &mut messages,
            ),
            Event::Mouse(mouse::Event::CursorMoved { position }) => self.maus_oder_touch_moved(
                Either::Left(position),
                bounds,
                &mut event_status,
                &mut messages,
            ),
            Event::Touch(touch::Event::FingerMoved { id, position }) => self.maus_oder_touch_moved(
                Either::Right((id, position)),
                bounds,
                &mut event_status,
                &mut messages,
            ),
            Event::Mouse(_) | Event::Keyboard(_) => {},
        };
        if event_status == event::Status::Captured {
            self.canvas.leeren();
        }
        (event_status, Some(messages))
    }
}

/// Fehler, die bei [`zustand_aktualisieren`](Gleise::zustand_aktualisieren) auftreten können.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AktualisierenFehler {
    /// Fehler beim Bewegen eines Gleises.
    BewegenFehler(BewegenFehler),
    /// Fehler beim Entfernen eines Gleises.
    EntfernenFehler(EntfernenFehler),
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Folge-Method für [`update`](Gleise::update), in der die notwendigen
    /// Zustands-Änderungen durchgeführt werden.
    ///
    /// ## Errors
    ///
    /// Fehler beim aktualisieren des Zustandes, z.B. bewegen eines gehaltenen Gleises.
    pub fn zustand_aktualisieren(
        &mut self,
        nachricht: ZustandAktualisieren,
    ) -> Result<(), AktualisierenFehler> {
        match nachricht.0 {
            ZustandAktualisierenEnum::LetzteMausPosition(position) => {
                self.letzte_maus_position = position;
                Ok(())
            },
            ZustandAktualisierenEnum::LetzterKlick(quelle, zeitpunkt) => {
                if let ModusDaten::Bauen { letzter_klick, .. } = &mut self.modus {
                    *letzter_klick = Some((quelle, zeitpunkt));
                } else {
                    error!("LetzterKlick-Nachricht im {:?}-Modus!", &self.modus);
                }
                Ok(())
            },
            ZustandAktualisierenEnum::LetzteCanvasGröße(größe) => {
                self.letzte_canvas_größe = größe;
                Ok(())
            },
            ZustandAktualisierenEnum::GehaltenAktualisieren(quelle, wert) => {
                if let ModusDaten::Bauen { gehalten, .. } = &mut self.modus {
                    if let Some(wert) = wert {
                        let bisher = gehalten.insert(quelle, wert);
                        if bisher.is_some() {
                            debug!("Aktualisiere gehaltenes Gleis für {quelle:?}.");
                        } else {
                            info!("Neues gehaltenes Gleis für {quelle:?}.");
                        }
                    } else {
                        let bisher = gehalten.remove(&quelle);
                        if bisher.is_some() {
                            info!("Gehaltenes Gleis für {quelle:?} entfernt.");
                        } else {
                            warn!("Gehaltenes Gleis für {quelle:?} soll entfernt werden, aber ist nicht vorhanden!");
                        }
                    }
                } else {
                    error!("GehaltenAktualisieren-Nachricht im {:?}-Modus!", &self.modus);
                }
                Ok(())
            },
            ZustandAktualisierenEnum::GehaltenBewegen(quelle, canvas_pos) => {
                self.gehalten_bewegen(&quelle, canvas_pos)?;
                Ok(())
            },
            ZustandAktualisierenEnum::GleisEntfernen(gleis_id) => {
                let _id = self.entfernen(gleis_id)?;
                Ok(())
            },
        }
    }
}
