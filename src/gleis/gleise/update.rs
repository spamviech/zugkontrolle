//! [update](iced::widget::canvas::Program::update)-Methode für [Gleise].

use std::{
    sync::mpsc::Sender,
    time::{Duration, Instant},
};

use iced::{
    mouse::{self, Cursor},
    widget::canvas::{event, Event, Program},
    Rectangle, Renderer,
};
use log::error;
use nonempty::{nonempty, NonEmpty};

use crate::{
    application::style::thema::Thema,
    gleis::{
        gleise::{
            self,
            daten::{BewegenFehler, EntfernenFehler, Zustand},
            id::AnyIdSteuerung,
            nachricht::{Gehalten, Nachricht, ZustandAktualisieren, ZustandAktualisierenEnum},
            steuerung::Steuerung,
            Gleise, ModusDaten,
        },
        weiche,
    },
    steuerung::{
        geschwindigkeit::Leiter,
        plan::{AktionSchalten, AktionStreckenabschnitt, AnyAktionSchalten},
        streckenabschnitt::Streckenabschnitt,
    },
    typen::{canvas::Position, skalar::Skalar, vektor::Vektor, winkel::Winkel},
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

/// Aktion für ein im Modus "Bauen" angeklicktes Gleis.
fn aktion_bauen(
    nachrichten: &mut Vec<Nachricht>,
    gleis_steuerung: AnyIdSteuerung,
    now: Instant,
    letzter_klick: Instant,
    halte_position: Vektor,
    winkel: Winkel,
) {
    let diff = now.checked_duration_since(letzter_klick).unwrap_or(Duration::MAX);
    let gleis_steuerung_serialisiert = gleis_steuerung.serialisiere();
    if diff < DOUBLE_CLICK_TIME {
        nachrichten.push(Nachricht::AnschlüsseAnpassen(gleis_steuerung_serialisiert));
        nachrichten.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenAktualisieren(None)))
    } else {
        nachrichten.push(Nachricht::from(ZustandAktualisierenEnum::GehaltenAktualisieren(Some(
            Gehalten { gleis_steuerung, halte_position, winkel, bewegt: false },
        ))));
    }
}

/// Aktion für ein im Modus "Fahren" angeklicktes Gleis.
fn aktion_fahren<AktualisierenNachricht>(
    gleis_steuerung: AnyIdSteuerung,
    streckenabschnitt: Option<&Streckenabschnitt>,
    sender: Sender<AktualisierenNachricht>,
) -> Option<Nachricht>
where
    AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
{
    use AnyIdSteuerung::*;
    match gleis_steuerung {
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
        Weiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::*;
            let richtung = match steuerung.richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        KurvenWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::kurve::Richtung::*;
            let richtung = match steuerung.richtung() {
                Innen => Außen,
                Außen => Innen,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteKurve(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        DreiwegeWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::dreiwege::{Richtung::*, RichtungInformation};
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
        SKurvenWeiche(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::*;
            let richtung = match steuerung.richtung() {
                Gerade => Kurve,
                Kurve => Gerade,
            };
            Nachricht::WeicheSchalten(AnyAktionSchalten::SchalteGerade(AktionSchalten {
                weiche: Steuerung::neu(steuerung.clone(), (sender, AktualisierenNachricht::from)),
                richtung,
            }))
        }),
        Kreuzung(_id, steuerung) => steuerung.as_ref().map(|steuerung| {
            use weiche::gerade::Richtung::*;
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

fn aktion_gleis_an_position<'t, L, AktualisierenNachricht>(
    bounds: Rectangle,
    cursor: &'t Cursor,
    modus: &'t ModusDaten,
    zustand2: &Zustand<L>,
    pivot: &'t Position,
    skalieren: &'t Skalar,
    sender: &Sender<AktualisierenNachricht>,
) -> (event::Status, Vec<Nachricht>)
where
    L: Leiter,
    AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
{
    let mut nachrichten = Vec::new();
    let mut status = event::Status::Ignored;
    if cursor.is_over(bounds) {
        if let Some(canvas_pos) = berechne_canvas_position(&bounds, &cursor, pivot, skalieren) {
            let gleis_an_position2 = zustand2.gleis_an_position(canvas_pos);
            match modus {
                ModusDaten::Bauen { gehalten: gehalten2, letzter_klick } => {
                    let now = Instant::now();
                    nachrichten.push(Nachricht::from(ZustandAktualisierenEnum::LetzterKlick(now)));
                    if gehalten2.is_none() {
                        if let Some((gleis_steuerung, halte_position, winkel, _streckenabschnitt)) =
                            gleis_an_position2
                        {
                            aktion_bauen(
                                &mut nachrichten,
                                gleis_steuerung,
                                now,
                                *letzter_klick,
                                halte_position,
                                winkel,
                            );
                            status = event::Status::Captured;
                        }
                    }
                },
                ModusDaten::Fahren => {
                    if let Some((id_steuerung, _halte_position, _winkel, streckenabschnitt)) =
                        gleis_an_position2
                    {
                        let nachricht = aktion_fahren(
                            id_steuerung,
                            streckenabschnitt.map(
                                |(_name, streckenabschnitt, _geschwindigkeit)| streckenabschnitt,
                            ),
                            sender.clone(),
                        );

                        if let Some(nachricht) = nachricht {
                            nachrichten.push(nachricht);
                            status = event::Status::Captured
                        }
                    }
                },
            }
        }
    }
    (status, nachrichten)
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// [update](iced::widget::canvas::Program::update)-Methode für [Gleise]
    pub fn update(
        &self,
        _state: &mut <Self as Program<NonEmpty<Nachricht>, Renderer<Thema>>>::State,
        event: Event,
        bounds: Rectangle,
        cursor: Cursor,
    ) -> (event::Status, Option<NonEmpty<Nachricht>>)
    where
        AktualisierenNachricht: 'static + From<gleise::steuerung::Aktualisieren> + Send,
    {
        let mut event_status = event::Status::Ignored;
        let mut messages =
            nonempty![Nachricht::from(ZustandAktualisierenEnum::LetzteCanvasGröße(Vektor {
                x: Skalar(bounds.width),
                y: Skalar(bounds.height),
            }))];
        match event {
            Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                let Gleise { zustand: zustand2, pivot, skalieren, modus, .. } = self;
                let (status, nachrichten) = aktion_gleis_an_position(
                    bounds,
                    &cursor,
                    modus,
                    zustand2,
                    pivot,
                    skalieren,
                    &self.sender,
                );
                event_status = status;
                messages.extend(nachrichten);
            },
            Event::Mouse(mouse::Event::ButtonReleased(mouse::Button::Left)) => {
                if let ModusDaten::Bauen { gehalten: gehalten2, .. } = &self.modus {
                    if let Some(Gehalten { gleis_steuerung, bewegt, .. }) = gehalten2 {
                        let gleis_id = gleis_steuerung.id();
                        if *bewegt {
                            if !cursor.is_over(bounds) {
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
            self.canvas.leeren();
        }
        (event_status, Some(messages))
    }
}

/// Fehler, die bei [zustand_aktualisieren](Gleise::zustand_aktualisieren) auftreten können.
#[derive(Debug, Clone, zugkontrolle_macros::From)]
pub enum AktualisierenFehler {
    /// Fehler beim Bewegen eines Gleises.
    BewegenFehler(BewegenFehler),
    /// Fehler beim Entfernen eines Gleises.
    EntfernenFehler(EntfernenFehler),
}

impl<L: Leiter, AktualisierenNachricht> Gleise<L, AktualisierenNachricht> {
    /// Folge-Method für [update](Gleise::update), in der die notwendigen
    /// Zustands-Änderungen durchgeführt werden.
    pub fn zustand_aktualisieren(
        &mut self,
        nachricht: ZustandAktualisieren,
    ) -> Result<(), AktualisierenFehler> {
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
                if let ModusDaten::Bauen { gehalten: gehalten2, .. } = &mut self.modus {
                    *gehalten2 = wert;
                }
                Ok(())
            },
            ZustandAktualisierenEnum::GehaltenBewegen(canvas_pos) => {
                let _ = self.gehalten_bewegen(canvas_pos)?;
                Ok(())
            },
            ZustandAktualisierenEnum::GleisEntfernen(gleis_id) => {
                let _id = self.entfernen(gleis_id)?;
                Ok(())
            },
        }
    }
}
