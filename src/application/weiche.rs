//! Einstellen der Steuerung einer [Weiche](crate::steuerung::Weiche).

use std::{
    fmt::{self, Debug, Display, Formatter},
    sync::Arc,
};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button, column, container, event, radio, row, text, text_input, Button, Clipboard, Column,
    Element, Event, Layout, Length, Point, Renderer, Row, Text, TextInput, Widget,
};
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{Reserviere, Serialisiere},
        Lager, OutputSerialisiert,
    },
    application::{
        anschluss, macros::reexport_no_event_methods, steuerung::Steuerung, style::tab_bar::TabBar,
    },
    nachschlagen::Nachschlagen,
    steuerung::weiche::{Name, Weiche, WeicheSerialisiert},
};

/// Zustand eines Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
#[derive(Debug, Clone)]
pub struct Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand> {
    name: String,
    name_zustand: text_input::State,
    anschlüsse_serialisiert: AnschlüsseSerialisiert,
    anschlüsse_zustand: AnschlüsseAuswahlZustand,
    festlegen_zustand: button::State,
    entfernen_zustand: button::State,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>
    Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>
where
    AnschlüsseSerialisiert: Default + Clone + Into<AnschlüsseAuswahlZustand>,
{
    /// Erstelle einen neuen [Zustand], potentiell mit voreingestellten Anschlüssen.
    pub fn neu<Anschlüsse, Richtung>(
        option_weiche: Option<WeicheSerialisiert<Richtung, Anschlüsse>>,
    ) -> Self
    where
        Anschlüsse: Serialisiere<Serialisiert = AnschlüsseSerialisiert>,
    {
        let (name, anschlüsse_serialisiert, hat_steuerung) =
            if let Some(WeicheSerialisiert { name, steuerung }) = option_weiche {
                (name.0, steuerung.anschlüsse, true)
            } else {
                (String::new(), AnschlüsseSerialisiert::default(), false)
            };
        let anschlüsse_zustand = anschlüsse_serialisiert.clone().into();
        Zustand {
            name,
            name_zustand: text_input::State::new(),
            anschlüsse_serialisiert,
            anschlüsse_zustand,
            festlegen_zustand: button::State::new(),
            entfernen_zustand: button::State::new(),
            hat_steuerung,
        }
    }
}

#[derive(Debug, Clone)]
enum InterneNachricht<Richtung> {
    Name(String),
    Anschluss(Richtung, OutputSerialisiert),
    Festlegen,
    Entfernen,
    Schließen,
}

/// Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
pub struct Auswahl<'t, Richtung, Anschlüsse: Serialisiere, R: card::Renderer> {
    card: Card<'t, InterneNachricht<Richtung>, R>,
    name: &'t mut String,
    anschlüsse: &'t mut <Anschlüsse as Serialisiere>::Serialisiert,
    mutex: &'t mut Steuerung<Arc<Mutex<Option<Weiche<Richtung, Anschlüsse>>>>>,
    lager: &'t mut Lager,
}

impl<Richtung, Anschlüsse: Serialisiere, R> Debug for Auswahl<'_, Richtung, Anschlüsse, R>
where
    Richtung: Debug,
    Anschlüsse: Debug,
    <Anschlüsse as Serialisiere>::Serialisiert: Debug,
    R: card::Renderer,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("card", &"<Card>")
            .field("name", &self.name)
            .field("anschlüsse", &self.anschlüsse)
            .field("mutex", &self.mutex)
            .field("lager", &self.lager)
            .finish()
    }
}

impl<'t, Richtung, Anschlüsse, AnschlüsseSerialisiert, R> Auswahl<'t, Richtung, Anschlüsse, R>
where
    Richtung: 'static + Clone + Display,
    Anschlüsse: Serialisiere<Serialisiert = AnschlüsseSerialisiert>,
    AnschlüsseSerialisiert: Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't
        + Renderer
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + text::Renderer
        + button::Renderer
        + text_input::Renderer
        + radio::Renderer
        + card::Renderer
        + tabs::Renderer
        + number_input::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<AnschlüsseAuswahlZustand>(
        zustand: &'t mut Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>,
        mutex: &'t mut Steuerung<Arc<Mutex<Option<Weiche<Richtung, Anschlüsse>>>>>,
        lager: &'t mut Lager,
    ) -> Self
    where
        AnschlüsseAuswahlZustand: Nachschlagen<Richtung, anschluss::Zustand<anschluss::Output>>,
    {
        let Zustand {
            name,
            name_zustand,
            anschlüsse_serialisiert,
            anschlüsse_zustand,
            festlegen_zustand,
            entfernen_zustand,
            hat_steuerung,
        } = zustand;
        let mut column = Column::new().push(
            TextInput::new(name_zustand, "<Name>", name, InterneNachricht::Name)
                .width(Length::Units(200)),
        );
        for (richtung, anschluss_zustand) in anschlüsse_zustand.referenzen_mut().into_iter() {
            column = column.push(Row::new().push(Text::new(format!("{}", richtung))).push(
                Element::from(anschluss::Auswahl::neu_output(anschluss_zustand)).map(
                    move |anschluss_save| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_save)
                    },
                ),
            ))
        }
        column = column.push(
            Row::new()
                .push(
                    Button::new(festlegen_zustand, Text::new("Festlegen"))
                        .on_press(InterneNachricht::Festlegen),
                )
                .push(
                    Button::new(
                        entfernen_zustand,
                        Text::new(if *hat_steuerung { "Entfernen" } else { "Keine Anschlüsse" }),
                    )
                    .on_press(InterneNachricht::Entfernen),
                ),
        );
        let card = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        Auswahl { card, name, anschlüsse: anschlüsse_serialisiert, mutex, lager }
    }
}

/// Nachricht einer [Auswahl].
#[derive(Debug, Clone)]
pub enum Nachricht {
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
    /// Ein Fehler beim [Reservieren](Reserviere::reserviere) der Weiche.
    ReservierenFehler {
        /// Titel der Fehlermeldung.
        titel: String,
        /// Nachricht der Fehlermeldung
        nachricht: String,
    },
}

#[allow(single_use_lifetimes)]
impl<'t, Richtung, Anschlüsse, AnschlüsseSerialisiert, R> Widget<Nachricht, R>
    for Auswahl<'t, Richtung, Anschlüsse, R>
where
    Richtung: Clone + PartialEq + Default + Serialize + for<'de> Deserialize<'de>,
    Anschlüsse: Serialisiere<Serialisiert = AnschlüsseSerialisiert>,
    AnschlüsseSerialisiert: Clone + PartialEq + Nachschlagen<Richtung, OutputSerialisiert>,
    R: Renderer + card::Renderer,
{
    reexport_no_event_methods! {
        Card<'t, InterneNachricht<Richtung>, R>,
        card,
        InterneNachricht<Richtung>,
        R
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Nachricht>,
    ) -> event::Status {
        let mut card_messages = Vec::new();
        let mut status = self.card.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut card_messages,
        );
        for message in card_messages {
            status = event::Status::Captured;
            match message {
                InterneNachricht::Name(name) => *self.name = name,
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    *self.anschlüsse.erhalte_mut(&richtung) = anschluss
                },
                InterneNachricht::Festlegen => {
                    let weiche_serialisiert: WeicheSerialisiert<Richtung, Anschlüsse> =
                        WeicheSerialisiert::neu(
                            Name(self.name.clone()),
                            Richtung::default(),
                            Richtung::default(),
                            self.anschlüsse.clone(),
                        );
                    let mut_ref = &mut *self.mutex.as_mut().lock();
                    if mut_ref.serialisiere().as_ref() != Some(&weiche_serialisiert) {
                        let bisher = mut_ref.take();
                        let (pwm_pins, output_anschlüsse, input_anschlüsse) = bisher.anschlüsse();
                        match weiche_serialisiert.reserviere(
                            &mut self.lager,
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse,
                        ) {
                            Ok(weiche) => {
                                *mut_ref = Some(weiche.anschluss);
                                messages.push(Nachricht::Schließen);
                            },
                            Err(fehler) => messages.push(Nachricht::ReservierenFehler {
                                titel: format!("Anschlüsse für Weiche {} reservieren.", self.name),
                                nachricht: format!("{fehler:?}"),
                            }),
                        }
                    }
                },
                InterneNachricht::Entfernen => {
                    *self.mutex.as_mut().lock() = None;
                    messages.push(Nachricht::Schließen);
                },
                InterneNachricht::Schließen => messages.push(Nachricht::Schließen),
            }
        }
        status
    }
}

#[allow(single_use_lifetimes)]
impl<'t, Richtung, Anschlüsse, AnschlüsseSerialisiert, R> From<Auswahl<'t, Richtung, Anschlüsse, R>>
    for Element<'t, Nachricht, R>
where
    Richtung: Clone + PartialEq + Default + Serialize + for<'de> Deserialize<'de>,
    Anschlüsse: Serialisiere<Serialisiert = AnschlüsseSerialisiert>,
    AnschlüsseSerialisiert: Clone + PartialEq + Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Richtung, Anschlüsse, R>) -> Self {
        Element::new(anzeige)
    }
}
