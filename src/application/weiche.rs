//! Einstellen der Steuerung einer Weiche.

use std::fmt::{Debug, Display};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button, column, container, event, radio, row, text, text_input, Button, Clipboard, Column,
    Element, Event, Layout, Length, Point, Renderer, Row, Text, TextInput, Widget,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar},
    lookup::Lookup,
    steuerung::weiche::{Name, WeicheSerialisiert},
};

#[derive(Debug, Clone)]
pub struct Status<AnschlüsseSerialisiert, AnschlüsseAuswahlStatus> {
    name: String,
    name_state: text_input::State,
    anschlüsse_save: AnschlüsseSerialisiert,
    anschlüsse_state: AnschlüsseAuswahlStatus,
    festlegen_state: button::State,
    entfernen_state: button::State,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert, AnschlüsseAuswahlStatus>
    Status<AnschlüsseSerialisiert, AnschlüsseAuswahlStatus>
where
    AnschlüsseSerialisiert: Default + Clone + Into<AnschlüsseAuswahlStatus>,
{
    pub fn neu<Richtung>(
        option_weiche: Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        let (name, anschlüsse_save, hat_steuerung) =
            if let Some(WeicheSerialisiert { name, anschlüsse, .. }) = option_weiche {
                (name.0, anschlüsse, true)
            } else {
                (String::new(), Default::default(), false)
            };
        let anschlüsse_state = anschlüsse_save.clone().into();
        Status {
            name,
            name_state: text_input::State::new(),
            anschlüsse_save,
            anschlüsse_state,
            festlegen_state: button::State::new(),
            entfernen_state: button::State::new(),
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

pub struct Auswahl<'t, Richtung, AnschlüsseSerialisiert, R: card::Renderer> {
    card: Card<'t, InterneNachricht<Richtung>, R>,
    name: &'t mut String,
    anschlüsse: &'t mut AnschlüsseSerialisiert,
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: 'static + Clone + Display,
    AnschlüsseSerialisiert: Lookup<Richtung, OutputSerialisiert>,
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
    pub fn neu<AnschlüsseAuswahlStatus: Lookup<Richtung, anschluss::Status<anschluss::Output>>>(
        status: &'t mut Status<AnschlüsseSerialisiert, AnschlüsseAuswahlStatus>,
    ) -> Self {
        let Status {
            name,
            name_state,
            anschlüsse_save,
            anschlüsse_state,
            festlegen_state,
            entfernen_state,
            hat_steuerung,
        } = status;
        let mut column = Column::new().push(
            TextInput::new(name_state, "<Name>", name, InterneNachricht::Name)
                .width(Length::Units(200)),
        );
        for (richtung, anschluss_status) in anschlüsse_state.refs_mut().into_iter() {
            column = column.push(Row::new().push(Text::new(format!("{}", richtung))).push(
                Element::from(anschluss::Auswahl::neu_output(anschluss_status)).map(
                    move |anschluss_save| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_save)
                    },
                ),
            ))
        }
        column = column.push(
            Row::new()
                .push(
                    Button::new(festlegen_state, Text::new("Festlegen"))
                        .on_press(InterneNachricht::Festlegen),
                )
                .push(
                    Button::new(
                        entfernen_state,
                        Text::new(if *hat_steuerung { "Entfernen" } else { "Keine Anschlüsse" }),
                    )
                    .on_press(InterneNachricht::Entfernen),
                ),
        );
        let card = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        Auswahl { card, name, anschlüsse: anschlüsse_save }
    }
}

#[derive(Debug, Clone)]
pub enum Nachricht<Richtung, AnschlüsseSerialisiert> {
    Festlegen(Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>),
    Schließen,
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> Widget<Nachricht<Richtung, AnschlüsseSerialisiert>, R>
    for Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: Clone + Default,
    AnschlüsseSerialisiert: Clone + Lookup<Richtung, OutputSerialisiert>,
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
        messages: &mut Vec<Nachricht<Richtung, AnschlüsseSerialisiert>>,
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
                    *self.anschlüsse.get_mut(&richtung) = anschluss
                }
                InterneNachricht::Festlegen => {
                    messages.push(Nachricht::Festlegen(Some(WeicheSerialisiert {
                        name: Name(self.name.clone()),
                        aktuelle_richtung: Default::default(),
                        letzte_richtung: Default::default(),
                        anschlüsse: self.anschlüsse.clone(),
                    })))
                }
                InterneNachricht::Entfernen => messages.push(Nachricht::Festlegen(None)),
                InterneNachricht::Schließen => messages.push(Nachricht::Schließen),
            }
        }
        status
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> From<Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<Richtung, AnschlüsseSerialisiert>, R>
where
    Richtung: 't + Clone + Default,
    AnschlüsseSerialisiert: Clone + Lookup<Richtung, OutputSerialisiert>,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>) -> Self {
        Element::new(anzeige)
    }
}
