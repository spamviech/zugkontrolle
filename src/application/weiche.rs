//! Einstellen der Steuerung einer Weiche.

use std::fmt::{Debug, Display};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button, column, container, event, radio, row, text, text_input, Button, Clipboard, Column,
    Element, Event, Layout, Length, Point, Renderer, Row, Text, TextInput, Widget,
};

use crate::{
    anschluss::OutputSave,
    application::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar},
    lookup::Lookup,
    steuerung::weiche::{Name, Weiche},
};

#[derive(Debug, Clone)]
pub struct Status<AnschlüsseSave, AnschlüsseAuswahlStatus> {
    name: String,
    name_state: text_input::State,
    anschlüsse_save: AnschlüsseSave,
    anschlüsse_state: AnschlüsseAuswahlStatus,
    festlegen_state: button::State,
}

impl<AnschlüsseSave, AnschlüsseAuswahlStatus> Status<AnschlüsseSave, AnschlüsseAuswahlStatus>
where
    AnschlüsseSave: Default + Clone + Into<AnschlüsseAuswahlStatus>,
{
    pub fn neu<Richtung>(option_weiche: Option<Weiche<Richtung, AnschlüsseSave>>) -> Self {
        let (name, anschlüsse_save) = if let Some(Weiche { name, anschlüsse, .. }) = option_weiche
        {
            (name.0, anschlüsse)
        } else {
            (String::new(), Default::default())
        };
        let anschlüsse_state = anschlüsse_save.clone().into();
        Status {
            name,
            name_state: text_input::State::new(),
            anschlüsse_save,
            anschlüsse_state,
            festlegen_state: button::State::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum InterneNachricht<Richtung> {
    Name(String),
    Anschluss(Richtung, OutputSave),
    Festlegen,
    Schließen,
}

pub struct Auswahl<'t, Richtung, AnschlüsseSave, R: card::Renderer> {
    card: Card<'t, InterneNachricht<Richtung>, R>,
    name: &'t mut String,
    anschlüsse: &'t mut AnschlüsseSave,
}

impl<'t, Richtung, AnschlüsseSave, R> Auswahl<'t, Richtung, AnschlüsseSave, R>
where
    Richtung: 'static + Clone + Display,
    AnschlüsseSave: Lookup<Richtung, OutputSave>,
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
        status: &'t mut Status<AnschlüsseSave, AnschlüsseAuswahlStatus>,
    ) -> Self {
        let Status { name, name_state, anschlüsse_save, anschlüsse_state, festlegen_state } =
            status;
        let mut column = Column::new().push(
            TextInput::new(name_state, "<Name>", name, InterneNachricht::Name)
                .width(Length::Units(200)),
        );
        for (richtung, anschluss_status) in anschlüsse_state.mut_refs().into_iter() {
            column = column.push(Row::new().push(Text::new(format!("{}", richtung))).push(
                Element::from(anschluss::Auswahl::neu_output(anschluss_status)).map(
                    move |anschluss_save| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_save)
                    },
                ),
            ))
        }
        column = column.push(
            Button::new(festlegen_state, Text::new("Festlegen"))
                .on_press(InterneNachricht::Festlegen),
        );
        let card = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        Auswahl { card, name, anschlüsse: anschlüsse_save }
    }
}

#[derive(Debug, Clone)]
pub enum Nachricht<Richtung, AnschlüsseSave> {
    Festlegen(Weiche<Richtung, AnschlüsseSave>),
    Schließen,
}

impl<'t, Richtung, AnschlüsseSave, R> Widget<Nachricht<Richtung, AnschlüsseSave>, R>
    for Auswahl<'t, Richtung, AnschlüsseSave, R>
where
    Richtung: Clone + Default,
    AnschlüsseSave: Clone + Lookup<Richtung, OutputSave>,
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
        messages: &mut Vec<Nachricht<Richtung, AnschlüsseSave>>,
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
                InterneNachricht::Festlegen => messages.push(Nachricht::Festlegen(Weiche {
                    name: Name(self.name.clone()),
                    aktuelle_richtung: Default::default(),
                    letzte_richtung: Default::default(),
                    anschlüsse: self.anschlüsse.clone(),
                })),
                InterneNachricht::Schließen => messages.push(Nachricht::Schließen),
            }
        }
        status
    }
}

impl<'t, Richtung, AnschlüsseSave, R> From<Auswahl<'t, Richtung, AnschlüsseSave, R>>
    for Element<'t, Nachricht<Richtung, AnschlüsseSave>, R>
where
    Richtung: 't + Clone + Default,
    AnschlüsseSave: Clone + Lookup<Richtung, OutputSave>,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSave, R>) -> Self {
        Element::new(anzeige)
    }
}
