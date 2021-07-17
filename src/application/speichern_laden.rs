//! Pfadauswahl mit Speichern und Laden Knopf

use iced_native::{
    button, column, event, row, text, text_input, Align, Button, Clipboard, Column, Element, Event,
    Layout, Length, Point, Renderer, Row, Text, TextInput, Widget,
};

use super::{background, macros::reexport_no_event_methods};

#[derive(Debug)]
pub struct Status {
    speichern: button::State,
    speichern_gefärbt: bool,
    laden: button::State,
    pfad: text_input::State,
    aktueller_pfad: String,
}

impl Status {
    pub fn neu(aktueller_pfad: String) -> Self {
        Status {
            speichern: button::State::new(),
            speichern_gefärbt: false,
            laden: button::State::new(),
            pfad: text_input::State::new(),
            aktueller_pfad,
        }
    }

    pub fn färbe_speichern(&mut self, färben: bool) {
        self.speichern_gefärbt = färben;
    }
}

#[derive(Debug, Clone)]
enum InterneNachricht {
    Speichern,
    Laden,
    Pfad(String),
}

#[derive(Debug, Clone)]
pub enum Nachricht {
    Speichern(String),
    Laden(String),
}

pub struct SpeichernLaden<'a, R> {
    row: Row<'a, InterneNachricht, R>,
    aktueller_pfad: &'a mut String,
}

impl<'a, R> SpeichernLaden<'a, R>
where
    R: 'a + column::Renderer + text::Renderer + button::Renderer + text_input::Renderer,
    <R as button::Renderer>::Style: From<background::Background>,
{
    pub fn neu(status: &'a mut Status) -> Self {
        let Status { speichern, speichern_gefärbt, laden, pfad, aktueller_pfad } = status;

        let speichern_ungefärbt =
            Button::new(speichern, Text::new("speichern")).on_press(InterneNachricht::Speichern);
        let speichern_style =
            if *speichern_gefärbt { background::GREEN } else { background::DEFAULT };
        let row = Row::new()
            .push(
                Column::new()
                    .push(speichern_ungefärbt.style(speichern_style))
                    .push(Button::new(laden, Text::new("laden")).on_press(InterneNachricht::Laden))
                    .align_items(Align::End),
            )
            .push(
                TextInput::new(pfad, "pfad", aktueller_pfad, InterneNachricht::Pfad)
                    .width(Length::Units(250))
                    .padding(1),
            )
            .spacing(5)
            .align_items(Align::Center)
            .width(Length::Shrink);
        SpeichernLaden { row, aktueller_pfad }
    }
}

impl<'a, R: Renderer + row::Renderer> Widget<Nachricht, R> for SpeichernLaden<'a, R> {
    reexport_no_event_methods! {Row<'a, InterneNachricht, R>, row, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Nachricht>,
    ) -> event::Status {
        let mut row_messages = Vec::new();
        let mut status = self.row.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut row_messages,
        );

        for message in row_messages {
            status = event::Status::Captured;

            match message {
                InterneNachricht::Speichern => {
                    messages.push(Nachricht::Speichern(self.aktueller_pfad.clone()))
                }
                InterneNachricht::Laden => {
                    messages.push(Nachricht::Laden(self.aktueller_pfad.clone()))
                }
                InterneNachricht::Pfad(pfad) => *self.aktueller_pfad = pfad,
            }
        }

        status
    }
}

impl<'a, R: 'a + row::Renderer> From<SpeichernLaden<'a, R>> for Element<'a, Nachricht, R> {
    fn from(auswahl: SpeichernLaden<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
