//! Pfadauswahl mit Speichern und Laden Knopf.

use std::fmt::Debug;

use iced_native::{
    event, text,
    widget::{
        button::{self, Button},
        text_input::{self, TextInput},
        Column, Row, Text,
    },
    Alignment, Clipboard, Element, Event, Layout, Length, Point, Renderer, Shell, Widget,
};

use crate::application::{macros::reexport_no_event_methods, style::hintergrund};

/// Zustand von [SpeichernLaden].
#[derive(Debug)]
pub struct Zustand {
    speichern: button::State,
    speichern_gefärbt: bool,
    laden: button::State,
    pfad: text_input::State,
    aktueller_pfad: String,
}

impl Zustand {
    /// Erstelle einen neuen Zustand von [SpeichernLaden].
    pub fn neu(aktueller_pfad: String) -> Self {
        Zustand {
            speichern: button::State::new(),
            speichern_gefärbt: false,
            laden: button::State::new(),
            pfad: text_input::State::new(),
            aktueller_pfad,
        }
    }

    /// Bestimme, ob der Speichern-Knopf gefärbt wird.
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

/// Nachricht des [SpeichernLaden]-Widgets.
#[derive(Debug, Clone)]
pub enum Nachricht {
    /// Speichern im gegebenen Pfad gewünscht.
    Speichern(String),
    /// Laden aus dem gegebenen Pfad gewünscht.
    Laden(String),
}

/// Widget mit Pfadauswahl und Knöpfen zum Speichern und Laden.
pub struct SpeichernLaden<'a, R> {
    row: Row<'a, InterneNachricht, R>,
    aktueller_pfad: &'a mut String,
}

impl<R> Debug for SpeichernLaden<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SpeichernLaden")
            .field("row", &"<Row>")
            .field("aktueller_pfad", &self.aktueller_pfad)
            .finish()
    }
}

impl<'a, R: 'a + text::Renderer> SpeichernLaden<'a, R> {
    /// Erstelle ein neuen [SpeichernLaden]-Widget.
    pub fn neu(zustand: &'a mut Zustand) -> Self {
        let Zustand { speichern, speichern_gefärbt, laden, pfad, aktueller_pfad } = zustand;

        let speichern_ungefärbt =
            Button::new(speichern, Text::new("Speichern")).on_press(InterneNachricht::Speichern);
        let speichern_style =
            if *speichern_gefärbt { hintergrund::GRÜN } else { hintergrund::STANDARD };
        let row = Row::new()
            .push(
                Column::new()
                    .push(speichern_ungefärbt.style(speichern_style))
                    .push(
                        Button::new(laden, Text::new("Laden"))
                            .style(hintergrund::STANDARD)
                            .on_press(InterneNachricht::Laden),
                    )
                    .align_items(Alignment::End),
            )
            .push(
                TextInput::new(pfad, "Pfad", aktueller_pfad, InterneNachricht::Pfad)
                    .width(Length::Units(200))
                    .padding(1),
            )
            .spacing(5)
            .align_items(Alignment::Center)
            .width(Length::Shrink);
        SpeichernLaden { row, aktueller_pfad }
    }
}

impl<'a, R: Renderer> Widget<Nachricht, R> for SpeichernLaden<'a, R> {
    reexport_no_event_methods! {Row<'a, InterneNachricht, R>, row, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
    ) -> event::Status {
        let mut row_messages = Vec::new();
        let mut row_shell = Shell::new(&mut row_messages);
        let mut status =
            self.row.on_event(event, layout, cursor_position, renderer, clipboard, &mut row_shell);
        if row_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            row_shell.revalidate_layout(|| shell.invalidate_layout())
        }

        for message in row_messages {
            status = event::Status::Captured;

            match message {
                InterneNachricht::Speichern => {
                    shell.publish(Nachricht::Speichern(self.aktueller_pfad.clone()))
                },
                InterneNachricht::Laden => {
                    shell.publish(Nachricht::Laden(self.aktueller_pfad.clone()))
                },
                InterneNachricht::Pfad(pfad) => *self.aktueller_pfad = pfad,
            }
        }

        status
    }
}

impl<'a, R: 'a + Renderer> From<SpeichernLaden<'a, R>> for Element<'a, Nachricht, R> {
    fn from(auswahl: SpeichernLaden<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
