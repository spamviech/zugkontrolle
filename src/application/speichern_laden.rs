//! Pfadauswahl mit Speichern und Laden Knopf.

use std::fmt::Debug;

use iced_native::{
    event, overlay, text, Alignment, Clipboard, Event, Layout, Length, Point, Shell,
};
use iced_pure::{
    widget::{
        tree::{self, Tag, Tree},
        Button, Column, Row, Text, TextInput,
    },
    Element, Widget,
};

use crate::application::{macros::widget_newtype_methods, style::hintergrund};

/// Zustand von [SpeichernLaden].
#[derive(Debug)]
struct Zustand {
    speichern_gefärbt: bool,
    aktueller_pfad: String,
}

impl Zustand {
    /// Erstelle einen neuen Zustand von [SpeichernLaden].
    fn neu(aktueller_pfad: String) -> Self {
        Zustand { speichern_gefärbt: false, aktueller_pfad }
    }

    /// Bestimme, ob der Speichern-Knopf gefärbt wird.
    fn färbe_speichern(&mut self, färben: bool) {
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
    element: Element<'a, InterneNachricht, R>,
    initialer_pfad: &'a str,
}

impl<R> Debug for SpeichernLaden<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SpeichernLaden")
            .field("element", &"<Element>")
            .field("initialer_pfad", &self.initialer_pfad)
            .finish()
    }
}

impl<'a, R: 'a + text::Renderer> SpeichernLaden<'a, R> {
    /// Erstelle ein neuen [SpeichernLaden]-Widget.
    pub fn neu(initialer_pfad: &'a str) -> Self {
        SpeichernLaden {
            element: Self::erzeuge_element(&Zustand::neu(initialer_pfad.to_owned())),
            initialer_pfad,
        }
    }
    fn erzeuge_element(zustand: &'a Zustand) -> Element<'a, InterneNachricht, R> {
        let Zustand { speichern_gefärbt, aktueller_pfad } = zustand;

        let speichern_ungefärbt =
            Button::new(Text::new("Speichern")).on_press(InterneNachricht::Speichern);
        let speichern_style =
            if *speichern_gefärbt { hintergrund::GRÜN } else { hintergrund::STANDARD };
        let row = Row::new()
            .push(
                Column::new()
                    .push(speichern_ungefärbt.style(speichern_style))
                    .push(
                        Button::new(Text::new("Laden"))
                            .style(hintergrund::STANDARD)
                            .on_press(InterneNachricht::Laden),
                    )
                    .align_items(Alignment::End),
            )
            .push(
                TextInput::new("Pfad", aktueller_pfad, InterneNachricht::Pfad)
                    .width(Length::Units(150))
                    .padding(1),
            )
            .spacing(5)
            .align_items(Alignment::Center)
            .width(Length::Shrink);
        row.into()
    }
}

impl<R: text::Renderer> Widget<Nachricht, R> for SpeichernLaden<'_, R> {
    widget_newtype_methods! {element, R}

    fn tag(&self) -> Tag {
        Tag::of::<Zustand>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(Zustand::neu(self.initialer_pfad.to_owned()))
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
    ) -> event::Status {
        let mut row_messages = Vec::new();
        let mut row_shell = Shell::new(&mut row_messages);
        let mut status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut row_shell,
        );
        if row_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if row_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }

        let zustand: &mut Zustand = state.state.downcast_mut();
        for message in row_messages {
            match message {
                InterneNachricht::Speichern => {
                    shell.publish(Nachricht::Speichern(zustand.aktueller_pfad.clone()))
                },
                InterneNachricht::Laden => {
                    shell.publish(Nachricht::Laden(zustand.aktueller_pfad.clone()))
                },
                InterneNachricht::Pfad(pfad) => {
                    zustand.aktueller_pfad = pfad;
                    self.element = Self::erzeuge_element(zustand);
                },
            }
            status = event::Status::Captured;
        }

        status
    }

    fn overlay<'a>(
        &'a self,
        _state: &'a mut Tree,
        _layout: Layout<'_>,
        _renderer: &R,
    ) -> Option<overlay::Element<'a, Nachricht, R>> {
        //TODO
        None
    }
}

impl<'a, R: 'a + text::Renderer> From<SpeichernLaden<'a, R>> for Element<'a, Nachricht, R> {
    fn from(auswahl: SpeichernLaden<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
