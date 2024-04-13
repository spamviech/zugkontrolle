//! Pfadauswahl mit Speichern und Laden Knopf.

use std::fmt::Debug;

use iced_core::{
    event, text as text_core,
    widget::text::{self, Text},
    Alignment, Element, Length,
};
use iced_widget::{
    button::{self, Button},
    text_input::{self, TextInput},
    Column, Row,
};

use crate::{map_mit_zustand::MapMitZustand, style};

/// Zustand von [`SpeichernLaden`].
#[derive(Debug, Default, PartialEq, Eq)]
struct Zustand {
    /// Der aktuell gewählte Pfad.
    aktueller_pfad: String,
}

impl Zustand {
    /// Erstelle einen neuen Zustand von [`SpeichernLaden`].
    fn neu(aktueller_pfad: String) -> Self {
        Zustand { aktueller_pfad }
    }
}

/// Die interne Nachricht zur Interaktion mit einem [`SpeichernLaden`]-Widget.
#[derive(Debug, Clone)]
enum InterneNachricht {
    /// Speichern gewünscht.
    Speichern,
    /// Laden gewünscht.
    Laden,
    /// Neuer aktuell gewählter Pfad.
    Pfad(String),
}

/// Nachricht des [`SpeichernLaden`]-Widgets.
#[derive(Debug, Clone)]
pub enum Nachricht {
    /// Speichern im gegebenen Pfad gewünscht.
    Speichern(String),
    /// Laden aus dem gegebenen Pfad gewünscht.
    Laden(String),
}

/// Widget mit Pfadauswahl und Knöpfen zum Speichern und Laden.
#[derive(Debug)]
pub struct SpeichernLaden<'a, Thema, R>(
    MapMitZustand<'a, Zustand, InterneNachricht, Nachricht, Thema, R>,
);

impl<'a, Thema, R> SpeichernLaden<'a, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a + button::StyleSheet + text::StyleSheet + text_input::StyleSheet,
    <Thema as button::StyleSheet>::Style: From<style::Button>,
{
    /// Erstelle ein [`SpeichernLaden`]-Widget.
    #[must_use]
    pub fn neu(initialer_pfad: &'a str, speichern_gefärbt: Option<bool>) -> Self {
        let erzeuge_element =
            move |zustand: &Zustand| Self::erzeuge_element(zustand, speichern_gefärbt);
        let mapper = |interne_nachricht, zustand: &mut Zustand, status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Speichern => {
                    vec![Nachricht::Speichern(zustand.aktueller_pfad.clone())]
                },
                InterneNachricht::Laden => {
                    vec![Nachricht::Laden(zustand.aktueller_pfad.clone())]
                },
                InterneNachricht::Pfad(pfad) => {
                    zustand.aktueller_pfad = pfad;
                    Vec::new()
                },
            }
        };
        SpeichernLaden(MapMitZustand::neu(
            Zustand::neu(String::from(initialer_pfad)),
            erzeuge_element,
            mapper,
        ))
    }

    /// Erzeuge die Widget-Hierarchie für ein [`SpeichernLaden`]-Widget.
    fn erzeuge_element(
        zustand: &Zustand,
        speichern_gefärbt: Option<bool>,
    ) -> Element<'a, InterneNachricht, Thema, R> {
        let Zustand { aktueller_pfad } = zustand;

        let speichern_ungefärbt =
            Button::new(Text::new("Speichern")).on_press(InterneNachricht::Speichern);
        let speichern_style = match speichern_gefärbt {
            Some(true) => style::button::GRÜN,
            Some(false) => style::button::ROT,
            None => style::Button::Standard,
        };
        let row = Row::new()
            .push(
                Column::new()
                    .push(speichern_ungefärbt.style(speichern_style))
                    .push(
                        Button::new(Text::new("Laden"))
                            .style(style::Button::Standard)
                            .on_press(InterneNachricht::Laden),
                    )
                    .align_items(Alignment::End),
            )
            .push(
                TextInput::new("Pfad", aktueller_pfad)
                    .on_input(InterneNachricht::Pfad)
                    .width(Length::Fixed(150.))
                    .padding(1),
            )
            .spacing(5)
            .align_items(Alignment::Center)
            .width(Length::Shrink);
        row.into()
    }
}

impl<'a, Thema, R> From<SpeichernLaden<'a, Thema, R>> for Element<'a, Nachricht, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a + button::StyleSheet + text::StyleSheet + text_input::StyleSheet,
    <Thema as button::StyleSheet>::Style: From<style::Button>,
{
    fn from(auswahl: SpeichernLaden<'a, Thema, R>) -> Self {
        Element::from(auswahl.0)
    }
}
