//! Show all Licenses of dependencies.

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_native::{
    button::{self, Button},
    column::Column,
    container::{self, Container},
    event,
    row::{self, Row},
    rule::{self, Rule},
    scrollable::{self, Scrollable},
    text::{self, Text},
    Clipboard, Element, Event, Layout, Length, Point, Renderer, Widget,
};

use crate::application::{
    macros::reexport_no_event_methods,
    style::{
        hintergrund::{self, Hintergrund},
        linie::{Linie, TRENNLINIE},
    },
};

#[derive(Debug, Clone)]
enum InterneNachricht {
    Aktuell(fn() -> String),
    Schließen,
}

/// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [Lizenzen]-Anzeige.
    Schließen,
}

/// Zustand eines [Lizenzen]-Widgets.
#[derive(Debug)]
pub struct Zustand {
    lizenzen_und_button_states: BTreeMap<&'static str, (button::State, fn() -> String)>,
    scrollable: scrollable::State,
    schließen: button::State,
    aktuell: String,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    pub fn neu(lizenzen: impl Iterator<Item = (&'static str, fn() -> String)>) -> Self {
        let mut aktuell = None;
        let lizenzen_und_button_states = lizenzen
            .map(|(name, f)| {
                if aktuell.is_none() {
                    aktuell = Some(f());
                }
                (name, (button::State::new(), f))
            })
            .collect();
        let aktuell = aktuell.unwrap_or_else(String::new);
        Zustand {
            lizenzen_und_button_states,
            scrollable: scrollable::State::new(),
            schließen: button::State::new(),
            aktuell,
        }
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
pub struct Lizenzen<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneNachricht, R>,
    aktuell: &'a mut String,
}

impl<R: Renderer + container::Renderer> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen").field("row", &"<Row>").finish()
    }
}

impl<'a, R> Lizenzen<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + row::Renderer
        + scrollable::Renderer
        + rule::Renderer
        + button::Renderer
        + text::Renderer,
    <R as rule::Renderer>::Style: From<Linie>,
    <R as container::Renderer>::Style: From<Hintergrund>,
{
    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu(
        zustand: &'a mut Zustand,
        scrollable_style: impl Into<<R as scrollable::Renderer>::Style>,
    ) -> Self {
        let Zustand { lizenzen_und_button_states, scrollable, schließen, aktuell } = zustand;
        let mut scrollable = Scrollable::new(scrollable)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            scrollable = scrollable.push(
                Button::new(button_state, Text::new(name)).on_press(InterneNachricht::Aktuell(*f)),
            );
        }
        let column = Column::new()
            .push(scrollable)
            .push(
                Button::new(schließen, Text::new("Schließen"))
                    .on_press(InterneNachricht::Schließen),
            )
            .width(Length::Shrink)
            .height(Length::Fill);
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(1).style(TRENNLINIE))
                .push(Text::new(aktuell.as_str()).width(Length::Fill).height(Length::Fill)),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, aktuell }
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> Widget<Nachricht, R> for Lizenzen<'a, R> {
    reexport_no_event_methods! {Container<'a, InterneNachricht, R>, container, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        nachrichten: &mut Vec<Nachricht>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let event_status = self.container.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_nachrichten,
        );
        for interne_nachricht in interne_nachrichten {
            match interne_nachricht {
                InterneNachricht::Aktuell(f) => *self.aktuell = f(),
                InterneNachricht::Schließen => nachrichten.push(Nachricht::Schließen),
            }
        }
        event_status
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> From<Lizenzen<'a, R>>
    for Element<'a, Nachricht, R>
{
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}
