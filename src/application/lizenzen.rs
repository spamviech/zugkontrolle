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
use log::error;

use crate::application::{
    macros::reexport_no_event_methods,
    style::{
        hintergrund::{self, Hintergrund},
        linie::{Linie, TRENNLINIE},
    },
};

struct Aktuell<R: text::Renderer> {
    text: Text<R>,
}

impl<R: text::Renderer> Aktuell<R> {
    fn neu(text: impl Into<String>) -> Self {
        Aktuell { text: Text::new(text) }
    }

    fn width(self, length: Length) -> Self {
        Aktuell { text: self.text.width(length) }
    }

    fn height(self, length: Length) -> Self {
        Aktuell { text: self.text.height(length) }
    }
}

impl<R: Renderer + text::Renderer> Widget<InterneNachricht, R> for Aktuell<R> {
    reexport_no_event_methods! {Text<R>, text, InterneNachricht, R}

    #[inline(always)]
    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        nachrichten: &mut Vec<InterneNachricht>,
    ) -> event::Status {
        let mut event_status =
            self.text.on_event(event, layout, cursor_position, renderer, clipboard, nachrichten);
        let mut andere_nachrichten = Vec::new();
        for nachricht in nachrichten.drain(..) {
            println!("{nachricht:?}");
            match nachricht {
                InterneNachricht::Aktuell(f) => {
                    println!("Neuer Text:\n{}", f());
                    self.text = Text::new(f());
                    event_status = event::Status::Captured;
                },
                _ => andere_nachrichten.push(nachricht),
            }
        }
        *nachrichten = andere_nachrichten;
        event_status
    }
}

impl<'a, R: 'a + Renderer + text::Renderer> From<Aktuell<R>> for Element<'a, InterneNachricht, R> {
    fn from(lizenzen: Aktuell<R>) -> Self {
        Element::new(lizenzen)
    }
}

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

/// Auswahl-Fenster für [Streckenabschnitte](Streckenabschnitt).
pub struct Lizenzen<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneNachricht, R>,
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
        lizenzen_und_button_states: &'a mut BTreeMap<&'static str, (button::State, fn() -> String)>,
        scrollable_state: &'a mut scrollable::State,
        scrollable_style: impl Into<<R as scrollable::Renderer>::Style>,
        schließen: &'a mut button::State,
    ) -> Self {
        let mut scrollable = Scrollable::new(scrollable_state)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        let mut aktuell = None;
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            if aktuell.is_none() {
                aktuell = Some(f());
            }
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
        let aktuell = aktuell.unwrap_or_else(String::new);
        // TODO aktuell veränderbar machen
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(1).style(TRENNLINIE))
                .push(Aktuell::neu(aktuell).width(Length::Fill).height(Length::Fill)),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container }
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
                InterneNachricht::Aktuell(f) => {
                    error!("Nicht verwendeter Lizenz-Text:\n{}", f())
                },
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
