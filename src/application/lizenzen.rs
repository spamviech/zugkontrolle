//! Show all Licenses of dependencies.

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_native::{
    button::{self, Button},
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

#[derive(Debug, Clone, Copy)]
enum InterneNachricht {}

// TODO vermutlich mindestens Schließen notwendig
// /// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
// #[derive(Debug, Clone, Copy)]
// pub enum Nachricht {}

/// Auswahl-Fenster für [Streckenabschnitte](Streckenabschnitt).
pub struct Lizenzen<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneNachricht, R>,
    lizenzen: BTreeMap<&'a str, fn() -> String>,
}

impl<R: Renderer + container::Renderer> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen").field("row", &"<Row>").field("lizenzen", &self.lizenzen).finish()
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
    ) -> Self {
        let mut scrollable =
            Scrollable::new(scrollable_state).width(Length::Shrink).style(scrollable_style);
        let mut aktuell = None;
        let mut lizenzen = BTreeMap::new();
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            if aktuell.is_none() {
                aktuell = Some(f());
            }
            scrollable = scrollable.push(Button::new(button_state, Text::new(name)));
            let _ = lizenzen.insert(name, *f);
        }
        let aktuell = aktuell.unwrap_or_else(String::new);
        // TODO aktuell veränderbar machen
        let container = Container::new(
            Row::new()
                .push(scrollable)
                .push(Rule::vertical(1).style(TRENNLINIE))
                .push(Text::new(aktuell).width(Length::Fill)),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, lizenzen }
    }
}

impl<'a, R: 'a + Renderer + container::Renderer, Nachricht> Widget<Nachricht, R>
    for Lizenzen<'a, R>
{
    reexport_no_event_methods! {Container<'a, InterneNachricht, R>, container, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        _nachrichten: &mut Vec<Nachricht>,
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
            // TODO aktuell veränderbar machen
            match interne_nachricht {}
        }
        event_status
    }
}

impl<'a, R: 'a + Renderer + container::Renderer, Nachricht> From<Lizenzen<'a, R>>
    for Element<'a, Nachricht, R>
{
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}
