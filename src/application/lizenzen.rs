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
    Aktuell(String),
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
        schließen: &'a mut button::State,
    ) -> Self {
        let mut scrollable = Scrollable::new(scrollable_state)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        let mut aktuell = None;
        let mut lizenzen = BTreeMap::new();
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            if aktuell.is_none() {
                aktuell = Some(f());
            }
            scrollable = scrollable.push(
                Button::new(button_state, Text::new(name))
                    .on_press(InterneNachricht::Aktuell(String::from(name))),
            );
            let _ = lizenzen.insert(name, *f);
        }
        let column = Column::new()
            .push(scrollable)
            .push(Container::new(Rule::horizontal(1).style(TRENNLINIE)).width(Length::Shrink))
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
                .push(Text::new(aktuell).width(Length::Fill)),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, lizenzen }
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
            // TODO aktuell veränderbar machen
            println!("{interne_nachricht:?}");
            match interne_nachricht {
                InterneNachricht::Aktuell(_name) => {},
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
