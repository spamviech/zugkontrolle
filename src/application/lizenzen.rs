//! Show all Licenses of dependencies.

use std::fmt::{self, Debug, Formatter};

use iced_native::{
    container::{self, Container},
    event, Clipboard, Element, Event, Layout, Point, Renderer, Scrollable, Text, Widget,
};

use crate::application::macros::reexport_no_event_methods;

// TODO

enum InterneNachricht {}

#[derive(Debug)]
pub enum Nachricht {}

/// Auswahl-Fenster für [Streckenabschnitte](Streckenabschnitt).
pub struct Lizenzen<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneNachricht, R>,
    index: usize,
}

impl<R: Renderer + container::Renderer> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen")
            .field("container", &"<Container>")
            .field("index", &self.index)
            .finish()
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
        messages: &mut Vec<Nachricht>,
    ) -> event::Status {
        todo!()
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> From<Lizenzen<'a, R>>
    for Element<'a, Nachricht, R>
{
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}
