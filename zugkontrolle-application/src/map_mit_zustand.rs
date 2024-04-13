//! Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    any::Any,
    convert::identity,
    fmt::{self, Debug, Formatter},
};

use iced_core::{
    event::{self},
    renderer::Renderer,
    widget::{self, operation::Operation},
    Element, Rectangle, Vector,
};
use iced_widget::{component, Component};

use crate::flat_map::FlatMap;

/// Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Der initiale Zustand.
    initialer_zustand: Option<Zustand>,
    /// Erzeuge die Widget-Hierarchie.
    #[allow(clippy::type_complexity)]
    erzeuge_element: Box<dyn 'a + Fn(&Zustand) -> Element<'a, Intern, Thema, R>>,
    /// Konvertiere eine interne Nachricht, potentiell unter Änderung des Zustands.
    #[allow(clippy::type_complexity)]
    mapper: Box<dyn 'a + Fn(Intern, &mut Zustand, &mut event::Status) -> Vec<Extern>>,
}

impl<Zustand: Debug, Intern, Extern, Thema, R> Debug
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MapMitZustand")
            .field("initialerZustand", &self.initialer_zustand)
            .field("erzeuge_element", &"<closure>")
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<'a, Zustand, Intern, Extern, Thema, R> MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Erzeuge einen neuen [`MapMitZustand`].
    pub fn neu(
        initialer_zustand: Zustand,
        erzeuge_element: impl 'a + Fn(&Zustand) -> Element<'a, Intern, Thema, R>,
        mapper: impl 'a + Fn(Intern, &mut Zustand, &mut event::Status) -> Vec<Extern>,
    ) -> Self {
        MapMitZustand {
            initialer_zustand: Some(initialer_zustand),
            erzeuge_element: Box::new(erzeuge_element),
            mapper: Box::new(mapper),
        }
    }
}

impl<Zustand, Intern, Extern, Thema, R> Component<Vec<Extern>, Thema, R>
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: Default,
    R: Renderer,
{
    type State = Zustand;

    type Event = Intern;

    fn update(&mut self, state: &mut Self::State, event: Self::Event) -> Option<Vec<Extern>> {
        if let Some(zustand) = self.initialer_zustand.take() {
            *state = zustand;
        }
        let mut status = event::Status::Ignored;
        Some((self.mapper)(event, state, &mut status))
    }

    fn view(&self, state: &Self::State) -> Element<'_, Self::Event, Thema, R> {
        (self.erzeuge_element)(self.initialer_zustand.as_ref().unwrap_or(state))
    }
}

impl<'a, Zustand, Intern, Extern, Thema, R>
    From<MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>> for Element<'a, Extern, Thema, R>
where
    Zustand: 'static + Default,
    Intern: 'a,
    Extern: 'a,
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(map_mit_zustand: MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>) -> Self {
        Element::from(FlatMap::neu(component(map_mit_zustand), identity))
    }
}

/// Kopiert von [`iced_core`](https://docs.rs/iced_core/latest/src/iced_core/element.rs.html#322)
pub(crate) struct MapOperation<'a, B> {
    /// [Operation]
    pub(crate) operation: &'a mut dyn Operation<B>,
}

impl<T, B> Operation<T> for MapOperation<'_, B> {
    fn container(
        &mut self,
        id: Option<&widget::Id>,
        bounds: Rectangle,
        operate_on_children: &mut dyn FnMut(&mut dyn Operation<T>),
    ) {
        self.operation.container(id, bounds, &mut |operation| {
            operate_on_children(&mut MapOperation { operation });
        });
    }

    fn focusable(&mut self, state: &mut dyn widget::operation::Focusable, id: Option<&widget::Id>) {
        self.operation.focusable(state, id);
    }

    fn scrollable(
        &mut self,
        state: &mut dyn widget::operation::Scrollable,
        id: Option<&widget::Id>,
        bounds: Rectangle,
        translation: Vector,
    ) {
        self.operation.scrollable(state, id, bounds, translation);
    }

    fn text_input(
        &mut self,
        state: &mut dyn widget::operation::TextInput,
        id: Option<&widget::Id>,
    ) {
        self.operation.text_input(state, id);
    }

    fn custom(&mut self, state: &mut dyn Any, id: Option<&widget::Id>) {
        self.operation.custom(state, id);
    }
}
