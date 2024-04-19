//! Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    convert::identity,
    fmt::{self, Debug, Formatter},
};

use iced_core::{
    event::{self},
    renderer::Renderer,
    Element,
};
use iced_widget::{component, Component};

use crate::flat_map::FlatMap;

/// Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Der initiale Zustand.
    initialer_zustand: Zustand,
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
            initialer_zustand,
            erzeuge_element: Box::new(erzeuge_element),
            mapper: Box::new(mapper),
        }
    }
}

impl<Zustand, Intern, Extern, Thema, R> Component<Vec<Extern>, Thema, R>
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: Clone,
    R: Renderer,
{
    type State = Option<Zustand>;

    type Event = Intern;

    fn update(&mut self, state: &mut Self::State, event: Self::Event) -> Option<Vec<Extern>> {
        let MapMitZustand { initialer_zustand, erzeuge_element: _, mapper } = self;
        let zustand = state.get_or_insert(initialer_zustand.clone());
        let mut status = event::Status::Ignored;
        Some(mapper(event, zustand, &mut status))
    }

    fn view(&self, state: &Self::State) -> Element<'_, Self::Event, Thema, R> {
        let MapMitZustand { initialer_zustand, erzeuge_element, mapper: _ } = self;
        erzeuge_element(state.as_ref().unwrap_or(initialer_zustand))
    }
}

impl<'a, Zustand, Intern, Extern, Thema, R>
    From<MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>> for Element<'a, Extern, Thema, R>
where
    Zustand: 'static + Clone,
    Intern: 'a,
    Extern: 'a,
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(map_mit_zustand: MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>) -> Self {
        Element::from(FlatMap::neu(component(map_mit_zustand), identity))
    }
}
