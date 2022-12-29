//! Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    fmt::{self, Debug, Formatter},
    ops::{Deref, DerefMut},
};

use iced_native::{
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    overlay::{self, Overlay},
    renderer::{Renderer, Style},
    widget::{
        operation::Operation,
        tree::{State, Tag, Tree},
    },
    Clipboard, Element, Length, Point, Rectangle, Shell, Size, Widget,
};

/// Ein Wrapper um eine mutable Referenz, die [DerefMut]-Zugriff überwacht.
#[derive(Debug)]
struct MutTracer<'a, T> {
    mut_ref: &'a mut T,
    verändert: bool,
}

impl<'a, T> MutTracer<'a, T> {
    /// Erzeuge einen neuen [MutTracer].
    fn neu(mut_ref: &'a mut T) -> Self {
        MutTracer { mut_ref, verändert: false }
    }

    /// Hat bereits ein Zugriff über [DerefMut] stattgefunden?
    #[inline(always)]
    fn verändert(&self) -> bool {
        self.verändert
    }
}

impl<T> Deref for MutTracer<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.mut_ref
    }
}

impl<T> DerefMut for MutTracer<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.verändert = true;
        self.mut_ref
    }
}

/// Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, R> {
    element: Element<'a, Intern, R>,
    erzeuge_zustand: &'a dyn Fn() -> Zustand,
    erzeuge_element: &'a dyn Fn(&Zustand) -> Element<'a, Intern, R>,
    mapper: &'a dyn Fn(
        Intern,
        &mut dyn DerefMut<Target = Zustand>,
        &mut event::Status,
    ) -> Option<Extern>,
}

impl<Zustand, Intern, Extern, R> Debug for MapMitZustand<'_, Zustand, Intern, Extern, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("erzeuge_zustand", &"<closure>")
            .field("erzeuge_element", &"<closure>")
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<'a, Zustand, Intern, Extern, R> MapMitZustand<'a, Zustand, Intern, Extern, R> {
    pub fn neu(
        erzeuge_zustand: &'a dyn Fn() -> Zustand,
        erzeuge_element: &'a dyn Fn(&Zustand) -> Element<'a, Intern, R>,
        mapper: &'a dyn Fn(
            Intern,
            &mut dyn DerefMut<Target = Zustand>,
            &mut event::Status,
        ) -> Option<Extern>,
    ) -> Self {
        let zustand = erzeuge_zustand();
        let element = erzeuge_element(&zustand);
        MapMitZustand { element, erzeuge_zustand, erzeuge_element, mapper }
    }
}

fn synchronisiere_widget_layout_validierung<Intern, Extern>(
    interne_shell: &Shell<'_, Intern>,
    shell: &mut Shell<'_, Extern>,
) {
    if interne_shell.are_widgets_invalid() {
        shell.invalidate_widgets()
    } else if interne_shell.is_layout_invalid() {
        shell.invalidate_layout()
    }
}

fn verarbeite_nachrichten<'a, Zustand, Intern, Extern, R>(
    interne_nachrichten: Vec<Intern>,
    shell: &mut Shell<'_, Extern>,
    zustand: &mut Zustand,
    event_status: &mut event::Status,
    mapper: &dyn Fn(
        Intern,
        &mut dyn DerefMut<Target = Zustand>,
        &mut event::Status,
    ) -> Option<Extern>,
    element: &mut Element<'a, Intern, R>,
    erzeuge_element: &dyn Fn(&Zustand) -> Element<'a, Intern, R>,
) {
    let mut mut_tracer = MutTracer::neu(zustand);
    for nachricht in interne_nachrichten {
        if let Some(externe_nachricht) = mapper(nachricht, &mut mut_tracer, event_status) {
            shell.publish(externe_nachricht)
        }
    }
    if mut_tracer.verändert() {
        *element = erzeuge_element(zustand);
    }
}

impl<Zustand, Intern, Extern, R> Widget<Extern, R> for MapMitZustand<'_, Zustand, Intern, Extern, R>
where
    Zustand: 'static,
    R: Renderer,
{
    fn width(&self) -> Length {
        self.element.as_widget().width()
    }

    fn height(&self) -> Length {
        self.element.as_widget().height()
    }

    fn layout(&self, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.element.as_widget().layout(renderer, limits)
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut R,
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        self.element.as_widget().draw(
            &state.children[0],
            renderer,
            theme,
            style,
            layout,
            cursor_position,
            viewport,
        )
    }

    fn tag(&self) -> Tag {
        Tag::of::<Zustand>()
    }

    fn state(&self) -> State {
        State::new((self.erzeuge_zustand)())
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.element)]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&[&self.element])
    }

    fn mouse_interaction(
        &self,
        state: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.element.as_widget().mouse_interaction(
            &state.children[0],
            layout,
            cursor_position,
            viewport,
            renderer,
        )
    }

    fn operate(&self, state: &mut Tree, layout: Layout<'_>, operation: &mut dyn Operation<Extern>) {
        self.element.as_widget().operate(state, layout, &mut MapOperation { operation })
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Extern>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let mut interne_shell = Shell::new(&mut interne_nachrichten);
        let mut event_status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_shell,
        );
        let zustand: &mut Zustand = state.state.downcast_mut();
        synchronisiere_widget_layout_validierung(&interne_shell, shell);
        verarbeite_nachrichten(
            interne_nachrichten,
            shell,
            zustand,
            &mut event_status,
            self.mapper,
            &mut self.element,
            self.erzeuge_element,
        );
        event_status
    }

    fn overlay<'a>(
        &'a mut self,
        state: &'a mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'a, Extern, R>> {
        let MapMitZustand { element, erzeuge_element, mapper, .. } = self;
        let zustand: &Zustand = state.state.downcast_ref();
        let zustand: &mut Zustand = state.state.downcast_mut();
        element.as_widget_mut().overlay(state, layout, renderer).map(|overlay| {
            let position = overlay.position();
            let map_mit_zustand_overlay =
                MapMitZustandOverlay { overlay, element, erzeuge_element, zustand, mapper };
            overlay::Element::new(position, Box::new(map_mit_zustand_overlay))
        })
    }
}

// Kopiert von https://docs.rs/iced_native/latest/src/iced_native/element.rs.html#295-335
pub(in crate::application) struct MapOperation<'a, B> {
    operation: &'a mut dyn Operation<B>,
}

impl<T, B> Operation<T> for MapOperation<'_, B> {
    fn container(
        &mut self,
        id: Option<&iced_native::widget::Id>,
        operate_on_children: &mut dyn FnMut(&mut dyn Operation<T>),
    ) {
        self.operation.container(id, &mut |operation| {
            operate_on_children(&mut MapOperation { operation });
        });
    }

    fn focusable(
        &mut self,
        state: &mut dyn iced_native::widget::operation::Focusable,
        id: Option<&iced_native::widget::Id>,
    ) {
        self.operation.focusable(state, id);
    }

    fn scrollable(
        &mut self,
        state: &mut dyn iced_native::widget::operation::Scrollable,
        id: Option<&iced_native::widget::Id>,
    ) {
        self.operation.scrollable(state, id);
    }

    fn text_input(
        &mut self,
        state: &mut dyn iced_native::widget::operation::TextInput,
        id: Option<&iced_native::widget::Id>,
    ) {
        self.operation.text_input(state, id);
    }
}

impl<'a, Zustand, Intern, Extern, R> From<MapMitZustand<'a, Zustand, Intern, Extern, R>>
    for Element<'a, Extern, R>
where
    Zustand: 'static,
    R: Renderer,
{
    fn from(map_mit_zustand: MapMitZustand<'a, Zustand, Intern, Extern, R>) -> Self {
        Element::new(map_mit_zustand)
    }
}

struct MapMitZustandOverlay<'a, 'e, Zustand, Intern, Extern, R> {
    overlay: overlay::Element<'a, Intern, R>,
    element: &'a mut Element<'e, Intern, R>,
    erzeuge_element: &'a dyn Fn(&Zustand) -> Element<'e, Intern, R>,
    zustand: &'a mut Zustand,
    mapper: &'a dyn Fn(
        Intern,
        &mut dyn DerefMut<Target = Zustand>,
        &mut event::Status,
    ) -> Option<Extern>,
}

impl<Zustand, Intern, Extern, R> Debug for MapMitZustandOverlay<'_, '_, Zustand, Intern, Extern, R>
where
    Zustand: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustandOverlay")
            .field("overlay", &"<overlay::Element>")
            .field("element", &"<Element>")
            .field("erzeuge_element", &"<closure>")
            .field("zustand", &self.zustand)
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<Zustand, Intern, Extern, R> Overlay<Extern, R>
    for MapMitZustandOverlay<'_, '_, Zustand, Intern, Extern, R>
where
    Zustand: 'static,
    R: Renderer,
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        let bisher = self.overlay.position();
        let unterschied = position - bisher;
        self.overlay = self.overlay.translate(unterschied);
        self.overlay.layout(renderer, bounds)
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
    ) {
        self.overlay.draw(renderer, theme, style, layout, cursor_position)
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Extern>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let mut interne_shell = Shell::new(&mut interne_nachrichten);
        let mut event_status = self.overlay.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_shell,
        );
        synchronisiere_widget_layout_validierung(&interne_shell, shell);
        verarbeite_nachrichten(
            interne_nachrichten,
            shell,
            self.zustand,
            &mut event_status,
            self.mapper,
            self.element,
            self.erzeuge_element,
        );
        event_status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.overlay.mouse_interaction(layout, cursor_position, viewport, renderer)
    }
}
