//! Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    any::Any,
    fmt::{self, Debug, Formatter},
    ops::{Deref, DerefMut},
};

use iced_core::{
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    overlay::{self, Overlay},
    renderer::{Renderer, Style},
    widget::{
        self,
        operation::Operation,
        tree::{State, Tag, Tree},
    },
    Clipboard, Element, Length, Point, Rectangle, Shell, Size, Vector, Widget,
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
///
/// Anmerkung: Das overlay des Elements wird NICHT angezeigt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, R> {
    element: Element<'a, Intern, R>,
    erzeuge_zustand: Box<dyn 'a + Fn() -> Zustand>,
    initialer_zustand: (Zustand, bool),
    erzeuge_element: Box<dyn 'a + Fn(&Zustand) -> Element<'a, Intern, R>>,
    mapper: Box<
        dyn 'a + Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    >,
}

impl<Zustand: Debug, Intern, Extern, R> Debug for MapMitZustand<'_, Zustand, Intern, Extern, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("erzeuge_zustand", &"<closure>")
            .field("initialerZustand", &self.initialer_zustand)
            .field("erzeuge_element", &"<closure>")
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<'a, Zustand, Intern, Extern, R> MapMitZustand<'a, Zustand, Intern, Extern, R> {
    /// Erzeuge einen neuen [MapMitZustand].
    ///
    /// **ANMERKUNG**: Aus technischen Gründen wird das overlay NICHT angezeigt.
    pub fn neu(
        erzeuge_zustand: impl 'a + Fn() -> Zustand,
        erzeuge_element: impl 'a + Fn(&Zustand) -> Element<'a, Intern, R>,
        mapper: impl 'a
            + Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    ) -> Self {
        let zustand = erzeuge_zustand();
        let element = erzeuge_element(&zustand);
        MapMitZustand {
            element,
            erzeuge_zustand: Box::new(erzeuge_zustand),
            initialer_zustand: (zustand, true),
            erzeuge_element: Box::new(erzeuge_element),
            mapper: Box::new(mapper),
        }
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
    mapper: &dyn Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    element: &mut Element<'a, Intern, R>,
    erzeuge_element: &dyn Fn(&Zustand) -> Element<'a, Intern, R>,
) {
    let mut mut_tracer = MutTracer::neu(zustand);
    for nachricht in interne_nachrichten {
        let externe_nachrichten = mapper(nachricht, &mut mut_tracer, event_status);
        for externe_nachricht in externe_nachrichten {
            shell.publish(externe_nachricht)
        }
    }
    if mut_tracer.verändert() {
        *element = erzeuge_element(zustand);
    }
}

impl<Zustand, Intern, Extern, R> Widget<Extern, R> for MapMitZustand<'_, Zustand, Intern, Extern, R>
where
    Zustand: 'static + PartialEq,
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
        cursor_position: mouse::Cursor,
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

    fn operate(
        &self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn Operation<Extern>,
    ) {
        self.element.as_widget().operate(state, layout, renderer, &mut MapOperation { operation })
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Extern>,
        viewport: &Rectangle,
    ) -> event::Status {
        let zustand: &mut Zustand = state.state.downcast_mut();
        if self.initialer_zustand.1 && (self.initialer_zustand.0 != *zustand) {
            self.initialer_zustand.1 = false;
            self.element = (self.erzeuge_element)(zustand);
            state.children = vec![Tree::new(&self.element)];
        }
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
            viewport,
        );
        synchronisiere_widget_layout_validierung(&interne_shell, shell);
        verarbeite_nachrichten(
            interne_nachrichten,
            shell,
            zustand,
            &mut event_status,
            &self.mapper,
            &mut self.element,
            &self.erzeuge_element,
        );
        event_status
    }

    fn mouse_interaction(
        &self,
        state: &Tree,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
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

    fn overlay<'a>(
        &'a mut self,
        _state: &'a mut Tree,
        _layout: Layout<'_>,
        _renderer: &R,
    ) -> Option<overlay::Element<'a, Extern, R>> {
        // FIXME die aktuelle Implementierung benötigt gleichzeitigen mutable-borrow von state & element
        None
        // let MapMitZustand { element, erzeuge_element, mapper, .. } = self;
        // element.as_widget_mut().overlay(state, layout, renderer).map(|overlay| {
        //     // FIXME state-borrow so lange wie overlay
        //     let zustand: &mut Zustand = state.state.downcast_mut();
        //     let position = overlay.position();
        //     // FIXME element-borrow so lange wie overlay
        //     let map_mit_zustand_overlay =
        //         MapMitZustandOverlay { overlay, element, erzeuge_element, zustand, mapper };
        //     overlay::Element::new(position, Box::new(map_mit_zustand_overlay))
        // })
    }
}

// Kopiert von https://docs.rs/iced_native/latest/src/iced_native/element.rs.html#295-335
pub(in crate::application) struct MapOperation<'a, B> {
    pub(in crate::application) operation: &'a mut dyn Operation<B>,
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

impl<'a, Zustand, Intern, Extern, R> From<MapMitZustand<'a, Zustand, Intern, Extern, R>>
    for Element<'a, Extern, R>
where
    Zustand: 'static + PartialEq,
    Intern: 'a,
    Extern: 'a,
    R: 'a + Renderer,
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
    mapper:
        &'a dyn Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
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
        self.overlay.layout(renderer, bounds, unterschied)
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
    ) {
        self.overlay.draw(renderer, theme, style, layout, cursor_position)
    }

    fn operate(&mut self, layout: Layout<'_>, renderer: &R, operation: &mut dyn Operation<Extern>) {
        self.overlay.operate(layout, renderer, &mut MapOperation { operation })
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
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
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.overlay.mouse_interaction(layout, cursor_position, viewport, renderer)
    }

    fn is_over(&self, layout: Layout<'_>, _renderer: &R, cursor_position: Point) -> bool {
        layout.bounds().contains(cursor_position)
    }
}
