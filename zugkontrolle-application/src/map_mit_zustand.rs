//! Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    any::Any,
    convert::identity,
    fmt::{self, Debug, Formatter},
    ops::{Deref, DerefMut},
};

use iced_core::{
    event::{self},
    renderer::Renderer,
    widget::{self, operation::Operation},
    Element, Length, Rectangle, Size, Vector,
};
use iced_widget::{component, Component};

use crate::flat_map::FlatMap;

/// Ein Wrapper um eine mutable Referenz, die [`DerefMut`]-Zugriff überwacht.
#[derive(Debug)]
struct MutTracer<'a, T> {
    /// Die Referenz.
    mut_ref: &'a mut T,
    /// Gab es einen verändernden Zugriff.
    verändert: bool,
}

impl<'a, T> MutTracer<'a, T> {
    /// Erzeuge einen neuen [`MutTracer`].
    fn neu(mut_ref: &'a mut T) -> Self {
        MutTracer { mut_ref, verändert: false }
    }

    // /// Hat bereits ein Zugriff über [`DerefMut`] stattgefunden?
    // fn verändert(&self) -> bool {
    //     self.verändert
    // }
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

/// Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
///
/// Anmerkung: Das overlay des Elements wird NICHT angezeigt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Das ursprüngliche Widget.
    element: Element<'a, Intern, Thema, R>,
    /// Der initiale Zustand, und ob es seit erzeugen des Widgets weiterhin der initiale Zustand gilt.
    initialer_zustand: (Zustand, bool),
    /// Erzeuge die Widget-Hierarchie.
    #[allow(clippy::type_complexity)]
    erzeuge_element: Box<dyn 'a + Fn(&Zustand) -> Element<'a, Intern, Thema, R>>,
    /// Konvertiere eine interne Nachricht, potentiell unter Änderung des Zustands.
    #[allow(clippy::type_complexity)]
    mapper: Box<
        dyn 'a + Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    >,
}

impl<Zustand: Debug, Intern, Extern, Thema, R> Debug
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("initialerZustand", &self.initialer_zustand)
            .field("erzeuge_element", &"<closure>")
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<'a, Zustand, Intern, Extern, Thema, R> MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Erzeuge einen neuen [`MapMitZustand`].
    ///
    /// **ANMERKUNG**: Aus technischen Gründen wird das overlay NICHT angezeigt.
    pub fn neu(
        erzeuge_zustand: impl 'a + Fn() -> Zustand,
        erzeuge_element: impl 'a + Fn(&Zustand) -> Element<'a, Intern, Thema, R>,
        mapper: impl 'a
            + Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    ) -> Self {
        let zustand = erzeuge_zustand();
        let element = erzeuge_element(&zustand);
        MapMitZustand {
            element,
            initialer_zustand: (zustand, true),
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
        // TODO status,MutTracer aus mapper entfernen
        let mut status = event::Status::Ignored;
        Some((self.mapper)(event, &mut MutTracer::neu(state), &mut status))
    }

    fn view(&self, state: &Self::State) -> Element<'_, Self::Event, Thema, R> {
        (self.erzeuge_element)(state)
    }

    fn operate(&self, _state: &mut Self::State, _operation: &mut dyn Operation<Vec<Extern>>) {
        // überlasse operate dem element
    }

    fn size_hint(&self) -> Size<Length> {
        self.element.as_widget().size_hint()
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

/*
/// Invalidiere Widgets und Layout der `shell`, wenn sie bei `interne_shell` invalidiert wurden.
fn synchronisiere_widget_layout_validierung<Intern, Extern>(
    interne_shell: &Shell<'_, Intern>,
    shell: &mut Shell<'_, Extern>,
) {
    if interne_shell.are_widgets_invalid() {
        shell.invalidate_widgets();
    }
    if interne_shell.is_layout_invalid() {
        shell.invalidate_layout();
    }
}
// mapper-Funktion
#[allow(clippy::type_complexity)]
/// Konvertiere interne Nachrichten mit dem `mapper` und erzeuge bei Zustands-Änderung das `element` neu.
fn verarbeite_nachrichten<'a, Zustand, Intern, Extern, Thema, R>(
    interne_nachrichten: Vec<Intern>,
    shell: &mut Shell<'_, Extern>,
    zustand: &mut Zustand,
    event_status: &mut event::Status,
    mapper: &dyn Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
    element: &mut Element<'a, Intern, Thema, R>,
    erzeuge_element: &dyn Fn(&Zustand) -> Element<'a, Intern, Thema, R>,
) {
    let mut mut_tracer = MutTracer::neu(zustand);
    for nachricht in interne_nachrichten {
        let externe_nachrichten = mapper(nachricht, &mut mut_tracer, event_status);
        for externe_nachricht in externe_nachrichten {
            shell.publish(externe_nachricht);
        }
    }
    if mut_tracer.verändert() {
        *element = erzeuge_element(zustand);
    }
}

impl<Zustand, Intern, Extern, Thema, R> Widget<Extern, Thema, R>
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: 'static + PartialEq,
    R: Renderer,
{
    fn size(&self) -> Size<Length> {
        self.element.as_widget().size()
    }

    fn size_hint(&self) -> Size<Length> {
        self.element.as_widget().size_hint()
    }

    fn layout(&self, state: &mut Tree, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.element.as_widget().layout(
            state.children.first_mut().expect("Keine State-Children gefunden!"),
            renderer,
            limits,
        )
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut R,
        theme: &Thema,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.element.as_widget().draw(
            state.children.first().expect("Keine State-Children gefunden!"),
            renderer,
            theme,
            style,
            layout,
            cursor_position,
            viewport,
        );
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
        tree.diff_children(&[&self.element]);
    }

    fn operate(
        &self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn Operation<Extern>,
    ) {
        self.element.as_widget().operate(state, layout, renderer, &mut MapOperation { operation });
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
        }
        let mut interne_nachrichten = Vec::new();
        let mut interne_shell = Shell::new(&mut interne_nachrichten);
        let mut event_status = self.element.as_widget_mut().on_event(
            state.children.first_mut().expect("Keine State-Children gefunden!"),
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
            state.children.first().expect("Keine State-Children gefunden!"),
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
        _translation: Vector,
    ) -> Option<overlay::Element<'a, Extern, Thema, R>> {
        // FIXME die aktuelle Implementierung benötigt gleichzeitigen mutable-borrow von state & element
        None
        // let MapMitZustand { element, erzeuge_element, mapper, .. } = self;
        // element.as_widget_mut().overlay(state, layout, renderer, translation).map(|overlay| {
        //     // FIXME state-borrow so lange wie overlay
        //     let zustand: &mut Zustand = state.state.downcast_mut();
        //     // FIXME element-borrow so lange wie overlay
        //     let map_mit_zustand_overlay =
        //         MapMitZustandOverlay { overlay, element, erzeuge_element, zustand, mapper };
        //     overlay::Element::new(Box::new(map_mit_zustand_overlay))
        // })
    }
}

/// Hilfs-Struct für [`MapMitZustand`] zum anzeigen des Overlays.
///
/// Funktioniert aktuell nicht.
struct MapMitZustandOverlay<'a, 'e, Zustand, Intern, Extern, Thema, R> {
    /// Das Overlay des Elements.
    overlay: overlay::Element<'a, Intern, Thema, R>,
    /// Das Element.
    element: &'a mut Element<'e, Intern, Thema, R>,
    /// Erzeuge ein neues Element aus dem aktuellen Zustand.
    erzeuge_element: &'a dyn Fn(&Zustand) -> Element<'e, Intern, Thema, R>,
    /// Der aktuelle Zustand.
    zustand: &'a mut Zustand,
    /// Konvertiere eine interne Nachricht, potentiell unter Anpassung des aktuellen Zustands.
    #[allow(clippy::type_complexity)]
    mapper:
        &'a dyn Fn(Intern, &mut dyn DerefMut<Target = Zustand>, &mut event::Status) -> Vec<Extern>,
}

impl<Zustand, Intern, Extern, Thema, R> Debug
    for MapMitZustandOverlay<'_, '_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: Debug,
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MapMitZustandOverlay")
            .field("overlay", &"<overlay::Element>")
            .field("element", &"<Element>")
            .field("erzeuge_element", &"<closure>")
            .field("zustand", &self.zustand)
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<Zustand, Intern, Extern, Thema, R> Overlay<Extern, Thema, R>
    for MapMitZustandOverlay<'_, '_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: 'static,
    R: Renderer,
{
    fn layout(&mut self, renderer: &R, bounds: Size) -> layout::Node {
        self.overlay.layout(renderer, bounds)
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &Thema,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
    ) {
        self.overlay.draw(renderer, theme, style, layout, cursor_position);
    }

    fn operate(&mut self, layout: Layout<'_>, renderer: &R, operation: &mut dyn Operation<Extern>) {
        self.overlay.operate(layout, renderer, &mut MapOperation { operation });
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

    fn overlay<'a>(
        &'a mut self,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'a, Extern, Thema, R>> {
        let MapMitZustandOverlay { overlay, element, erzeuge_element, zustand, mapper } = self;
        let overlay = overlay.overlay(layout, renderer);
        // false-positive: overlay related über `map`
        #[allow(clippy::shadow_unrelated)]
        overlay.map(|overlay| {
            let map_mit_zustand_overlay =
                MapMitZustandOverlay { overlay, element, erzeuge_element, zustand, mapper };
            overlay::Element::new(Box::new(map_mit_zustand_overlay))
        })
    }
}
*/
