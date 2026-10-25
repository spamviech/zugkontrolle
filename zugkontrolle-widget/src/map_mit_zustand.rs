//! Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::{
    convert::identity,
    fmt::{self, Debug, Formatter},
};

use iced_core::{
    Element, Event, Length, Rectangle, Shell, Size, Vector, Widget,
    layout::{self, Layout},
    mouse,
    overlay::{self, Overlay},
    renderer::{self, Renderer},
    widget::{
        self,
        tree::{self, Tree},
    },
};

use crate::flat_map::FlatMap;

/// Erzeuge ein [`Element`] ausgehend vom aktuellen [`Zustand`].
type ElementFn<'a, Zustand, Intern, Thema, R> =
    Box<dyn 'a + Fn(&Zustand) -> Element<'a, Intern, Thema, R>>;
/// Konvertiere eine [`Intern`]e Nachricht zu einer [`Extern`]en Nachricht,
/// bei potentieller Mutation des Zustandes.
type MapFn<'a, Zustand, Intern, Extern> = Box<dyn 'a + Fn(Intern, &mut Zustand) -> Vec<Extern>>;

/// Ein Hilfs-[`Widget`], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
pub struct MapMitZustand<'a, Zustand, Intern, Extern, Thema, R> {
    /// Das Element.
    element: Element<'a, Intern, Thema, R>,
    /// Der initiale Zustand.
    initialer_zustand: Zustand,
    /// Erzeuge die Widget-Hierarchie.
    erzeuge_element: ElementFn<'a, Zustand, Intern, Thema, R>,
    /// Konvertiere eine interne Nachricht, potentiell unter Änderung des Zustands.
    mapper: MapFn<'a, Zustand, Intern, Extern>,
}

impl<Zustand: Debug, Intern, Extern, Thema, R> Debug
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("MapMitZustand")
            .field("element", &"<element>")
            .field("zustand", &"<closure>")
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
        mapper: impl 'a + Fn(Intern, &mut Zustand) -> Vec<Extern>,
    ) -> Self {
        let element = erzeuge_element(&initialer_zustand);
        MapMitZustand {
            element,
            initialer_zustand,
            erzeuge_element: Box::new(erzeuge_element),
            mapper: Box::new(mapper),
        }
    }
}

impl<Zustand, Intern, Extern, Thema, R> Widget<Vec<Extern>, Thema, R>
    for MapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
where
    Zustand: 'static + Clone + PartialEq,
    R: Renderer,
{
    fn size(&self) -> Size<Length> {
        self.element.as_widget().size()
    }

    fn layout(&mut self, tree: &mut Tree, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.element.as_widget_mut().layout(
            tree.children.first_mut().expect("Tree has one child."),
            renderer,
            limits,
        )
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut R,
        theme: &Thema,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.element.as_widget().draw(
            tree.children.first().expect("Tree has one child."),
            renderer,
            theme,
            style,
            layout,
            cursor,
            viewport,
        );
    }

    fn size_hint(&self) -> Size<Length> {
        self.element.as_widget().size_hint()
    }

    fn tag(&self) -> tree::Tag {
        tree::Tag::of::<Zustand>()
    }

    fn state(&self) -> tree::State {
        tree::State::Some(Box::new(self.initialer_zustand.clone()))
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(self.element.as_widget())]
    }

    fn diff(&self, tree: &mut Tree) {
        self.element.as_widget().diff(tree.children.first_mut().expect("Tree has one child."));
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn widget::Operation,
    ) {
        self.element.as_widget_mut().operate(
            tree.children.first_mut().expect("Tree has one child."),
            layout,
            renderer,
            operation,
        );
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn iced_core::Clipboard,
        shell: &mut Shell<'_, Vec<Extern>>,
        viewport: &Rectangle,
    ) {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        self.element.as_widget_mut().update(
            tree.children.first_mut().expect("Tree has one child."),
            event,
            layout,
            cursor,
            renderer,
            clipboard,
            &mut local_shell,
            viewport,
        );

        let redraw_request = local_shell.redraw_request();
        shell.request_redraw_at(redraw_request);

        if local_shell.is_layout_invalid() {
            shell.invalidate_layout();
        }

        if local_shell.are_widgets_invalid() {
            shell.invalidate_widgets();
        }

        if local_shell.is_event_captured() {
            shell.capture_event();
        }

        *shell.input_method_mut() = local_shell.input_method().clone();

        let zustand = tree.state.downcast_mut::<Zustand>();
        let alter_zustand = zustand.clone();
        for intern in local_messages.drain(..) {
            let message = (self.mapper)(intern, zustand);
            shell.publish(message);
        }

        if alter_zustand != *zustand {
            self.element = (self.erzeuge_element)(&*zustand);
            // Anmerkung: Kein Aufruf von shell.invalidate_widgets().
            // Ansonsten werden bestimmte Änderungen nicht sofort sichtbar.
            // Beispiel: Anderen Lizenztext anzeigen.
            shell.request_redraw();
            shell.invalidate_layout();
            shell.capture_event();
        }
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.element.as_widget().mouse_interaction(
            tree.children.first().expect("Tree has one child."),
            layout,
            cursor,
            viewport,
            renderer,
        )
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'b>,
        renderer: &R,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'b, Vec<Extern>, Thema, R>> {
        self.element
            .as_widget_mut()
            .overlay(
                tree.children.first_mut().expect("Tree has one child."),
                layout,
                renderer,
                viewport,
                translation,
            )
            .map(|overlay| {
                let zustand = tree.state.downcast_mut::<Zustand>();
                overlay::Element::new(Box::new(OverlayMapMitZustand::neu(
                    overlay,
                    &self.mapper,
                    zustand,
                )))
            })
    }
}

/// Overlay für ein [`FlatMap`]-Widget.
struct OverlayMapMitZustand<'a, Zustand, Intern, Extern, Thema, Renderer> {
    /// Das Overlay.
    content: overlay::Element<'a, Intern, Thema, Renderer>,
    /// Die Funktion zur Transformation der ursprünglichen Nachrichten.
    mapper: &'a dyn Fn(Intern, &mut Zustand) -> Vec<Extern>,
    /// Der aktuelle Zustand.
    zustand: &'a mut Zustand,
}

impl<'a, Zustand, Intern, Extern, Thema, Renderer>
    OverlayMapMitZustand<'a, Zustand, Intern, Extern, Thema, Renderer>
{
    /// Erzeuge ein neues [`OverlayFlatMap`].
    fn neu(
        content: overlay::Element<'a, Intern, Thema, Renderer>,
        mapper: &'a dyn Fn(Intern, &mut Zustand) -> Vec<Extern>,
        zustand: &'a mut Zustand,
    ) -> OverlayMapMitZustand<'a, Zustand, Intern, Extern, Thema, Renderer> {
        OverlayMapMitZustand { content, mapper, zustand }
    }
}

impl<Zustand, Intern, Extern, Thema, R> Overlay<Vec<Extern>, Thema, R>
    for OverlayMapMitZustand<'_, Zustand, Intern, Extern, Thema, R>
where
    R: Renderer,
{
    fn layout(&mut self, renderer: &R, bounds: Size) -> layout::Node {
        self.content.as_overlay_mut().layout(renderer, bounds)
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &Thema,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
    ) {
        self.content.as_overlay().draw(renderer, theme, style, layout, cursor);
    }

    fn operate(&mut self, layout: Layout<'_>, renderer: &R, operation: &mut dyn widget::Operation) {
        self.content.as_overlay_mut().operate(layout, renderer, operation);
    }

    fn update(
        &mut self,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn iced_core::Clipboard,
        shell: &mut Shell<'_, Vec<Extern>>,
    ) {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        self.content.as_overlay_mut().update(
            event,
            layout,
            cursor,
            renderer,
            clipboard,
            &mut local_shell,
        );

        let redraw_request = local_shell.redraw_request();
        shell.request_redraw_at(redraw_request);

        if local_shell.is_layout_invalid() {
            shell.invalidate_layout();
        }

        if local_shell.are_widgets_invalid() {
            shell.invalidate_widgets();
        }

        if local_shell.is_event_captured() {
            shell.capture_event();
        }

        *shell.input_method_mut() = local_shell.input_method().clone();

        for intern in local_messages.drain(..) {
            let message = (self.mapper)(intern, self.zustand);
            shell.publish(message);
        }
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
    ) -> mouse::Interaction {
        self.content.as_overlay().mouse_interaction(layout, cursor, renderer)
    }

    fn overlay<'a>(
        &'a mut self,
        layout: Layout<'a>,
        renderer: &R,
    ) -> Option<overlay::Element<'a, Vec<Extern>, Thema, R>> {
        // Anmerkung: self.element wird erst bei nächstem Aufruf von self.update(..) aktualisiert.
        self.content.as_overlay_mut().overlay(layout, renderer).map(|overlay| {
            overlay::Element::new(Box::new(OverlayMapMitZustand::neu(
                overlay,
                self.mapper,
                self.zustand,
            )))
        })
    }

    fn index(&self) -> f32 {
        self.content.as_overlay().index()
    }
}

impl<'a, Zustand, Intern, Extern, Thema, R: Renderer>
    From<MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>> for Element<'a, Extern, Thema, R>
where
    Zustand: 'static + Clone + PartialEq,
    Intern: 'a,
    Extern: 'a,
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(map_mit_zustand: MapMitZustand<'a, Zustand, Intern, Extern, Thema, R>) -> Self {
        let map_mit_zustand = Element::new(map_mit_zustand);
        Element::new(FlatMap::neu(map_mit_zustand, identity))
    }
}
