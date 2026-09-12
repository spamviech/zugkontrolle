//! Wie [`Map`](iced_native::element::Map), nur dass mehrere Nachrichten zurückgegeben werden können.

use iced_core::{
    Element, Length, Point, Rectangle, Shell, Size, Vector,
    clipboard::Clipboard,
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    overlay::{self, Overlay},
    renderer::{self, Renderer},
    widget::{
        self, Widget, operation,
        tree::{self, Tree},
    },
};
use zugkontrolle_gleise::knopf::Nachricht;

///  Wie [`Map`](iced_native::element::Map), nur dass mehrere Nachrichten zurückgegeben werden können.
#[allow(missing_debug_implementations)]
pub struct FlatMap<'a, A, I, Thema, R> {
    /// Das ursprüngliche Widget.
    element: Element<'a, A, Thema, R>,
    /// Die Funktion zur Transformation der ursprünglichen Nachrichten.
    mapper: Box<dyn Fn(A) -> I + 'a>,
}

impl<'a, A, I, Thema, R> FlatMap<'a, A, I, Thema, R> {
    /// Erzeuge ein neues [`FlatMap`]-widget.
    pub fn neu(widget: impl Into<Element<'a, A, Thema, R>>, mapper: impl 'a + Fn(A) -> I) -> Self {
        FlatMap { element: widget.into(), mapper: Box::new(mapper) }
    }
}

impl<'a, A, B, I, Thema, R> Widget<B, Thema, R> for FlatMap<'a, A, I, Thema, R>
where
    B: 'a,
    I: IntoIterator<Item = B>,
    R: Renderer,
{
    fn tag(&self) -> tree::Tag {
        self.element.as_widget().tag()
    }

    fn state(&self) -> tree::State {
        self.element.as_widget().state()
    }

    fn children(&self) -> Vec<Tree> {
        self.element.as_widget().children()
    }

    fn diff(&self, tree: &mut Tree) {
        self.element.as_widget().diff(tree);
    }

    fn size(&self) -> Size<Length> {
        self.element.as_widget().size()
    }

    fn size_hint(&self) -> Size<Length> {
        self.element.as_widget().size_hint()
    }

    fn layout(&mut self, tree: &mut Tree, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.element.as_widget_mut().layout(tree, renderer, limits)
    }

    fn operate(
        &mut self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn widget::Operation,
    ) {
        self.element.as_widget_mut().operate(tree, layout, renderer, operation);
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, B>,
        viewport: &Rectangle,
    ) {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        self.element.as_widget_mut().update(
            tree,
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

        for message in local_messages.drain(..).flat_map(&self.mapper) {
            shell.publish(message);
        }
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut R,
        theme: &Thema,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.element.as_widget().draw(
            tree,
            renderer,
            theme,
            style,
            layout,
            cursor_position,
            viewport,
        );
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.element.as_widget().mouse_interaction(
            tree,
            layout,
            cursor_position,
            viewport,
            renderer,
        )
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'b>,
        renderer: &R,
        viewport: &Rectangle<f32>,
        translation: Vector,
    ) -> Option<overlay::Element<'b, B, Thema, R>> {
        let mapper = &self.mapper;
        self.element.as_widget_mut().overlay(tree, layout, renderer, viewport, translation).map(
            move |overlay| overlay::Element::new(Box::new(OverlayFlatMap::neu(overlay, mapper))),
        )
    }
}

impl<'a, A, B, I, Thema, R> From<FlatMap<'a, A, I, Thema, R>> for Element<'a, B, Thema, R>
where
    A: 'a,
    B: 'a,
    I: 'a + IntoIterator<Item = B>,
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(flat_map: FlatMap<'a, A, I, Thema, R>) -> Self {
        Element::new(flat_map)
    }
}

/// Overlay für ein [`FlatMap`]-Widget.
struct OverlayFlatMap<'a, A, B, I: IntoIterator<Item = B>, Thema, Renderer> {
    /// Das Overlay.
    content: overlay::Element<'a, A, Thema, Renderer>,
    /// Die Funktion zur Transformation der ursprünglichen Nachrichten.
    mapper: &'a dyn Fn(A) -> I,
}

impl<'a, A, B, I: IntoIterator<Item = B>, Thema, Renderer>
    OverlayFlatMap<'a, A, B, I, Thema, Renderer>
{
    /// Erzeuge ein neues [`OverlayFlatMap`].
    fn neu(
        content: overlay::Element<'a, A, Thema, Renderer>,
        mapper: &'a dyn Fn(A) -> I,
    ) -> OverlayFlatMap<'a, A, B, I, Thema, Renderer> {
        OverlayFlatMap { content, mapper }
    }
}

impl<A, B, I: IntoIterator<Item = B>, Thema, Renderer> Overlay<B, Thema, Renderer>
    for OverlayFlatMap<'_, A, B, I, Thema, Renderer>
where
    Renderer: self::Renderer,
{
    fn layout(&mut self, renderer: &Renderer, bounds: Size) -> layout::Node {
        self.content.as_overlay_mut().layout(renderer, bounds)
    }

    fn operate(
        &mut self,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn widget::Operation,
    ) {
        self.content.as_overlay_mut().operate(layout, renderer, operation);
    }

    fn update(
        &mut self,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, B>,
    ) {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        let event_status = self.content.as_overlay_mut().update(
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

        for message in local_messages.drain(..).flat_map(self.mapper) {
            shell.publish(message);
        }

        event_status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.content.as_overlay().mouse_interaction(layout, cursor, renderer)
    }

    fn draw(
        &self,
        renderer: &mut Renderer,
        theme: &Thema,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
    ) {
        self.content.as_overlay().draw(renderer, theme, style, layout, cursor_position);
    }
}
