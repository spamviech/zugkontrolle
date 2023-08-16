//! Wie [Map](iced_native::element::Map), nur dass mehrere Nachrichten zurückgegeben werden können.

use std::any::Any;

use iced_native::{
    clipboard::Clipboard,
    event::{self, Event},
    layout::{self, Layout},
    mouse,
    overlay::{self, Overlay},
    renderer::{self, Renderer},
    widget::{
        self,
        tree::{self, Tree},
        Widget,
    },
    Length, Point, Rectangle, Shell, Size,
};

///  Wie [Map](iced_native::element::Map), nur dass mehrere Nachrichten zurückgegeben werden können.
#[allow(missing_debug_implementations)]
pub struct FlatMap<'a, A, B, I: IntoIterator<Item = B>, Renderer> {
    widget: Box<dyn Widget<A, Renderer> + 'a>,
    mapper: Box<dyn Fn(A) -> I + 'a>,
}

impl<'a, A, B, I: IntoIterator<Item = B>, Renderer> FlatMap<'a, A, B, I, Renderer> {
    /// Erzeuge ein neues [FlatMap]-widget.
    pub fn neu(
        widget: Box<dyn Widget<A, Renderer> + 'a>,
        mapper: impl 'a + Fn(A) -> I,
    ) -> FlatMap<'a, A, B, I, Renderer> {
        FlatMap { widget, mapper: Box::new(mapper) }
    }
}

impl<'a, A, B, I: IntoIterator<Item = B>, Renderer> Widget<B, Renderer>
    for FlatMap<'a, A, B, I, Renderer>
where
    Renderer: self::Renderer + 'a,
    A: 'a,
    B: 'a,
{
    fn tag(&self) -> tree::Tag {
        self.widget.tag()
    }

    fn state(&self) -> tree::State {
        self.widget.state()
    }

    fn children(&self) -> Vec<Tree> {
        self.widget.children()
    }

    fn diff(&self, tree: &mut Tree) {
        self.widget.diff(tree)
    }

    fn width(&self) -> Length {
        self.widget.width()
    }

    fn height(&self) -> Length {
        self.widget.height()
    }

    fn layout(&self, renderer: &Renderer, limits: &layout::Limits) -> layout::Node {
        self.widget.layout(renderer, limits)
    }

    fn operate(
        &self,
        tree: &mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn widget::Operation<B>,
    ) {
        struct MapOperation<'a, B> {
            operation: &'a mut dyn widget::Operation<B>,
        }

        #[allow(single_use_lifetimes)]
        impl<'a, T, B> widget::Operation<T> for MapOperation<'a, B> {
            fn container(
                &mut self,
                id: Option<&widget::Id>,
                operate_on_children: &mut dyn FnMut(&mut dyn widget::Operation<T>),
            ) {
                self.operation.container(id, &mut |operation| {
                    operate_on_children(&mut MapOperation { operation });
                });
            }

            fn focusable(
                &mut self,
                state: &mut dyn widget::operation::Focusable,
                id: Option<&widget::Id>,
            ) {
                self.operation.focusable(state, id);
            }

            fn scrollable(
                &mut self,
                state: &mut dyn widget::operation::Scrollable,
                id: Option<&widget::Id>,
            ) {
                self.operation.scrollable(state, id);
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

        self.widget.operate(tree, layout, renderer, &mut MapOperation { operation });
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, B>,
    ) -> event::Status {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        let status = self.widget.on_event(
            tree,
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut local_shell,
        );

        if let Some(at) = local_shell.redraw_request() {
            shell.request_redraw(at);
        }

        if local_shell.is_layout_invalid() {
            shell.invalidate_layout();
        }

        if local_shell.are_widgets_invalid() {
            shell.invalidate_widgets();
        }

        for message in local_messages.drain(..).flat_map(&self.mapper) {
            shell.publish(message)
        }

        status
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        theme: &Renderer::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        self.widget.draw(tree, renderer, theme, style, layout, cursor_position, viewport)
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.widget.mouse_interaction(tree, layout, cursor_position, viewport, renderer)
    }

    fn overlay<'b>(
        &'b mut self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
    ) -> Option<overlay::Element<'b, B, Renderer>> {
        let mapper = &self.mapper;

        // self.widget.overlay(tree, layout, renderer).map(move |overlay| overlay.map(mapper))
        self.widget.overlay(tree, layout, renderer).map(move |overlay| {
            overlay::Element::new(
                overlay.position(),
                Box::new(OverlayFlatMap::neu(overlay, mapper)),
            )
        })
    }
}

struct OverlayFlatMap<'a, A, B, I: IntoIterator<Item = B>, Renderer> {
    content: overlay::Element<'a, A, Renderer>,
    mapper: &'a dyn Fn(A) -> I,
}

impl<'a, A, B, I: IntoIterator<Item = B>, Renderer> OverlayFlatMap<'a, A, B, I, Renderer> {
    fn neu(
        content: overlay::Element<'a, A, Renderer>,
        mapper: &'a dyn Fn(A) -> I,
    ) -> OverlayFlatMap<'a, A, B, I, Renderer> {
        OverlayFlatMap { content, mapper }
    }
}

impl<A, B, I: IntoIterator<Item = B>, Renderer> Overlay<B, Renderer>
    for OverlayFlatMap<'_, A, B, I, Renderer>
where
    Renderer: self::Renderer,
{
    fn layout(&self, renderer: &Renderer, bounds: Size, position: Point) -> layout::Node {
        self.content.layout(renderer, bounds, position - self.content.position())
    }

    fn operate(
        &mut self,
        layout: Layout<'_>,
        renderer: &Renderer,
        operation: &mut dyn widget::Operation<B>,
    ) {
        struct MapOperation<'a, B> {
            operation: &'a mut dyn widget::Operation<B>,
        }

        impl<T, B> widget::Operation<T> for MapOperation<'_, B> {
            fn container(
                &mut self,
                id: Option<&widget::Id>,
                operate_on_children: &mut dyn FnMut(&mut dyn widget::Operation<T>),
            ) {
                self.operation.container(id, &mut |operation| {
                    operate_on_children(&mut MapOperation { operation });
                });
            }

            fn focusable(
                &mut self,
                state: &mut dyn widget::operation::Focusable,
                id: Option<&widget::Id>,
            ) {
                self.operation.focusable(state, id);
            }

            fn scrollable(
                &mut self,
                state: &mut dyn widget::operation::Scrollable,
                id: Option<&widget::Id>,
            ) {
                self.operation.scrollable(state, id);
            }

            fn text_input(
                &mut self,
                state: &mut dyn widget::operation::TextInput,
                id: Option<&widget::Id>,
            ) {
                self.operation.text_input(state, id)
            }

            fn custom(&mut self, state: &mut dyn Any, id: Option<&widget::Id>) {
                self.operation.custom(state, id);
            }
        }

        self.content.operate(layout, renderer, &mut MapOperation { operation });
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, B>,
    ) -> event::Status {
        let mut local_messages = Vec::new();
        let mut local_shell = Shell::new(&mut local_messages);

        let event_status = self.content.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut local_shell,
        );

        if let Some(at) = local_shell.redraw_request() {
            shell.request_redraw(at);
        }

        if local_shell.is_layout_invalid() {
            shell.invalidate_layout();
        }

        if local_shell.are_widgets_invalid() {
            shell.invalidate_widgets();
        }

        for message in local_messages.drain(..).flat_map(self.mapper) {
            shell.publish(message)
        }

        event_status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        self.content.mouse_interaction(layout, cursor_position, viewport, renderer)
    }

    fn draw(
        &self,
        renderer: &mut Renderer,
        theme: &Renderer::Theme,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: Point,
    ) {
        self.content.draw(renderer, theme, style, layout, cursor_position)
    }

    fn is_over(&self, layout: Layout<'_>, cursor_position: Point) -> bool {
        self.content.is_over(layout, cursor_position)
    }
}
