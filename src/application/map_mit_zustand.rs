//! Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
//! mit potentieller Mutation eines Zustands erlaubt.

use std::fmt::{self, Debug, Formatter};

use iced_native::{
    event::{self, Event},
    layout::{self, Layout},
    mouse, overlay,
    renderer::{Quad, Renderer, Style},
    touch, Background, Clipboard, Color, Length, Point, Rectangle, Shell, Size,
};
use iced_pure::{
    widget::tree::{State, Tag, Tree},
    Element, Widget,
};

/// Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
struct MapMitZustand<'a, Intern, Zustand, Extern, R> {
    element: Element<'a, Intern, R>,
    zustand: &'a dyn Fn() -> Zustand,
    mapper: &'a dyn Fn(Intern, &mut Zustand) -> Option<Extern>,
}

impl<Intern, Zustand, Extern, R> Debug for MapMitZustand<'_, Intern, Zustand, Extern, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("zustand", &"<closure>")
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<Intern, Zustand, Extern, R> Widget<Extern, R> for MapMitZustand<'_, Intern, Zustand, Extern, R>
where
    Zustand: 'static,
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
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        self.element.as_widget().draw(
            &state.children[0],
            renderer,
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
        State::new((self.zustand)())
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.element)]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&[&self.element])
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
        todo!()
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

    fn overlay<'a>(
        &'a self,
        state: &'a mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<iced_native::overlay::Element<'a, Extern, R>> {
        todo!()
    }
}
