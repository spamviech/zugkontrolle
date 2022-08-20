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
    renderer::{Quad, Renderer, Style},
    touch, Background, Clipboard, Color, Length, Point, Rectangle, Shell, Size, Vector,
};
use iced_pure::{
    overlay::{self, Overlay},
    widget::tree::{State, Tag, Tree},
    Element, Widget,
};

/// Ein Wrapper um eine mutable Referenz, die [DerefMut]-Zugriff überwacht.
#[derive(Debug)]
pub struct MutTracer<'a, T> {
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
pub struct MapMitZustand<'a, Intern, Zustand, Extern, R> {
    element: Element<'a, Intern, R>,
    zustand: &'a dyn Fn() -> Zustand,
    erzeuge_element: &'a dyn Fn(&Zustand) -> Element<'a, Intern, R>,
    mapper: &'a dyn Fn(Intern, &mut MutTracer<'_, Zustand>, &mut event::Status) -> Option<Extern>,
}

impl<Intern, Zustand, Extern, R> Debug for MapMitZustand<'_, Intern, Zustand, Extern, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("zustand", &"<closure>")
            .field("erzeuge_element", &"<closure>")
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
        if interne_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if interne_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }
        let zustand: &mut Zustand = state.state.downcast_mut();
        let mut mut_tracer = MutTracer::neu(zustand);
        for nachricht in interne_nachrichten {
            if let Some(externe_nachricht) =
                (self.mapper)(nachricht, &mut mut_tracer, &mut event_status)
            {
                shell.publish(externe_nachricht)
            }
        }
        if mut_tracer.verändert() {
            self.element = (self.erzeuge_element)(zustand)
        }
        event_status
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
    ) -> Option<overlay::Element<'a, Extern, R>> {
        todo!()
    }
}

// TODO overlay kann zustand, aber nicht widget verändern :(
struct MapMitZustandOverlay<'a, Intern, Zustand, Extern, R> {
    element: overlay::Element<'a, Intern, R>,
    zustand: &'a mut MutTracer<'a, Zustand>,
    mapper: &'a dyn Fn(Intern, &mut MutTracer<'_, Zustand>, &mut event::Status) -> Option<Extern>,
}

impl<Intern, Zustand: Debug, Extern, R> Debug
    for MapMitZustandOverlay<'_, Intern, Zustand, Extern, R>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustandOverlay")
            .field("element", &"<overlay::Element>")
            .field("zustand", &self.zustand)
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<Intern, Zustand, Extern, R: Renderer> Overlay<Extern, R>
    for MapMitZustandOverlay<'_, Intern, Zustand, Extern, R>
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        self.element.layout(renderer, bounds).translate(Vector { x: position.x, y: position.y })
    }

    fn draw(&self, renderer: &mut R, style: &Style, layout: Layout<'_>, cursor_position: Point) {
        self.element.draw(renderer, style, layout, cursor_position)
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
        todo!()
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.element.mouse_interaction(layout, cursor_position, viewport, renderer)
    }
}
