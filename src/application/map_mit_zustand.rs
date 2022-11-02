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
    renderer::{Renderer, Style},
    Clipboard, Length, Point, Rectangle, Shell, Size, Vector,
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

#[derive(Debug)]
struct Zustand<Allgemein, Overlay> {
    allgemein: Allgemein,
    overlay: Overlay,
}

/// Ein Hilfs-[Widget], dass eine Konvertierung einer internen Nachricht in eine externe Nachricht
/// mit potentieller Mutation eines Zustands erlaubt.
pub struct MapMitZustand<'a, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R> {
    element: Element<'a, NIntern, R>,
    erzeuge_zustand: &'a dyn Fn() -> Zustand<ZAllgemein, ZOverlay>,
    erzeuge_element: &'a dyn Fn(&ZAllgemein, &ZOverlay) -> Element<'a, NIntern, R>,
    mapper: &'a dyn Fn(
        NIntern,
        &mut MutTracer<'_, ZAllgemein>,
        &mut ZOverlay,
        &mut event::Status,
    ) -> Option<NExtern>,
    erzeuge_overlay: &'a dyn Fn(&ZOverlay) -> Option<overlay::Element<'a, NOverlay, R>>,
    mapper_overlay: &'a dyn Fn(NOverlay, &mut ZOverlay, &mut event::Status) -> Option<NExtern>,
}

impl<'a, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R>
    MapMitZustand<'a, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R>
{
    pub fn neu(
        erzeuge_zustand: &'a dyn Fn() -> Zustand<ZAllgemein, ZOverlay>,
        erzeuge_element: &'a dyn Fn(&ZAllgemein, &ZOverlay) -> Element<'a, NIntern, R>,
        mapper: &'a dyn Fn(
            NIntern,
            &mut MutTracer<'_, ZAllgemein>,
            &mut ZOverlay,
            &mut event::Status,
        ) -> Option<NExtern>,
        erzeuge_overlay: &'a dyn Fn(&ZOverlay) -> Option<overlay::Element<'a, NOverlay, R>>,
        mapper_overlay: &'a dyn Fn(NOverlay, &mut ZOverlay, &mut event::Status) -> Option<NExtern>,
    ) -> MapMitZustand<'a, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R> {
        let Zustand { allgemein, overlay } = erzeuge_zustand();
        MapMitZustand {
            element: erzeuge_element(&allgemein, &overlay),
            erzeuge_zustand,
            erzeuge_element,
            mapper,
            erzeuge_overlay,
            mapper_overlay,
        }
    }
}

impl<ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R> Debug
    for MapMitZustand<'_, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustand")
            .field("element", &"<Element>")
            .field("erzeuge_zustand", &"<closure>")
            .field("erzeuge_element", &"<closure>")
            .field("mapper", &"<closure>")
            .field("erzeuge_overlay", &"<closure>")
            .field("mapper_overlay", &"<closure>")
            .finish()
    }
}

impl<ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R> Widget<NExtern, R>
    for MapMitZustand<'_, ZAllgemein, ZOverlay, NIntern, NOverlay, NExtern, R>
where
    ZAllgemein: 'static,
    ZOverlay: 'static,
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
        Tag::of::<Zustand<ZAllgemein, ZOverlay>>()
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

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, NExtern>,
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
        let Zustand { allgemein, overlay }: &mut Zustand<ZAllgemein, ZOverlay> =
            state.state.downcast_mut();
        let mut mut_tracer = MutTracer::neu(allgemein);
        for nachricht in interne_nachrichten {
            if let Some(externe_nachricht) =
                (self.mapper)(nachricht, &mut mut_tracer, overlay, &mut event_status)
            {
                shell.publish(externe_nachricht)
            }
        }
        if mut_tracer.verändert() {
            self.element = (self.erzeuge_element)(allgemein, overlay)
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
        _renderer: &R,
    ) -> Option<overlay::Element<'a, NExtern, R>> {
        let zustand: &mut Zustand<ZAllgemein, ZOverlay> = state.state.downcast_mut();
        (self.erzeuge_overlay)(&zustand.overlay).map(|overlay| {
            let position = layout.position();
            overlay::Element::new(
                position,
                Box::new(MapMitZustandOverlay {
                    overlay,
                    zustand: &mut zustand.overlay,
                    mapper: &self.mapper_overlay,
                }),
            )
        })
    }
}

struct MapMitZustandOverlay<'a, ZOverlay, NOverlay, NExtern, R> {
    overlay: overlay::Element<'a, NOverlay, R>,
    zustand: &'a mut ZOverlay,
    mapper: &'a dyn Fn(NOverlay, &mut ZOverlay, &mut event::Status) -> Option<NExtern>,
}

impl<ZOverlay, NOverlay, NExtern, R> Debug
    for MapMitZustandOverlay<'_, ZOverlay, NOverlay, NExtern, R>
where
    ZOverlay: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapMitZustandOverlay")
            .field("element", &"<overlay::Element>")
            .field("zustand", &self.zustand)
            .field("mapper", &"<closure>")
            .finish()
    }
}

impl<ZOverlay, NOverlay, NExtern, R> Overlay<NExtern, R>
    for MapMitZustandOverlay<'_, ZOverlay, NOverlay, NExtern, R>
where
    R: Renderer,
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        self.overlay.layout(renderer, bounds).translate(Vector { x: position.x, y: position.y })
    }

    fn draw(&self, renderer: &mut R, style: &Style, layout: Layout<'_>, cursor_position: Point) {
        self.overlay.draw(renderer, style, layout, cursor_position)
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, NExtern>,
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
        if interne_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if interne_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }
        for nachricht in interne_nachrichten {
            if let Some(externe_nachricht) =
                (self.mapper)(nachricht, self.zustand, &mut event_status)
            {
                shell.publish(externe_nachricht)
            }
        }
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
