//! Überdecke ein Widget einem anderen Widget.

use std::fmt::{self, Debug, Formatter};

use either::Either;
use iced::{Rectangle, Size};
use iced_native::{
    event,
    keyboard::{self, KeyCode},
    layout, mouse,
    renderer::{Renderer, Style},
    Clipboard, Event, Layout, Length, Point, Shell,
};
use iced_pure::{
    overlay,
    widget::{
        tree::{self, Tag, Tree},
        Container,
    },
    Element, Widget,
};

use crate::application::style::hintergrund::Hintergrund;

/// Nachricht für Underlay und Overlay eines [Modals](Modal).
pub enum Nachricht<Overlay, ElementNachricht> {
    /// Nach außen durchgereichte Nachricht.
    Underlay(ElementNachricht),
    /// Ändere das angezeigte Overlay.
    ZeigeOverlay(Overlay),
    /// Verstecke das angezeigte Overlay.
    VersteckeOverlay,
}

/// Zustand des [Modal]-Widgets.
#[derive(Debug)]
struct Zustand<Overlay> {
    overlay: Option<Overlay>,
}

impl<Overlay> Zustand<Overlay> {
    /// Erstelle einen neuen Zustand für das [Modal]-Widget.
    fn neu() -> Self {
        Zustand { overlay: None }
    }

    // FIXME funktioniert stateless mit beeinflussen von außen?
    // Nein, aber spezielle Nachrichten sind möglich
    /// Zeige ein Overlay über dem Widget.
    #[inline(always)]
    fn zeige_overlay(&mut self, overlay: Overlay) {
        self.overlay = Some(overlay);
    }

    /// Lösche das Overlay, so dass nur das originale Widget sichtbar ist.
    #[inline(always)]
    fn verstecke_overlay(&mut self) {
        self.overlay = None;
    }

    /// Das aktuell gezeigte Overlay.
    #[inline(always)]
    fn overlay(&self) -> &Option<Overlay> {
        &self.overlay
    }

    /// Eine veränderliche Referenz auf das aktuelle Overlay.
    #[inline(always)]
    fn overlay_mut(&mut self) -> &mut Option<Overlay> {
        &mut self.overlay
    }
}

/// Ein Widget, dass ein Overlay vor einem anderen Widget anzeigen kann.
pub struct Modal<'a, Overlay, ElementNachricht, R> {
    underlay: Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    zeige_overlay: &'a dyn Fn(&Overlay) -> Element<'_, Nachricht<Overlay, ElementNachricht>, R>,
    schließe_bei_esc: bool,
}

impl<Overlay, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("zeige_overlay", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc)
            .finish()
    }
}

impl<'a, Overlay, ElementNachricht, R> Modal<'a, Overlay, ElementNachricht, R> {
    /// Erstelle ein neues [Modal].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
        zeige_overlay: &'a impl Fn(&Overlay) -> Element<'_, Nachricht<Overlay, ElementNachricht>, R>,
        schließe_bei_esc: bool,
    ) -> Self {
        Modal { underlay: underlay.into(), zeige_overlay, schließe_bei_esc }
    }
}

impl<Overlay, ElementNachricht, R: Renderer> Widget<ElementNachricht, R>
    for Modal<'_, Overlay, ElementNachricht, R>
{
    fn width(&self) -> Length {
        self.underlay.as_widget().width()
    }

    fn height(&self) -> Length {
        self.underlay.as_widget().height()
    }

    fn layout(&self, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.underlay.as_widget().layout(renderer, limits)
    }

    fn children(&self) -> Vec<Tree> {
        vec![Tree::new(&self.underlay)]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&[&self.underlay])
    }

    fn state(&self) -> tree::State {
        tree::State::new(Zustand::<Overlay>::neu())
    }

    fn tag(&self) -> Tag {
        Tag::of::<Zustand<Overlay>>()
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
        self.underlay.as_widget().draw(
            &state.children[0],
            renderer,
            style,
            layout,
            cursor_position,
            viewport,
        )
    }

    fn overlay<'s>(
        &'s self,
        state: &'s mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'s, ElementNachricht, R>> {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        if let Some(overlay) = zustand.overlay() {
            let bounds = layout.bounds();
            let position = Point::new(bounds.x, bounds.y);
            Some(
                ModalOverlay::neu_element((self.zeige_overlay)(overlay), zustand).overlay(position),
            )
        } else {
            self.underlay.as_widget().overlay(&mut state.children[0], layout, renderer).map(
                |overlay| {
                    let bounds = layout.bounds();
                    let position = Point::new(bounds.x, bounds.y);
                    ModalOverlay::neu_overlay(overlay, zustand).overlay(position)
                },
            )
        }
    }

    fn mouse_interaction(
        &self,
        state: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.underlay.as_widget().mouse_interaction(
            &state.children[0],
            layout,
            cursor_position,
            viewport,
            renderer,
        )
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, ElementNachricht>,
    ) -> event::Status {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        if zustand.overlay.is_none() {
            let mut messages = Vec::new();
            let mut inner_shell = Shell::new(&mut messages);
            let status = self.underlay.as_widget_mut().on_event(
                &mut state.children[0],
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            );
            if inner_shell.are_widgets_invalid() {
                shell.invalidate_widgets()
            } else if inner_shell.is_layout_invalid() {
                shell.invalidate_layout()
            }
            for message in messages {
                todo!()
            }
            status
        } else {
            match event {
                Event::Keyboard(keyboard::Event::KeyPressed {
                    key_code: KeyCode::Escape,
                    modifiers: _,
                }) if self.schließe_bei_esc => {
                    zustand.verstecke_overlay();
                    event::Status::Captured
                },
                _ => event::Status::Ignored,
            }
        }
    }
}

impl<'a, Inner, Nachricht, R> From<Modal<'a, Inner, Nachricht, R>> for Element<'a, Nachricht, R>
where
    Inner: 'a,
    Nachricht: 'a,
    R: 'a + Renderer,
{
    fn from(modal: Modal<'a, Inner, Nachricht, R>) -> Self {
        Element::new(modal)
    }
}

struct ModalOverlay<'a, Overlay, ElementNachricht, R> {
    element_or_overlay: Either<
        Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
        overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    >,
    zustand: &'a mut Zustand<Overlay>,
}

impl<'a, Overlay, ElementNachricht: 'a, R: 'a + Renderer>
    ModalOverlay<'a, Overlay, ElementNachricht, R>
{
    fn neu_element(
        overlay: Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
        zustand: &'a mut Zustand<Overlay>,
    ) -> Self {
        ModalOverlay {
            element_or_overlay: Either::Left(
                Container::new(overlay)
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .center_x()
                    .center_y()
                    .style(Hintergrund::GrauTransparent { grau: 0.7, alpha: 0.5 })
                    .into(),
            ),
            zustand,
        }
    }

    fn neu_overlay(
        overlay: overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
        zustand: &'a mut Zustand<Overlay>,
    ) -> Self {
        ModalOverlay { element_or_overlay: Either::Right(overlay), zustand }
    }
}

impl<'a, Overlay, ElementNachricht: 'a, R: 'a + Renderer>
    ModalOverlay<'a, Overlay, ElementNachricht, R>
{
    fn overlay(self, position: Point) -> overlay::Element<'a, ElementNachricht, R> {
        overlay::Element::new(position, Box::new(self))
    }
}

impl<Overlay, ElementNachricht, R: Renderer> overlay::Overlay<ElementNachricht, R>
    for ModalOverlay<'_, Overlay, ElementNachricht, R>
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        let mut layout = match &self.element_or_overlay {
            Either::Left(element) => {
                element.as_widget().layout(renderer, &layout::Limits::new(bounds, bounds))
            },
            Either::Right(overlay) => overlay.layout(renderer, bounds),
        };
        layout.move_to(position);
        layout
    }

    fn draw(&self, renderer: &mut R, style: &Style, layout: Layout<'_>, cursor_position: Point) {
        match &self.element_or_overlay {
            Either::Left(element) => {
                let widget = element.as_widget();
                let tree = Tree::new(widget);
                widget.draw(&tree, renderer, style, layout, cursor_position, &layout.bounds())
            },
            Either::Right(overlay) => overlay.draw(renderer, style, layout, cursor_position),
        }
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, ElementNachricht>,
    ) -> event::Status {
        let mut messages = Vec::new();
        let mut inner_shell = Shell::new(&mut messages);
        let status = match &mut self.element_or_overlay {
            Either::Left(element) => {
                let tree = Tree::new(element.as_widget());
                element.as_widget_mut().on_event(
                    &mut tree,
                    event,
                    layout,
                    cursor_position,
                    renderer,
                    clipboard,
                    &mut inner_shell,
                )
            },
            Either::Right(overlay) => overlay.on_event(
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            ),
        };
        if inner_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if inner_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }
        for message in messages {
            todo!()
        }
        status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        match &self.element_or_overlay {
            Either::Left(element) => {
                let widget = element.as_widget();
                let tree = Tree::new(widget);
                widget.mouse_interaction(&tree, layout, cursor_position, viewport, renderer)
            },
            Either::Right(overlay) => {
                overlay.mouse_interaction(layout, cursor_position, viewport, renderer)
            },
        }
    }
}
