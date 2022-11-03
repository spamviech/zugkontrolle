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

use crate::application::{map_mit_zustand::Dummy, style::hintergrund::Hintergrund};

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
    initial_overlay: Option<&'a dyn Fn() -> Overlay>,
    zeige_overlay: &'a dyn Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    schließe_bei_esc: bool,
}

impl<Overlay, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("initial_overlay", &"<closure>")
            .field("zeige_overlay", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc)
            .finish()
    }
}

impl<'a, Overlay, ElementNachricht, R> Modal<'a, Overlay, ElementNachricht, R> {
    /// Erstelle ein neues [Modal].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
        zeige_overlay: &'a impl Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    ) -> Self {
        Modal {
            underlay: underlay.into(),
            initial_overlay: None,
            zeige_overlay,
            schließe_bei_esc: false,
        }
    }

    /// Setzte das initial angezeigte Overlay.
    pub fn initiales_overlay(mut self, initial_overlay: &'a impl Fn() -> Overlay) -> Self {
        self.initial_overlay = Some(initial_overlay);
        self
    }

    /// Schließe das Overlay bei einem Druck der Esc-Taste.
    pub fn schließe_bei_esc(mut self) -> Self {
        self.schließe_bei_esc = true;
        self
    }
}

fn synchronisiere_widget_layout_validierung<Overlay, ElementNachricht>(
    inner_shell: &Shell<'_, Nachricht<Overlay, ElementNachricht>>,
    shell: &mut Shell<'_, ElementNachricht>,
) {
    if inner_shell.are_widgets_invalid() {
        shell.invalidate_widgets()
    } else if inner_shell.is_layout_invalid() {
        shell.invalidate_layout()
    }
}

fn bearbeite_modal_nachrichten<Overlay, ElementNachricht>(
    messages: Vec<Nachricht<Overlay, ElementNachricht>>,
    shell: &mut Shell<'_, ElementNachricht>,
    zustand: &mut Zustand<Overlay>,
    status: &mut event::Status,
) {
    for message in messages {
        match message {
            Nachricht::Underlay(element_nachricht) => shell.publish(element_nachricht),
            Nachricht::ZeigeOverlay(overlay) => {
                *status = event::Status::Captured;
                zustand.zeige_overlay(overlay)
            },
            Nachricht::VersteckeOverlay => {
                *status = event::Status::Captured;
                zustand.verstecke_overlay()
            },
        }
    }
}

impl<Overlay, ElementNachricht, R: Renderer> Widget<ElementNachricht, R>
    for Modal<'_, Overlay, ElementNachricht, R>
where
    Overlay: 'static,
{
    // TODO widget_newtype_methods!-Macro verwenden
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
        vec![
            Tree::new(&self.underlay),
            Tree::new(Element::<Nachricht<Overlay, ElementNachricht>, R>::new(Dummy)),
        ]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&[
            &self.underlay,
            &Element::<Nachricht<Overlay, ElementNachricht>, R>::new(Dummy),
        ])
    }

    fn state(&self) -> tree::State {
        let mut zustand = Zustand::<Overlay>::neu();
        if let Some(initial_overlay) = &self.initial_overlay {
            zustand.zeige_overlay(initial_overlay())
        }
        tree::State::new(zustand)
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
            let mut status = self.underlay.as_widget_mut().on_event(
                &mut state.children[0],
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            );
            synchronisiere_widget_layout_validierung(&inner_shell, shell);
            bearbeite_modal_nachrichten(messages, shell, zustand, &mut status);
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

    fn overlay<'s>(
        &'s self,
        state: &'s mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'s, ElementNachricht, R>> {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        if let Some(overlay) = zustand.overlay() {
            let element = (self.zeige_overlay)(overlay);
            let position = layout.position();
            Some(
                ModalOverlay::neu_element(element, zustand, &mut state.children[1])
                    .overlay(position),
            )
        } else {
            let [state_element, state_overlay] = &mut state.children[0..2] else {
                unreachable!("2-item slice has exactly two items!")
            };
            self.underlay.as_widget().overlay(state_element, layout, renderer).map(|overlay| {
                let position = layout.position();
                ModalOverlay::neu_overlay(overlay, zustand, state_overlay).overlay(position)
            })
        }
    }
}

impl<'a, Inner, Nachricht, R> From<Modal<'a, Inner, Nachricht, R>> for Element<'a, Nachricht, R>
where
    Inner: 'static,
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
    state: &'a mut Tree,
}

impl<'a, Overlay, ElementNachricht: 'a, R: 'a + Renderer>
    ModalOverlay<'a, Overlay, ElementNachricht, R>
{
    fn neu_element(
        overlay: Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
        zustand: &'a mut Zustand<Overlay>,
        state: &'a mut Tree,
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
            state,
        }
    }

    fn neu_overlay(
        overlay: overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
        zustand: &'a mut Zustand<Overlay>,
        state: &'a mut Tree,
    ) -> Self {
        ModalOverlay { element_or_overlay: Either::Right(overlay), zustand, state }
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
            Either::Left(element) => element.as_widget().draw(
                self.state,
                renderer,
                style,
                layout,
                cursor_position,
                &layout.bounds(),
            ),
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
        let mut status = match &mut self.element_or_overlay {
            Either::Left(element) => element.as_widget_mut().on_event(
                self.state,
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            ),
            Either::Right(overlay) => overlay.on_event(
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            ),
        };
        synchronisiere_widget_layout_validierung(&inner_shell, shell);
        bearbeite_modal_nachrichten(messages, shell, &mut self.zustand, &mut status);
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
            Either::Left(element) => element.as_widget().mouse_interaction(
                self.state,
                layout,
                cursor_position,
                viewport,
                renderer,
            ),
            Either::Right(overlay) => {
                overlay.mouse_interaction(layout, cursor_position, viewport, renderer)
            },
        }
    }
}
