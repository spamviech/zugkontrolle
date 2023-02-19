//! Überdecke ein Widget einem anderen Widget.

use std::fmt::{self, Debug, Formatter};

use iced::{Rectangle, Size};
use iced_native::{
    event,
    keyboard::{self, KeyCode},
    layout, mouse, overlay,
    renderer::{Renderer, Style},
    widget::{
        container::{self, Container},
        operation::Operation,
        tree::{self, Tag, Tree},
    },
    Clipboard, Element, Event, Layout, Length, Point, Shell, Widget,
};

use crate::application::{map_mit_zustand::MapOperation, style::hintergrund::Hintergrund};

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
    overlay: Option<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    initial_overlay: Option<&'a dyn Fn() -> Overlay>,
    zeige_overlay: &'a dyn Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    schließe_bei_esc: bool,
}

impl<Overlay, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("overlay", &self.overlay.map(|_| "<Element>"))
            .field("initial_overlay", &self.initial_overlay.map(|_| "<closure>"))
            .field("zeige_overlay", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc)
            .finish()
    }
}

fn aktualisiere_overlay_element<'a, Overlay, ElementNachricht, R>(
    overlay: &mut Option<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    zeige_overlay: &impl Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    neues_overlay: &Option<Overlay>,
) where
    R: Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    *overlay = neues_overlay.as_ref().map(|neues_overlay| {
        let element = zeige_overlay(neues_overlay);
        Container::new(element)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .style(Hintergrund::GrauTransparent { grau: 0.7, alpha: 0.5 })
            .into()
    });
}

impl<'a, Overlay, ElementNachricht, R> Modal<'a, Overlay, ElementNachricht, R> {
    /// Erstelle ein neues [Modal].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
        zeige_overlay: &'a impl Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    ) -> Self {
        Modal {
            underlay: underlay.into(),
            overlay: None,
            initial_overlay: None,
            zeige_overlay,
            schließe_bei_esc: false,
        }
    }

    /// Schließe das Overlay bei einem Druck der Esc-Taste.
    pub fn schließe_bei_esc(mut self) -> Self {
        self.schließe_bei_esc = true;
        self
    }
}

impl<'a, Overlay, ElementNachricht, R> Modal<'a, Overlay, ElementNachricht, R>
where
    R: Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    /// Setzte das initial angezeigte Overlay.
    pub fn initiales_overlay(mut self, initial_overlay: &'a impl Fn() -> Overlay) -> Self {
        aktualisiere_overlay_element(
            &mut self.overlay,
            &self.zeige_overlay,
            &Some(initial_overlay()),
        );
        self.initial_overlay = Some(initial_overlay);
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

fn bearbeite_modal_nachrichten<'a, Overlay, ElementNachricht, R>(
    messages: Vec<Nachricht<Overlay, ElementNachricht>>,
    shell: &mut Shell<'_, ElementNachricht>,
    zustand: &mut Zustand<Overlay>,
    status: &mut event::Status,
    overlay: &mut Option<Element<'_, Nachricht<Overlay, ElementNachricht>, R>>,
    zeige_overlay: &impl Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
) where
    R: Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
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
    aktualisiere_overlay_element(overlay, zeige_overlay, zustand.overlay());
}

impl<Overlay, ElementNachricht, R> Widget<ElementNachricht, R>
    for Modal<'_, Overlay, ElementNachricht, R>
where
    Overlay: 'static,
    R: Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
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
        vec![
            Tree::new(&self.underlay),
            Tree::new(self.overlay.as_ref().unwrap_or(&Element::from(Dummy))),
        ]
    }

    fn diff(&self, tree: &mut Tree) {
        tree.diff_children(&[
            &self.underlay,
            self.overlay.as_ref().unwrap_or(&Element::from(Dummy)),
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
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        self.underlay.as_widget().draw(
            &state.children[0],
            renderer,
            theme,
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

    fn operate(
        &self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn Operation<ElementNachricht>,
    ) {
        self.underlay.as_widget().operate(state, layout, renderer, &mut MapOperation { operation })
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
            bearbeite_modal_nachrichten(
                messages,
                shell,
                zustand,
                &mut status,
                &mut self.overlay,
                &self.zeige_overlay,
            );
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
        &'s mut self,
        state: &'s mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'s, ElementNachricht, R>> {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        let element_overlay =
            self.underlay.as_widget().overlay(&mut state.children[0], layout, renderer);
        let overlay = ModalOverlay {
            modal_overlay: &mut self.overlay,
            element_overlay,
            zustand,
            state: &mut state.children[1],
            zeige_overlay: &self.zeige_overlay,
        };
        Some(overlay::Element::new(layout.position(), Box::new(overlay)))
    }
}

impl<'a, Inner, Nachricht, R> From<Modal<'a, Inner, Nachricht, R>> for Element<'a, Nachricht, R>
where
    Inner: 'static,
    Nachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    fn from(modal: Modal<'a, Inner, Nachricht, R>) -> Self {
        Element::new(modal)
    }
}

/// Dummy-Widget, das nichts anzeigt.
struct Dummy;

impl<M, R: Renderer> Widget<M, R> for Dummy {
    fn width(&self) -> Length {
        Length::Units(0)
    }

    fn height(&self) -> Length {
        Length::Units(0)
    }

    fn layout(&self, renderer: &R, limits: &layout::Limits) -> layout::Node {
        layout::Node::new(Size::ZERO)
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut R,
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        // zeichne nichts
    }
}

impl<M, R: Renderer> From<Dummy> for Element<'_, M, R> {
    fn from(dummy: Dummy) -> Self {
        Element::new(dummy)
    }
}

struct ModalOverlay<'a, Overlay, ElementNachricht, R> {
    modal_overlay: &'a mut Option<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    element_overlay: Option<overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    zustand: &'a mut Zustand<Overlay>,
    state: &'a mut Tree,
    zeige_overlay: &'a dyn Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
}

impl<'a, Overlay, ElementNachricht, R> ModalOverlay<'a, Overlay, ElementNachricht, R>
where
    ElementNachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    fn overlay(self, position: Point) -> overlay::Element<'a, ElementNachricht, R> {
        overlay::Element::new(position, Box::new(self))
    }
}

impl<Overlay, ElementNachricht, R> overlay::Overlay<ElementNachricht, R>
    for ModalOverlay<'_, Overlay, ElementNachricht, R>
where
    R: Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        let mut layout = if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().layout(renderer, &layout::Limits::new(bounds, bounds))
        } else if let Some(overlay) = &self.element_overlay {
            overlay.layout(renderer, bounds)
        } else {
            layout::Node::new(Size::ZERO)
        };
        layout.move_to(position);
        layout
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &<R as Renderer>::Theme,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: Point,
    ) {
        if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().draw(
                self.state,
                renderer,
                theme,
                style,
                layout,
                cursor_position,
                &layout.bounds(),
            )
        } else if let Some(overlay) = &self.element_overlay {
            overlay.draw(renderer, theme, style, layout, cursor_position)
        } else {
            // zeichne nichts
        }
    }

    fn operate(
        &mut self,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn Operation<ElementNachricht>,
    ) {
        if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().operate(
                self.state,
                layout,
                renderer,
                &mut MapOperation { operation },
            )
        } else if let Some(overlay) = &self.element_overlay {
            overlay.operate(layout, renderer, &mut MapOperation { operation })
        } else {
            // keine Operation
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
        let mut status = if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().on_event(
                self.state,
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            )
        } else if let Some(overlay) = &self.element_overlay {
            overlay.on_event(event, layout, cursor_position, renderer, clipboard, &mut inner_shell)
        } else {
            event::Status::Ignored
        };
        synchronisiere_widget_layout_validierung(&inner_shell, shell);
        bearbeite_modal_nachrichten(
            messages,
            shell,
            &mut self.zustand,
            &mut status,
            &mut self.modal_overlay,
            &self.zeige_overlay,
        );
        status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().mouse_interaction(
                self.state,
                layout,
                cursor_position,
                viewport,
                renderer,
            )
        } else if let Some(overlay) = &self.element_overlay {
            overlay.mouse_interaction(layout, cursor_position, viewport, renderer)
        } else {
            mouse::Interaction::default()
        }
    }
}
