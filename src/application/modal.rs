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
    Clipboard, Element, Event, Layout, Length, Point, Shell, Vector, Widget,
};

use crate::application::{map_mit_zustand::MapOperation, style::hintergrund::Hintergrund};

/// Nachricht für Underlay und Overlay eines [Modals](Modal).
#[derive(Debug, Clone)]
pub enum Nachricht<Overlay, ElementNachricht> {
    /// Nach außen durchgereichte Nachricht.
    Underlay(ElementNachricht),
    /// Ändere das angezeigte Overlay.
    ZeigeOverlay(Overlay),
    /// Verstecke das angezeigte Overlay.
    VersteckeOverlay,
}

impl<Overlay, ElementNachricht> From<ElementNachricht> for Nachricht<Overlay, ElementNachricht> {
    fn from(nachricht: ElementNachricht) -> Self {
        Nachricht::Underlay(nachricht)
    }
}

impl<Overlay, ElementNachricht> Nachricht<Overlay, ElementNachricht> {
    /// Konvertiere Overlay-Nachrichten mit dem [From]-Trait.
    pub fn overlay_from<O1>(value: Nachricht<O1, ElementNachricht>) -> Self
    where
        Overlay: From<O1>,
    {
        value.overlay_map(<Overlay as From<O1>>::from)
    }

    /// Konvertiere Overlay-Nachrichten mit der übergebenen Funktion.
    pub fn overlay_map<O1>(self, f: impl FnOnce(Overlay) -> O1) -> Nachricht<O1, ElementNachricht> {
        match self {
            Nachricht::Underlay(nachricht) => Nachricht::Underlay(nachricht),
            Nachricht::ZeigeOverlay(overlay) => Nachricht::ZeigeOverlay(f(overlay)),
            Nachricht::VersteckeOverlay => Nachricht::VersteckeOverlay,
        }
    }

    /// Konvertiere Underlay-Nachrichten mit dem [From]-Trait.
    pub fn underlay_from<N1>(value: Nachricht<Overlay, N1>) -> Self
    where
        ElementNachricht: From<N1>,
    {
        value.underlay_map(<ElementNachricht as From<N1>>::from)
    }

    /// Konvertiere Underlay-Nachrichten mit der übergebenen Funktion.
    pub fn underlay_map<N1>(
        self,
        f: impl FnOnce(ElementNachricht) -> N1,
    ) -> Nachricht<Overlay, N1> {
        match self {
            Nachricht::Underlay(nachricht) => Nachricht::Underlay(f(nachricht)),
            Nachricht::ZeigeOverlay(overlay) => Nachricht::ZeigeOverlay(overlay),
            Nachricht::VersteckeOverlay => Nachricht::VersteckeOverlay,
        }
    }
}

/// Muss das overlay-Element aktualisiert werden?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OverlayElement {
    /// Das Element ist aktuell und muss nicht aktualisiert werden.
    Aktuell,
    /// Das Overlay wurde geändert. Element und Kind-Zustand müssen aktualisiert werden.
    Geändert,
    /// Das initiale Element soll gezeigt werden. Element und Kind-Zustand müssen evtl. aktualisiert werden.
    Initial,
}

/// Zustand des [Modal]-Widgets.
#[derive(Debug)]
struct Zustand<Overlay> {
    overlay: Option<Overlay>,
    aktualisiere_element: OverlayElement,
}

impl<Overlay> Zustand<Overlay> {
    /// Erstelle einen neuen Zustand für das [Modal]-Widget.
    fn neu(overlay: Option<Overlay>) -> Self {
        let aktualisiere_element =
            if overlay.is_some() { OverlayElement::Initial } else { OverlayElement::Aktuell };
        Zustand { overlay, aktualisiere_element }
    }

    /// Zeige ein Overlay über dem Widget.
    fn zeige_overlay(&mut self, overlay: Overlay) {
        self.overlay = Some(overlay);
        self.aktualisiere_element = OverlayElement::Geändert;
    }

    /// Lösche das Overlay, so dass nur das originale Widget sichtbar ist.
    fn verstecke_overlay(&mut self) {
        self.overlay = None;
        self.aktualisiere_element = OverlayElement::Geändert;
    }
}

/// Ein Widget, dass ein Overlay vor einem anderen Widget anzeigen kann.
pub struct Modal<'a, Overlay, ElementNachricht, R> {
    underlay: Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    overlay: Option<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    initiales_overlay: Option<&'a dyn Fn() -> Overlay>,
    zeige_overlay:
        Box<dyn 'a + Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    schließe_bei_esc: bool,
}

impl<Overlay, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("overlay", &self.overlay.as_ref().map(|_| "<Element>"))
            .field("initiales_overlay", &self.initiales_overlay.map(|_| "<closure>"))
            .field("zeige_overlay", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc)
            .finish()
    }
}

fn aktualisiere_overlay_element<'a, Overlay, ElementNachricht, R>(
    overlay: &mut Option<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    state_overlay: &mut Tree,
    zeige_overlay: &impl Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    neues_overlay: &Option<Overlay>,
    aktualisiere_overlay: &mut OverlayElement,
) where
    Overlay: 'a,
    ElementNachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    macro_rules! aktualisiere_element {
        () => {
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
        };
    }
    macro_rules! aktualisiere_state {
        () => {
            let dummy = Element::from(Dummy);
            *state_overlay = Tree::new(overlay.as_ref().unwrap_or_else(|| &dummy));
        };
    }
    match *aktualisiere_overlay {
        OverlayElement::Aktuell => {},
        OverlayElement::Geändert => {
            aktualisiere_element!();
            aktualisiere_state!();
            *aktualisiere_overlay = OverlayElement::Aktuell;
        },
        OverlayElement::Initial => {
            if overlay.is_some() != neues_overlay.is_some() {
                aktualisiere_element!();
                aktualisiere_state!();
            }
        },
    }
}

impl<'a, Overlay, ElementNachricht, R> Modal<'a, Overlay, ElementNachricht, R> {
    /// Erstelle ein neues [Modal].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
        zeige_overlay: impl 'a + Fn(&Overlay) -> Element<'a, Nachricht<Overlay, ElementNachricht>, R>,
    ) -> Self {
        Modal {
            underlay: underlay.into(),
            overlay: None,
            initiales_overlay: None,
            zeige_overlay: Box::new(zeige_overlay),
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
    Overlay: 'a,
    ElementNachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    /// Setzte das initial angezeigte Overlay.
    pub fn initiales_overlay(mut self, initiales_overlay: &'a impl Fn() -> Overlay) -> Self {
        self.initiales_overlay = Some(initiales_overlay);
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

fn bearbeite_modal_nachrichten<'a, Overlay, ElementNachricht>(
    messages: Vec<Nachricht<Overlay, ElementNachricht>>,
    shell: &mut Shell<'_, ElementNachricht>,
    zustand: &mut Zustand<Overlay>,
    status: &mut event::Status,
) where
    Overlay: 'a,
    ElementNachricht: 'a,
{
    for message in messages {
        match message {
            Nachricht::Underlay(element_nachricht) => shell.publish(element_nachricht),
            Nachricht::ZeigeOverlay(overlay) => {
                zustand.zeige_overlay(overlay);
                *status = event::Status::Captured;
            },
            Nachricht::VersteckeOverlay => {
                zustand.verstecke_overlay();
                *status = event::Status::Captured;
            },
        }
    }
}

impl<'a, Overlay, ElementNachricht, R> Widget<ElementNachricht, R>
    for Modal<'a, Overlay, ElementNachricht, R>
where
    Overlay: 'static,
    ElementNachricht: 'a,
    R: 'a + Renderer,
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
        tree::State::new(Zustand::<Overlay>::neu(self.initiales_overlay.map(|f| f())))
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
        &'s mut self,
        state: &'s mut Tree,
        layout: Layout<'_>,
        renderer: &R,
    ) -> Option<overlay::Element<'s, ElementNachricht, R>> {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        let [state_underlay, state_overlay] = state.children.as_mut_slice() else {unreachable!("Invalide children-Anzahl!")};
        let element_overlay: Option<overlay::Element<'s, Nachricht<Overlay, ElementNachricht>, R>> =
            self.underlay.as_widget_mut().overlay(state_underlay, layout, renderer);
        aktualisiere_overlay_element(
            &mut self.overlay,
            state_overlay,
            &self.zeige_overlay,
            &zustand.overlay,
            &mut zustand.aktualisiere_element,
        );
        let modal_overlay = &mut self.overlay;
        let overlay: ModalOverlay<'s, 'a, Overlay, ElementNachricht, R> =
            ModalOverlay { modal_overlay, element_overlay, zustand, state_overlay };
        Some(overlay::Element::new(layout.position(), Box::new(overlay)))
    }
}

impl<'a, Overlay, Nachricht, R> From<Modal<'a, Overlay, Nachricht, R>> for Element<'a, Nachricht, R>
where
    Overlay: 'static,
    Nachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    fn from(modal: Modal<'a, Overlay, Nachricht, R>) -> Self {
        Element::new(modal)
    }
}

/// Dummy-Widget, das nichts anzeigt.
struct Dummy;

impl<M, R: Renderer> Widget<M, R> for Dummy {
    fn width(&self) -> Length {
        Length::Fixed(0.)
    }

    fn height(&self) -> Length {
        Length::Fixed(0.)
    }

    fn layout(&self, _renderer: &R, _limits: &layout::Limits) -> layout::Node {
        layout::Node::new(Size::ZERO)
    }

    fn draw(
        &self,
        _state: &Tree,
        _renderer: &mut R,
        _theme: &<R as Renderer>::Theme,
        _style: &Style,
        _layout: Layout<'_>,
        _cursor_position: Point,
        _viewport: &Rectangle,
    ) {
        // zeichne nichts
    }
}

impl<M, R: Renderer> From<Dummy> for Element<'_, M, R> {
    fn from(dummy: Dummy) -> Self {
        Element::new(dummy)
    }
}

struct ModalOverlay<'a, 'e, Overlay, ElementNachricht, R> {
    modal_overlay: &'a mut Option<Element<'e, Nachricht<Overlay, ElementNachricht>, R>>,
    state_overlay: &'a mut Tree,
    element_overlay: Option<overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    zustand: &'a mut Zustand<Overlay>,
}

impl<'e, Overlay, ElementNachricht, R> overlay::Overlay<ElementNachricht, R>
    for ModalOverlay<'_, 'e, Overlay, ElementNachricht, R>
where
    ElementNachricht: 'e,
    Overlay: 'e,
    R: 'e + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<Hintergrund>,
{
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> layout::Node {
        let mut layout = if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().layout(renderer, &layout::Limits::new(bounds, bounds))
        } else if let Some(overlay) = &self.element_overlay {
            overlay.layout(renderer, bounds, Vector::ZERO)
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
                self.state_overlay,
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
                self.state_overlay,
                layout,
                renderer,
                &mut MapOperation { operation },
            )
        } else if let Some(overlay) = &mut self.element_overlay {
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
        let mut status = if let Some(overlay) = &mut self.modal_overlay {
            overlay.as_widget_mut().on_event(
                self.state_overlay,
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
            )
        } else if let Some(overlay) = &mut self.element_overlay {
            overlay.on_event(event, layout, cursor_position, renderer, clipboard, &mut inner_shell)
        } else {
            event::Status::Ignored
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
        if let Some(overlay) = &self.modal_overlay {
            overlay.as_widget().mouse_interaction(
                self.state_overlay,
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
