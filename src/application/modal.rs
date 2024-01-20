//! Überdecke ein Widget einem anderen Widget.

use std::{
    fmt::{self, Debug, Formatter},
    ops::{Deref, DerefMut},
    time::Instant,
};

use iced::{Rectangle, Size};
use iced_core::{
    event,
    keyboard::{self, KeyCode},
    layout, mouse, overlay,
    renderer::{Renderer, Style},
    widget::{
        operation::Operation,
        tree::{self, Tag, Tree},
    },
    Clipboard, Element, Event, Layout, Length, Point, Shell, Vector, Widget,
};
use iced_widget::container::{self, Container};

use crate::application::{map_mit_zustand::MapOperation, style};

/// Nachricht für Underlay und Overlay eines [`Modals`](Modal).
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
    /// Konvertiere Overlay-Nachrichten mit dem [`From`]-Trait.
    pub fn overlay_from<O1>(value: Nachricht<O1, ElementNachricht>) -> Self
    where
        Overlay: From<O1>,
    {
        value.overlay_map(<Overlay as From<O1>>::from)
    }

    /// Konvertiere Overlay-Nachrichten mit der übergebenen Funktion.
    pub fn overlay_map<O1>(
        self,
        mapper: impl FnOnce(Overlay) -> O1,
    ) -> Nachricht<O1, ElementNachricht> {
        match self {
            Nachricht::Underlay(nachricht) => Nachricht::Underlay(nachricht),
            Nachricht::ZeigeOverlay(overlay) => Nachricht::ZeigeOverlay(mapper(overlay)),
            Nachricht::VersteckeOverlay => Nachricht::VersteckeOverlay,
        }
    }

    /// Konvertiere Underlay-Nachrichten mit dem [`From`]-Trait.
    pub fn underlay_from<N1>(value: Nachricht<Overlay, N1>) -> Self
    where
        ElementNachricht: From<N1>,
    {
        value.underlay_map(<ElementNachricht as From<N1>>::from)
    }

    /// Konvertiere Underlay-Nachrichten mit der übergebenen Funktion.
    pub fn underlay_map<N1>(
        self,
        mapper: impl FnOnce(ElementNachricht) -> N1,
    ) -> Nachricht<Overlay, N1> {
        match self {
            Nachricht::Underlay(nachricht) => Nachricht::Underlay(mapper(nachricht)),
            Nachricht::ZeigeOverlay(overlay) => Nachricht::ZeigeOverlay(overlay),
            Nachricht::VersteckeOverlay => Nachricht::VersteckeOverlay,
        }
    }

    /// Konvertiere die Nachricht, wenn das [Modal] innerhalb eines äußeren [`Modals`](Modal) angezeigt wird,
    /// ohne dieses direkt zu beeinflussen.
    pub fn äußeres_modal<Außen>(
        self,
    ) -> Nachricht<Overlay, Nachricht<Außen, ElementNachricht>> {
        match self {
            Nachricht::Underlay(element_nachricht) => {
                Nachricht::Underlay(Nachricht::Underlay(element_nachricht))
            },
            Nachricht::ZeigeOverlay(overlay) => Nachricht::ZeigeOverlay(overlay),
            Nachricht::VersteckeOverlay => Nachricht::VersteckeOverlay,
        }
    }
}

/// Wrapper-Struktur für den aktuellen Zustand des Overlays.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Overlay<O> {
    /// Das aktuell angezeigte Overlay.
    overlay: Option<O>,
    /// Die Zeit der letzten Änderung "von außen".
    zeitstempel: Instant,
}

impl<O> Deref for Overlay<O> {
    type Target = Option<O>;

    fn deref(&self) -> &Self::Target {
        &self.overlay
    }
}

impl<O> DerefMut for Overlay<O> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.zeitstempel = Instant::now();
        &mut self.overlay
    }
}

impl<O> Overlay<O> {
    /// Erzeuge einen neuen [`Overlay`]-Wrapper.
    pub fn neu(overlay: Option<O>) -> Overlay<O> {
        Overlay { overlay, zeitstempel: Instant::now() }
    }
}

/// Muss das overlay-Element aktualisiert werden?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OverlayElement {
    /// Das Element ist aktuell und muss nicht aktualisiert werden.
    Aktuell,
    /// Das Overlay wurde geändert. Element und Kind-Zustand müssen aktualisiert werden.
    Geändert,
}

/// Zustand des [`Modal`]-Widgets.
#[derive(Debug)]
struct Zustand<O> {
    /// Das aktuell angezeigte Overlay.
    overlay: Option<O>,
    /// Das initiale Overlay beim erzeugen des Widgets.
    initiales_overlay: Overlay<O>,
    /// Muss das overlay-Element aktualisiert werden?
    aktualisiere_element: OverlayElement,
    /// Der viewport des Elements, wird für [`Widget::on_event`] des overlays verwendet.
    viewport: Rectangle,
}

impl<O> Zustand<O> {
    /// Erstelle einen neuen Zustand für das [`Modal`]-Widget.
    fn neu(initiales_overlay: Overlay<O>) -> Self
    where
        O: Clone,
    {
        let overlay = initiales_overlay.deref().clone();
        let aktualisiere_element = OverlayElement::Geändert;
        Zustand { overlay, initiales_overlay, aktualisiere_element, viewport: Rectangle::default() }
    }

    /// Zeige ein Overlay über dem Widget.
    fn zeige_overlay(&mut self, overlay: O) {
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
pub struct Modal<'a, O, ElementNachricht, R> {
    /// Das normal angezeigte Element.
    underlay: Element<'a, Nachricht<O, ElementNachricht>, R>,
    /// Das aktuelle Overlay.
    overlay: Option<Element<'a, Nachricht<O, ElementNachricht>, R>>,
    /// Der initiale Wert des Overlays.
    initiales_overlay: &'a Overlay<O>,
    /// Erzeuge die Widget-Hierarchie für das Overlay.
    #[allow(clippy::type_complexity)]
    zeige_overlay: Box<dyn 'a + Fn(&O) -> Element<'a, Nachricht<O, ElementNachricht>, R>>,
    /// Wird ein [`Event`] vom `underlay` verarbeitet, auch wenn ein Overlay angezeigt wird.
    passthrough_event: Box<dyn 'a + Fn(&Event) -> bool>,
    /// Wird das Overlay geschlossen, wenn `Esc` gedrückt wird.
    schließe_bei_esc: bool,
}

impl<Overlay: Debug, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("overlay", &self.overlay.as_ref().map(|_| "<Element>"))
            .field("initiales_overlay", &self.initiales_overlay)
            .field("zeige_overlay", &"<closure>")
            .field("passthrough_event", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc)
            .finish()
    }
}

/// Aktualisiere das [`Element`] für das aktuell angezeigte Overlay.
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
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    let hat_overlay = overlay.is_some();
    let mut aktualisiere_element_und_children = || {
        // false-positive: neues_overlay related über as_ref().map()
        #[allow(clippy::shadow_unrelated)]
        {
            *overlay = neues_overlay.as_ref().map(|neues_overlay| {
                let element = zeige_overlay(neues_overlay);
                Container::new(element)
                    .width(Length::Fill)
                    .height(Length::Fill)
                    .center_x()
                    .center_y()
                    .style(style::Container::hintergrund_grau_transparent(0.7, 0.5))
                    .into()
            });
        }
        let dummy = Element::from(Dummy);
        *state_overlay = Tree::new(overlay.as_ref().unwrap_or(&dummy));
    };
    match *aktualisiere_overlay {
        OverlayElement::Aktuell => {
            if hat_overlay != neues_overlay.is_some() {
                aktualisiere_element_und_children();
            }
        },
        OverlayElement::Geändert => {
            aktualisiere_element_und_children();
            *aktualisiere_overlay = OverlayElement::Aktuell;
        },
    }
}

impl<'a, O, ElementNachricht, R> Modal<'a, O, ElementNachricht, R> {
    /// Erstelle ein neues [`Modal`].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht<O, ElementNachricht>, R>>,
        initiales_overlay: &'a Overlay<O>,
        zeige_overlay: impl 'a + Fn(&O) -> Element<'a, Nachricht<O, ElementNachricht>, R>,
    ) -> Self {
        /// Blockiere alle Events, wenn das Overlay angezeigt wird.
        fn kein_passthrough_event(_event: &Event) -> bool {
            false
        }
        Modal {
            underlay: underlay.into(),
            overlay: initiales_overlay.as_ref().map(&zeige_overlay),
            initiales_overlay,
            zeige_overlay: Box::new(zeige_overlay),
            passthrough_event: Box::new(kein_passthrough_event),
            schließe_bei_esc: false,
        }
    }

    /// Schließe das Overlay bei einem Druck der Esc-Taste.
    #[must_use]
    pub fn schließe_bei_esc(mut self) -> Self {
        self.schließe_bei_esc = true;
        self
    }

    /// Bearbeite die von `passthrough_event` akzeptierten Events zum Underlay,
    /// selbst wenn das Overlay aktuell angezeigt wird.
    #[must_use]
    pub fn passthrough_event(mut self, passthrough_event: impl 'a + Fn(&Event) -> bool) -> Self {
        self.passthrough_event = Box::new(passthrough_event);
        self
    }
}

/// Invalidiere Widgets und Layout der `shell`, wenn sie bei `interne_shell` invalidiert wurden.
fn synchronisiere_widget_layout_validierung<Overlay, ElementNachricht>(
    inner_shell: &Shell<'_, Nachricht<Overlay, ElementNachricht>>,
    shell: &mut Shell<'_, ElementNachricht>,
) {
    if inner_shell.are_widgets_invalid() {
        shell.invalidate_widgets();
    }
    if inner_shell.is_layout_invalid() {
        shell.invalidate_layout();
    }
}

/// Aktualisiere das aktuell angezeigt Overlay, oder gebe die Element-Nachricht weiter.
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
    Overlay: 'static + Clone + PartialEq,
    ElementNachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
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
        ]);
    }

    fn state(&self) -> tree::State {
        tree::State::new(Zustand::<Overlay>::neu(self.initiales_overlay.clone()))
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
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
    ) {
        self.underlay.as_widget().draw(
            state.children.first().expect("Keine State-Children gefunden!"),
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
        state: &Tree,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        viewport: &Rectangle,
        renderer: &R,
    ) -> mouse::Interaction {
        self.underlay.as_widget().mouse_interaction(
            state.children.first().expect("Keine State-Children gefunden!"),
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
        self.underlay.as_widget().operate(state, layout, renderer, &mut MapOperation { operation });
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, ElementNachricht>,
        viewport: &Rectangle,
    ) -> event::Status {
        let zustand: &mut Zustand<Overlay> = state.state.downcast_mut();
        if zustand.overlay.is_none() || (self.passthrough_event)(&event) {
            let mut messages = Vec::new();
            let mut inner_shell = Shell::new(&mut messages);
            let mut status = self.underlay.as_widget_mut().on_event(
                state.children.first_mut().expect("Keine State-Children gefunden!"),
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                &mut inner_shell,
                viewport,
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
                Event::Keyboard(_) | Event::Mouse(_) | Event::Window(_) | Event::Touch(_) => {
                    event::Status::Ignored
                },
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
        if zustand.initiales_overlay != *self.initiales_overlay {
            // Wenn sich initiales_overlay ändert muss der Zustand zurückgesetzt werden.
            *zustand = Zustand::<Overlay>::neu(self.initiales_overlay.clone());
        }
        let [state_underlay, state_overlay] = state.children.as_mut_slice() else {
            unreachable!("Invalide children-Anzahl!")
        };
        let element_overlay: Option<overlay::Element<'s, Nachricht<Overlay, ElementNachricht>, R>> =
            self.underlay.as_widget_mut().overlay(state_underlay, layout, renderer);
        aktualisiere_overlay_element(
            &mut self.overlay,
            state_overlay,
            &self.zeige_overlay,
            &zustand.overlay,
            &mut zustand.aktualisiere_element,
        );
        let modal_overlay = self.overlay.as_mut();
        let overlay = ModalOverlay {
            modal_overlay,
            element_overlay,
            zustand,
            state_overlay,
            passthrough_event: &self.passthrough_event,
        };
        Some(overlay::Element::new(layout.position(), Box::new(overlay)))
    }
}

impl<'a, Overlay, Nachricht, R> From<Modal<'a, Overlay, Nachricht, R>> for Element<'a, Nachricht, R>
where
    Overlay: 'static + Clone + PartialEq,
    Nachricht: 'a,
    R: 'a + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    fn from(modal: Modal<'a, Overlay, Nachricht, R>) -> Self {
        Element::new(modal)
    }
}

/// Dummy-Widget, das nichts anzeigt.
pub(in crate::application) struct Dummy;

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
        _cursor_position: mouse::Cursor,
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

/// Hilfs-Struktur für [`Modal`] um das aktuelle Overlay anzuzeigen.
struct ModalOverlay<'a, 'e, Overlay, ElementNachricht, R> {
    // Unterschied zu state_overlay
    #[allow(clippy::struct_field_names)]
    /// Die Overlay durch das Modal-Widget.
    modal_overlay: Option<&'a mut Element<'e, Nachricht<Overlay, ElementNachricht>, R>>,
    /// Der Zustand von `modal_overlay`.
    state_overlay: &'a mut Tree,
    /// Die Overlay des Elements.
    element_overlay: Option<overlay::Element<'a, Nachricht<Overlay, ElementNachricht>, R>>,
    /// Der aktuelle Zustand.
    zustand: &'a mut Zustand<Overlay>,
    /// Wird das Overlay geschlossen, wenn `Esc` gedrückt wird.
    passthrough_event: &'a dyn Fn(&Event) -> bool,
}

impl<'e, Overlay, ElementNachricht, R> overlay::Overlay<ElementNachricht, R>
    for ModalOverlay<'_, 'e, Overlay, ElementNachricht, R>
where
    ElementNachricht: 'e,
    Overlay: 'e,
    R: 'e + Renderer,
    <R as Renderer>::Theme: container::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
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
        cursor_position: mouse::Cursor,
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
            );
        } else if let Some(overlay) = &self.element_overlay {
            overlay.draw(renderer, theme, style, layout, cursor_position);
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
            );
        } else if let Some(overlay) = &mut self.element_overlay {
            overlay.operate(layout, renderer, &mut MapOperation { operation });
        } else {
            // keine Operation
        }
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, ElementNachricht>,
    ) -> event::Status {
        let mut messages = Vec::new();
        let mut inner_shell = Shell::new(&mut messages);
        let mut status = if let Some(overlay) = &mut self.modal_overlay {
            if (self.passthrough_event)(&event) {
                event::Status::Ignored
            } else {
                overlay.as_widget_mut().on_event(
                    self.state_overlay,
                    event,
                    layout,
                    cursor_position,
                    renderer,
                    clipboard,
                    &mut inner_shell,
                    &self.zustand.viewport,
                )
            }
        } else if let Some(overlay) = &mut self.element_overlay {
            overlay.on_event(event, layout, cursor_position, renderer, clipboard, &mut inner_shell)
        } else {
            event::Status::Ignored
        };
        synchronisiere_widget_layout_validierung(&inner_shell, shell);
        bearbeite_modal_nachrichten(messages, shell, self.zustand, &mut status);
        status
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
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
