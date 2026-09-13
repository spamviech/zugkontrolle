//! Überdecke ein Widget einem anderen Widget.

use std::fmt::{self, Debug, Formatter};

use iced_core::{
    Clipboard, Element, Event, Layout, Length, Rectangle, Shell, Size, Vector, Widget,
    keyboard::{
        self,
        key::{self, Key},
    },
    layout, mouse, overlay,
    renderer::{Renderer, Style},
    widget::{
        operation::Operation,
        tree::{self, Tag, Tree},
    },
};
use iced_widget::container::{self, Container};


/// Ein Widget, dass ein Overlay vor einem anderen Widget anzeigen kann.
pub struct Modal<'a, Nachricht, Thema, R> {
    /// Das normal angezeigte Element.
    underlay: Element<'a, Nachricht, Thema, R>,
    /// Das aktuelle Overlay.
    overlay: Option<Element<'a, Nachricht, Thema, R>>,
    /// Wird ein [`Event`] vom `underlay` verarbeitet, auch wenn ein Overlay angezeigt wird.
    passthrough_event: Box<dyn 'a + Fn(&Event) -> bool>,
    /// Welche Nachricht wird erzeugt, wenn bei offenem Overlay `Esc` gedrückt wird.
    schließe_bei_esc: Option<Box<dyn 'a + Fn() -> Nachricht>>,
}

impl<Nachricht, Thema, R> Debug for Modal<'_, Nachricht, Thema, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("Modal")
            .field("underlay", &"<Element>")
            .field("overlay", &self.overlay.as_ref().map(|_element| "<Element>"))
            .field("passthrough_event", &"<closure>")
            .field("schließe_bei_esc", &self.schließe_bei_esc.as_ref().map(|_closure| "<closure>"))
            .finish()
    }
}

impl<'a, Nachricht, Thema, R> Modal<'a, Nachricht, Thema, R> {
    /// Erstelle ein neues [`Modal`].
    pub fn neu(
        underlay: impl 'a + Into<Element<'a, Nachricht, Thema, R>>,
        overlay: Option<impl 'a + Into<Element<'a, Nachricht, Thema, R>>>,
    ) -> Self
    where
        Nachricht: 'a,
        Thema: 'a + container::Catalog,
        R: 'a + Renderer,
    {
        /// Blockiere alle Events, wenn das Overlay angezeigt wird.
        fn kein_passthrough_event(_event: &Event) -> bool {
            false
        }
        Modal {
            underlay: underlay.into(),
            overlay: overlay.map(|element| {
                let element = element.into();
                let size = element.as_widget().size();
                Container::new(element).center_x(size.width).center_y(size.height).into()
            }),
            passthrough_event: Box::new(kein_passthrough_event),
            schließe_bei_esc: None,
        }
    }

    /// Schließe das Overlay bei einem Druck der Esc-Taste.
    #[must_use]
    pub fn schließe_bei_esc(
        mut self,
        erzeuge_schließen_nachricht: impl 'a + Fn() -> Nachricht,
    ) -> Self {
        self.schließe_bei_esc = Some(Box::new(erzeuge_schließen_nachricht));
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

impl<'a, Nachricht, Thema, R> Widget<Nachricht, Thema, R> for Modal<'a, Nachricht, Thema, R>
where
    Nachricht: 'a,
    R: 'a + Renderer,
    Thema: 'a + container::Catalog,
{
    fn size(&self) -> Size<Length> {
        self.underlay.as_widget().size()
    }

    fn size_hint(&self) -> Size<Length> {
        self.underlay.as_widget().size_hint()
    }

    fn layout(&mut self, state: &mut Tree, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.underlay.as_widget_mut().layout(
            state.children.first_mut().expect("Keine State-Children gefunden!"),
            renderer,
            limits,
        )
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
        tree::State::None
    }

    fn tag(&self) -> Tag {
        Tag::stateless()
    }

    fn draw(
        &self,
        state: &Tree,
        renderer: &mut R,
        theme: &Thema,
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
        &mut self,
        state: &mut Tree,
        layout: Layout<'_>,
        renderer: &R,
        operation: &mut dyn Operation,
    ) {
        self.underlay.as_widget_mut().operate(state, layout, renderer, operation);
    }

    fn update(
        &mut self,
        tree: &mut Tree,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
        viewport: &Rectangle,
    ) {
        if self.overlay.is_none() || (self.passthrough_event)(event) {
            self.underlay.as_widget_mut().update(
                tree.children.first_mut().expect("Keine State-Children gefunden!"),
                event,
                layout,
                cursor,
                renderer,
                clipboard,
                shell,
                viewport,
            );
        } else if let (
            Event::Keyboard(keyboard::Event::KeyPressed {
                key: Key::Named(key::Named::Escape),
                modifiers: _,
                location: _,
                text: _,
                modified_key: _,
                physical_key: _,
                repeat: _,
            }),
            Some(erzeuge_schließen_nachricht),
        ) = (event, &self.schließe_bei_esc) { shell.publish(erzeuge_schließen_nachricht()) }
    }

    fn overlay<'s>(
        &'s mut self,
        state: &'s mut Tree,
        layout: Layout<'s>,
        renderer: &R,
        viewport: &Rectangle,
        translation: Vector,
    ) -> Option<overlay::Element<'s, Nachricht, Thema, R>> {
        let [state_underlay, state_overlay] = state.children.as_mut_slice() else {
            unreachable!("Invalide children-Anzahl!")
        };
        if let Some(overlay) = &mut self.overlay {
            overlay.as_widget().diff(state_overlay);

            let element = ModalOverlay {
                element: overlay,
                state: state_overlay,
                passthrough_event: &self.passthrough_event,
                viewport: layout.bounds(),
            };
            Some(overlay::Element::new(Box::new(element)))
        } else {
            self.underlay.as_widget_mut().overlay(
                state_underlay,
                layout,
                renderer,
                viewport,
                translation,
            )
        }
    }
}

impl<'a, Nachricht, Thema, R> From<Modal<'a, Nachricht, Thema, R>>
    for Element<'a, Nachricht, Thema, R>
where
    Nachricht: 'a,
    R: 'a + Renderer,
    Thema: 'a + container::Catalog,
{
    fn from(modal: Modal<'a, Nachricht, Thema, R>) -> Self {
        Element::new(modal)
    }
}

/// Dummy-Widget, das nichts anzeigt.
struct Dummy;

impl<M, Thema, R: Renderer> Widget<M, Thema, R> for Dummy {
    fn size(&self) -> Size<Length> {
        Size { width: Length::Fixed(0.), height: Length::Fixed(0.) }
    }

    fn layout(
        &mut self,
        _tree: &mut Tree,
        _renderer: &R,
        _limits: &layout::Limits,
    ) -> layout::Node {
        layout::Node::new(Size::ZERO)
    }

    fn draw(
        &self,
        _state: &Tree,
        _renderer: &mut R,
        _theme: &Thema,
        _style: &Style,
        _layout: Layout<'_>,
        _cursor_position: mouse::Cursor,
        _viewport: &Rectangle,
    ) {
        // zeichne nichts
    }
}

impl<M, Thema, R: Renderer> From<Dummy> for Element<'_, M, Thema, R> {
    fn from(dummy: Dummy) -> Self {
        Element::new(dummy)
    }
}

/// Hilfs-Struktur für [`Modal`] um das aktuelle Overlay anzuzeigen.
struct ModalOverlay<'a, 'e, Nachricht, Thema, R> {
    /// Das Element des Overlays.
    element: &'a mut Element<'e, Nachricht, Thema, R>,
    /// Der Zustand des Overlays.
    state: &'a mut Tree,
    /// Wird das Overlay geschlossen, wenn `Esc` gedrückt wird.
    passthrough_event: &'a dyn Fn(&Event) -> bool,
    /// Der viewport des Elements, wird für die [`Widget`]-Implementierung des overlays verwendet.
    viewport: Rectangle,
}

impl<'e, Nachricht, Thema, R> overlay::Overlay<Nachricht, Thema, R>
    for ModalOverlay<'_, 'e, Nachricht, Thema, R>
where
    Nachricht: 'e,
    R: 'e + Renderer,
    Thema: container::Catalog,
{
    fn layout(&mut self, renderer: &R, bounds: Size) -> layout::Node {
        let ModalOverlay { element, state, .. } = self;
        element.as_widget_mut().layout(state, renderer, &layout::Limits::new(bounds, bounds))
    }

    fn draw(
        &self,
        renderer: &mut R,
        theme: &Thema,
        style: &Style,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
    ) {
        let ModalOverlay { element, state, .. } = self;
        element.as_widget().draw(
            state,
            renderer,
            theme,
            style,
            layout,
            cursor_position,
            &layout.bounds(),
        );
    }

    fn operate(&mut self, layout: Layout<'_>, renderer: &R, operation: &mut dyn Operation) {
        let ModalOverlay { element, state, .. } = self;
        element.as_widget_mut().operate(state, layout, renderer, operation);
    }

    fn update(
        &mut self,
        event: &Event,
        layout: Layout<'_>,
        cursor: mouse::Cursor,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht>,
    ) {
        let ModalOverlay { element, state, passthrough_event, viewport } = self;
        if !passthrough_event(event) {
            element
                .as_widget_mut()
                .update(state, event, layout, cursor, renderer, clipboard, shell, viewport);
        }
    }

    fn mouse_interaction(
        &self,
        layout: Layout<'_>,
        cursor_position: mouse::Cursor,
        renderer: &R,
    ) -> mouse::Interaction {
        let ModalOverlay { element, state, viewport, .. } = self;
        element.as_widget().mouse_interaction(state, layout, cursor_position, viewport, renderer)
    }

    fn overlay<'a>(
        &'a mut self,
        layout: Layout<'a>,
        renderer: &R,
    ) -> Option<overlay::Element<'a, Nachricht, Thema, R>> {
        let ModalOverlay { element, state, viewport, .. } = self;
        element.as_widget_mut().overlay(state, layout, renderer, viewport, Vector { x: 0., y: 0. })
    }
}
