//! Überdecke ein Widget einem anderen Widget.

use std::{fmt::Debug, hash::Hash};

use iced::{Rectangle, Size};
use iced_native::{
    container, event,
    keyboard::{self, KeyCode},
    layout, overlay, Clipboard, Container, Element, Event, Layout, Length, Overlay, Point,
    Renderer, Widget,
};

use crate::application::style::background::Background;

/// Zustand des [Modal]-Widgets.
#[derive(Debug)]
pub struct Zustand<Overlay> {
    overlay: Option<Overlay>,
}

impl<Overlay> Zustand<Overlay> {
    /// Erstelle einen neuen Zustand für das [Modal]-Widget.
    pub fn neu() -> Self {
        Zustand { overlay: None }
    }

    /// Zeige ein Overlay über dem Widget.
    #[inline(always)]
    pub fn zeige_modal(&mut self, overlay: Overlay) {
        self.overlay = Some(overlay);
    }

    /// Lösche das Overlay, so dass nur das originale Widget sichtbar ist.
    #[inline(always)]
    pub fn verstecke_modal(&mut self) {
        self.overlay = None;
    }

    /// Das aktuell gezeigte Overlay.
    #[inline(always)]
    pub fn overlay(&self) -> &Option<Overlay> {
        &self.overlay
    }

    /// Eine veränderliche Referenz auf das aktuelle Overlay.
    #[inline(always)]
    pub fn overlay_mut(&mut self) -> &mut Option<Overlay> {
        &mut self.overlay
    }
}

/// Ein Widget, dass ein Overlay vor einem anderen Widget anzeigen kann.
pub struct Modal<'a, Overlay, Nachricht, R> {
    zustand: &'a mut Zustand<Overlay>,
    underlay: Element<'a, Nachricht, R>,
    zeige_overlay: &'a dyn for<'t> Fn(&'t mut Overlay) -> Element<'t, Nachricht, R>,
    esc_nachricht: Option<&'a dyn Fn() -> Nachricht>,
}

impl<Overlay: Debug, Nachricht, R> Debug for Modal<'_, Overlay, Nachricht, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Modal")
            .field("zustand", &self.zustand)
            .field("underlay", &"<Element>")
            .field("zeige_overlay", &"<closure>")
            .field("esc_nachricht", &self.esc_nachricht.map(|_| "<closure>"))
            .finish()
    }
}

impl<'a, Overlay, Nachricht, R> Modal<'a, Overlay, Nachricht, R> {
    /// Erstelle ein neues [Modal].
    pub fn neu(
        zustand: &'a mut Zustand<Overlay>,
        underlay: impl Into<Element<'a, Nachricht, R>>,
        zeige_overlay: &'a impl for<'t> Fn(&'t mut Overlay) -> Element<'t, Nachricht, R>,
    ) -> Self {
        Modal { zustand, underlay: underlay.into(), zeige_overlay, esc_nachricht: None }
    }

    /// Erzeuge die gegebene Nachricht, wenn die Esc-Taste gedrückt wird.
    pub fn on_esc(mut self, esc_nachricht: &'a impl Fn() -> Nachricht) -> Self {
        self.esc_nachricht = Some(esc_nachricht);
        self
    }
}

impl<Overlay, Nachricht, R> Widget<Nachricht, R> for Modal<'_, Overlay, Nachricht, R>
where
    R: Renderer + container::Renderer,
    <R as container::Renderer>::Style: From<Background>,
{
    fn width(&self) -> Length {
        self.underlay.width()
    }

    fn height(&self) -> Length {
        self.underlay.height()
    }

    fn layout(&self, renderer: &R, limits: &layout::Limits) -> layout::Node {
        self.underlay.layout(renderer, limits)
    }

    fn draw(
        &self,
        renderer: &mut R,
        defaults: &R::Defaults,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) -> R::Output {
        self.underlay.draw(renderer, defaults, layout, cursor_position, viewport)
    }

    fn hash_layout(&self, zustand: &mut iced_native::Hasher) {
        self.zustand.overlay.is_some().hash(zustand);
        self.underlay.hash_layout(zustand)
    }

    fn overlay<'s>(&'s mut self, layout: Layout<'_>) -> Option<overlay::Element<'s, Nachricht, R>> {
        if let Some(overlay) = self.zustand.overlay_mut() {
            let bounds = layout.bounds();
            let position = Point::new(bounds.x, bounds.y);
            Some(ModalOverlay::neu((self.zeige_overlay)(overlay)).overlay(position))
        } else {
            self.underlay.overlay(layout)
        }
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Nachricht>,
    ) -> event::Status {
        if self.zustand.overlay.is_none() {
            self.underlay.on_event(event, layout, cursor_position, renderer, clipboard, messages)
        } else {
            match (self.esc_nachricht, event) {
                (
                    Some(esc_nachricht),
                    Event::Keyboard(keyboard::Event::KeyPressed {
                        key_code: KeyCode::Escape,
                        modifiers: _,
                    }),
                ) => {
                    messages.push(esc_nachricht());
                    event::Status::Captured
                },
                _ => event::Status::Ignored,
            }
        }
    }
}

impl<'a, Inner, Nachricht, R> From<Modal<'a, Inner, Nachricht, R>> for Element<'a, Nachricht, R>
where
    R: Renderer + container::Renderer,
    <R as container::Renderer>::Style: From<Background>,
{
    fn from(modal: Modal<'a, Inner, Nachricht, R>) -> Self {
        Element::new(modal)
    }
}

struct ModalOverlay<'a, Nachricht, R>(Element<'a, Nachricht, R>);

impl<'a, Nachricht: 'a, R> ModalOverlay<'a, Nachricht, R>
where
    R: Renderer + container::Renderer + 'a,
    <R as container::Renderer>::Style: From<Background>,
{
    fn neu(overlay: Element<'a, Nachricht, R>) -> Self {
        ModalOverlay(
            Container::new(overlay)
                .width(Length::Fill)
                .height(Length::Fill)
                .center_x()
                .center_y()
                .style(Background::GreyTransparent { grey: 0.7, alpha: 0.5 })
                .into(),
        )
    }
}

impl<'a, Nachricht: 'a, R: Renderer + 'a> ModalOverlay<'a, Nachricht, R> {
    fn overlay(self, position: Point) -> overlay::Element<'a, Nachricht, R> {
        overlay::Element::new(position, Box::new(self))
    }
}

impl<Nachricht, R: Renderer> Overlay<Nachricht, R> for ModalOverlay<'_, Nachricht, R> {
    fn layout(&self, renderer: &R, bounds: Size, position: Point) -> iced_native::layout::Node {
        let mut layout = self.0.layout(renderer, &layout::Limits::new(bounds, bounds));
        layout.move_to(position);
        layout
    }

    fn draw(
        &self,
        renderer: &mut R,
        defaults: &R::Defaults,
        layout: Layout<'_>,
        cursor_position: Point,
    ) -> R::Output {
        self.0.draw(renderer, defaults, layout, cursor_position, &layout.bounds())
    }

    fn hash_layout(&self, zustand: &mut iced_native::Hasher, position: Point) {
        format!("{:?}", position).hash(zustand);
        self.0.hash_layout(zustand);
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Nachricht>,
    ) -> event::Status {
        self.0.on_event(event, layout, cursor_position, renderer, clipboard, messages)
    }
}
