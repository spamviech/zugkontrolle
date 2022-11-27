//! [iced::Canvas]-Wrapper mit Touch-Event.

use std::fmt::{self, Debug, Formatter};

use iced_graphics::{backend::Backend, widget::canvas::Program, Renderer};
use iced_native::{
    event,
    mouse::{self, Button},
    touch,
    widget::tree::Tree,
    Clipboard, Element, Layout, Length, Point, Shell, Widget,
};
use log::trace;

use crate::application::macros::widget_newtype_methods;

// FIXME remove, canvas now supports touch events
/// [iced::Canvas]-Wrapper mit Touch-Event.
pub struct Canvas<'a, Message, R>(Element<'a, Message, R>);

impl<Message, R> Debug for Canvas<'_, Message, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Canvas").field(&"<Element>").finish()
    }
}

impl<'t, Message: 't, B: Backend, T> Canvas<'t, Message, Renderer<B, T>> {
    /// Erstelle einen neuen [Canvas].
    pub fn new<P: 't + Program<Message>>(
        program: P,
        width: impl Into<Option<Length>>,
        height: impl Into<Option<Length>>,
    ) -> Self {
        let mut canvas = iced::widget::Canvas::new(program);
        if let Some(width) = width.into() {
            canvas = canvas.height(width);
        }
        if let Some(height) = height.into() {
            canvas = canvas.height(height);
        }
        Canvas(canvas.into())
    }
}

impl<Message, R: iced_native::Renderer> Widget<Message, R> for Canvas<'_, Message, R> {
    widget_newtype_methods! {
        0, R, Message
        => [width, height, layout, draw, children, diff, mouse_interaction, overlay]
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        mut event: iced_native::Event,
        layout: Layout<'_>,
        mut cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
    ) -> event::Status {
        if let iced_native::Event::Touch(touch_event) = event {
            match touch_event {
                touch::Event::FingerPressed { id, position } => {
                    trace!("FingerPressed {{ id: {:?}, position: {:?} }}", id, position);
                    event = iced_native::Event::Mouse(mouse::Event::ButtonPressed(Button::Left));
                    cursor_position = position;
                },
                touch::Event::FingerLifted { id, position } => {
                    trace!("FingerLifted {{ id: {:?}, position: {:?} }}", id, position);
                    event = iced_native::Event::Mouse(mouse::Event::ButtonReleased(Button::Left));
                    cursor_position = position;
                },
                touch::Event::FingerLost { id, position } => {
                    trace!("FingerLost {{ id: {:?}, position: {:?} }}", id, position);
                    event = iced_native::Event::Mouse(mouse::Event::ButtonReleased(Button::Left));
                    cursor_position = position;
                },
                touch::Event::FingerMoved { id, position } => {
                    trace!("FingerMoved {{ id: {:?}, position: {:?} }}", id, position);
                    event = iced_native::Event::Mouse(mouse::Event::CursorMoved { position });
                    cursor_position = position;
                },
            }
        }
        self.0.as_widget_mut().on_event(
            &mut state.children[0],
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            shell,
        )
    }
}

impl<'a, Message: 'a, R: 'a> From<Canvas<'a, Message, R>> for Element<'a, Message, R> {
    fn from(canvas: Canvas<'a, Message, R>) -> Self {
        Element::new(canvas)
    }
}
