//! [iced::Canvas]-Wrapper mit Touch-Event.

use std::fmt::{self, Debug, Formatter};

use iced_graphics::{backend::Backend, pure::canvas::Program, Renderer};
use iced_native::{
    event,
    mouse::{self, Button},
    touch, Clipboard, Layout, Length, Point, Shell,
};
use iced_pure::{widget::tree::Tree, Element, Widget};
use log::trace;

use crate::application::macros::widget_newtype_methods;

/// [iced::Canvas]-Wrapper mit Touch-Event.
pub struct Canvas<'a, P, Message, R> {
    element: Element<'a, Message, R>,
    program: P,
    width: Option<Length>,
    height: Option<Length>,
}

impl<P: Debug, Message, R> Debug for Canvas<'_, P, Message, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Canvas")
            .field("element", &"<Element>")
            .field("program", &self.program)
            .field("width", &self.width)
            .field("height", &self.height)
            .finish()
    }
}

impl<P: Program<Message>, Message, B: Backend> Canvas<'_, P, Message, Renderer<B>> {
    /// Erstelle einen neuen [Canvas].
    pub fn new(program: P) -> Self {
        let mut canvas = iced::pure::widget::Canvas::new(&program);
        Canvas { element: canvas.into(), program, width: None, height: None }
    }

    pub fn width(mut self, width: Length) -> Self {
        let mut canvas = iced::pure::widget::Canvas::new(&self.program).width(width);
        self.width = Some(width);
        if let Some(height) = self.height {
            canvas = canvas.height(height);
        }
        self.element = canvas.into();
        self
    }

    pub fn height(mut self, height: Length) -> Self {
        let mut canvas = iced::pure::widget::Canvas::new(&self.program).height(height);
        self.height = Some(height);
        if let Some(width) = self.width {
            canvas = canvas.height(width);
        }
        self.element = canvas.into();
        self
    }
}

impl<P, Message, R> Widget<Message, R> for Canvas<'_, P, Message, R> {
    widget_newtype_methods! {
        element, R, Message
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
        self.element.as_widget_mut().on_event(
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

impl<'a, P, Message, R> From<Canvas<'a, P, Message, R>> for Element<'a, Message, R> {
    fn from(canvas: Canvas<'a, P, Message, R>) -> Self {
        Element::new(canvas)
    }
}
