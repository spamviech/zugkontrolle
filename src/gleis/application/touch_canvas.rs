//! iced::Canvas-Wrapper mit Touch-Event debug messages

use iced::canvas::Program;
use iced_graphics::{backend::Backend, Renderer};
use iced_native::{
    event,
    layout,
    overlay,
    Clipboard,
    Element,
    Hasher,
    Layout,
    Length,
    Point,
    Rectangle,
    Widget,
};
use log::debug;

pub struct Canvas<Message, P: Program<Message>>(iced::Canvas<Message, P>);

impl<Message, P: Program<Message>> Canvas<Message, P> {
    pub fn new(program: P) -> Self {
        Canvas(iced::Canvas::new(program))
    }

    pub fn width(self, width: Length) -> Self {
        Canvas(self.0.width(width))
    }

    pub fn height(self, height: Length) -> Self {
        Canvas(self.0.height(height))
    }
}

impl<Message, P: Program<Message>, B: Backend> Widget<Message, Renderer<B>> for Canvas<Message, P> {
    fn width(&self) -> Length {
        <iced::Canvas<Message, P> as Widget<Message, Renderer<B>>>::width(&self.0)
    }

    fn height(&self) -> Length {
        <iced::Canvas<Message, P> as Widget<Message, Renderer<B>>>::height(&self.0)
    }

    fn layout(&self, renderer: &Renderer<B>, limits: &layout::Limits) -> layout::Node {
        Widget::layout(&self.0, renderer, limits)
    }

    fn hash_layout(&self, state: &mut Hasher) {
        <iced::Canvas<Message, P> as Widget<Message, Renderer<B>>>::hash_layout(&self.0, state)
    }

    fn draw(
        &self,
        renderer: &mut Renderer<B>,
        defaults: &<Renderer<B> as iced_native::Renderer>::Defaults,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) -> <Renderer<B> as iced_native::Renderer>::Output {
        <iced::Canvas<Message, P> as Widget<Message, Renderer<B>>>::draw(
            &self.0,
            renderer,
            defaults,
            layout,
            cursor_position,
            viewport,
        )
    }

    fn on_event(
        &mut self,
        event: iced_native::Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Message>,
    ) -> event::Status {
        if let iced_native::Event::Touch(touch) = event {
            debug!("{:?}", touch);
        }
        Widget::on_event(&mut self.0, event, layout, cursor_position, renderer, clipboard, messages)
    }

    fn overlay(
        &mut self,
        layout: Layout<'_>,
    ) -> Option<overlay::Element<'_, Message, Renderer<B>>> {
        Widget::overlay(&mut self.0, layout)
    }
}

impl<'a, Message, P: Program<Message>, B> From<Canvas<Message, P>> for Element<'a, Message, B>
where
    iced::Canvas<Message, P>: Into<Element<'a, Message, B>>,
{
    fn from(Canvas(canvas): Canvas<Message, P>) -> Self {
        canvas.into()
    }
}
