//! iced::Canvas-Wrapper mit Touch-Event debug messages

use iced_graphics::{backend::Backend, canvas::Program, Renderer};
use iced_native::{
    event,
    layout,
    mouse::{self, Button},
    overlay,
    touch,
    Clipboard,
    Element,
    Hasher,
    Layout,
    Length,
    Point,
    Rectangle,
    Widget,
};
use log::trace;

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
        mut event: iced_native::Event,
        layout: Layout<'_>,
        mut cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Message>,
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
        Widget::on_event(&mut self.0, event, layout, cursor_position, renderer, clipboard, messages)
    }

    fn overlay(
        &mut self,
        layout: Layout<'_>,
    ) -> Option<overlay::Element<'_, Message, Renderer<B>>> {
        Widget::overlay(&mut self.0, layout)
    }
}

impl<'a, Message, P, B> From<Canvas<Message, P>> for Element<'a, Message, Renderer<B>>
where
    Message: 'static,
    P: Program<Message> + 'a,
    B: Backend,
{
    fn from(canvas: Canvas<Message, P>) -> Element<'a, Message, Renderer<B>> {
        Element::new(canvas)
    }
}

pub mod wrapper {
    // TODO decide if switching to this is worth it
    pub use iced_graphics::canvas::{Cursor, Geometry};
    pub use iced_native::{Event, Rectangle};

    pub mod event {
        pub use iced_native::event::{Event, Status};
    }
    pub mod mouse {
        pub use iced_native::mouse::Interaction;
    }

    pub trait Program<Message> {
        fn draw(&self, bounds: Rectangle<f32>, cursor: Cursor) -> Vec<Geometry>;

        fn update(
            &mut self,
            _event: iced_native::Event,
            _bounds: Rectangle<f32>,
            _cursor: Cursor,
        ) -> (event::Status, Option<Message>) {
            (event::Status::Ignored, None)
        }

        fn mouse_interaction(
            &self,
            _bounds: Rectangle<f32>,
            _cursor: Cursor,
        ) -> mouse::Interaction {
            mouse::Interaction::default()
        }
    }

    struct Wrapper<P>(P);
    impl<Message, P: Program<Message>> iced_graphics::canvas::Program<Message> for Wrapper<P> {
        fn draw(&self, bounds: Rectangle, cursor: Cursor) -> Vec<Geometry> {
            self.0.draw(bounds, cursor)
        }

        fn update(
            &mut self,
            _event: iced_graphics::canvas::Event,
            _bounds: Rectangle<f32>,
            _cursor: Cursor,
        ) -> (event::Status, Option<Message>) {
            // event handled exclusively by custom trait
            (event::Status::Ignored, None)
        }

        fn mouse_interaction(&self, bounds: Rectangle<f32>, cursor: Cursor) -> mouse::Interaction {
            self.0.mouse_interaction(bounds, cursor)
        }
    }

    use iced_graphics::{backend::Backend, Renderer};
    use iced_native::{layout, overlay, Clipboard, Element, Hasher, Layout, Length, Point, Widget};

    pub struct Canvas<Message, P: Program<Message>>(iced::Canvas<Message, Wrapper<P>>);

    impl<Message, P: Program<Message>> Canvas<Message, P> {
        pub fn new(program: P) -> Self {
            Canvas(iced::Canvas::new(Wrapper(program)))
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
            <iced::Canvas<Message, Wrapper<P>> as Widget<Message, Renderer<B>>>::width(&self.0)
        }

        fn height(&self) -> Length {
            <iced::Canvas<Message, Wrapper<P>> as Widget<Message, Renderer<B>>>::height(&self.0)
        }

        fn layout(&self, renderer: &Renderer<B>, limits: &layout::Limits) -> layout::Node {
            Widget::layout(&self.0, renderer, limits)
        }

        fn hash_layout(&self, state: &mut Hasher) {
            <iced::Canvas<Message, Wrapper<P>> as Widget<Message, Renderer<B>>>::hash_layout(
                &self.0, state,
            )
        }

        fn draw(
            &self,
            renderer: &mut Renderer<B>,
            defaults: &<Renderer<B> as iced_native::Renderer>::Defaults,
            layout: Layout<'_>,
            cursor_position: Point,
            viewport: &Rectangle,
        ) -> <Renderer<B> as iced_native::Renderer>::Output {
            <iced::Canvas<Message, Wrapper<P>> as Widget<Message, Renderer<B>>>::draw(
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
            _event: iced_native::Event,
            layout: Layout<'_>,
            cursor_position: Point,
            _renderer: &Renderer<B>,
            _clipboard: &mut dyn Clipboard,
            _messages: &mut Vec<Message>,
        ) -> event::Status {
            let _bounds = layout.bounds();
            let _cursor = Cursor::Available(cursor_position);
            // TODO wie komme ich an den Wert im Canvas????
            todo!()
        }

        fn overlay(
            &mut self,
            layout: Layout<'_>,
        ) -> Option<overlay::Element<'_, Message, Renderer<B>>> {
            Widget::overlay(&mut self.0, layout)
        }
    }

    impl<'a, Message, P, B> From<Canvas<Message, P>> for Element<'a, Message, Renderer<B>>
    where
        Message: 'static,
        P: Program<Message> + 'a,
        B: Backend,
    {
        fn from(canvas: Canvas<Message, P>) -> Element<'a, Message, Renderer<B>> {
            Element::new(canvas)
        }
    }
}
