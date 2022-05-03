//! Macros zur einfacheren Implementierung des Widget-Traits.

macro_rules! reexport_no_event_methods {
    ($type:ty, $record:tt, $message:ty, $renderer:ty) => {
        #[inline(always)]
        #[allow(unused_qualifications)]
        fn width(&self) -> iced_native::Length {
            <$type as iced_native::Widget<$message, $renderer>>::width(&self.$record)
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn height(&self) -> iced_native::Length {
            <$type as iced_native::Widget<$message, $renderer>>::height(&self.$record)
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn layout(
            &self,
            renderer: &$renderer,
            limits: &iced_native::layout::Limits,
        ) -> iced_native::layout::Node {
            <$type as iced_native::Widget<$message, $renderer>>::layout(
                &self.$record,
                renderer,
                limits,
            )
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn draw(
            &self,
            renderer: &mut $renderer,
            style: &iced_native::renderer::Style,
            layout: Layout<'_>,
            cursor_position: Point,
            viewport: &iced_native::Rectangle,
        ) {
            <$type as iced_native::Widget<$message, $renderer>>::draw(
                &self.$record,
                renderer,
                style,
                layout,
                cursor_position,
                viewport,
            )
        }
    };
}
pub(crate) use reexport_no_event_methods;

struct Test;

impl<M, R: iced_native::Renderer> iced_native::Widget<M, R> for Test {
    fn width(&self) -> iced::Length {
        todo!()
    }

    fn height(&self) -> iced::Length {
        todo!()
    }

    fn layout(
        &self,
        renderer: &R,
        limits: &iced_native::layout::Limits,
    ) -> iced_native::layout::Node {
        todo!()
    }

    fn draw(
        &self,
        renderer: &mut R,
        style: &iced_native::renderer::Style,
        layout: iced_native::Layout<'_>,
        cursor_position: iced::Point,
        viewport: &iced::Rectangle,
    ) {
        todo!()
    }

    fn mouse_interaction(
        &self,
        _layout: iced_native::Layout<'_>,
        _cursor_position: iced::Point,
        _viewport: &iced::Rectangle,
        _renderer: &R,
    ) -> iced_native::mouse::Interaction {
        iced_native::mouse::Interaction::Idle
    }

    fn overlay(
        &mut self,
        _layout: iced_native::Layout<'_>,
        _renderer: &R,
    ) -> Option<iced_native::overlay::Element<'_, M, R>> {
        None
    }

    fn on_event(
        &mut self,
        _event: iced_native::Event,
        _layout: iced_native::Layout<'_>,
        _cursor_position: iced::Point,
        _renderer: &R,
        _clipboard: &mut dyn iced_native::Clipboard,
        _shell: &mut iced_native::Shell<'_, M>,
    ) -> iced_native::event::Status {
        iced_native::event::Status::Ignored
    }
}
