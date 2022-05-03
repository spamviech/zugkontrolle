//! Macros zur einfacheren Implementierung des Widget-Traits.

/// Implementiere alle benötigten Methoden des [Widget](iced_native::Widget)-Traits für $type,
/// indem ein Widget unter $record mit identischer $message und $renderer verwendet wird.
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
