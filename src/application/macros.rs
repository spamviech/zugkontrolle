//! Macros zur einfacheren Implementierung des Widget-Traits.

macro_rules! reexport_no_event_methods {
    ($type:ty, $record:ident, $message:ty, $renderer:ty) => {
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
            defaults: &<$renderer as iced_native::Renderer>::Defaults,
            layout: Layout<'_>,
            cursor_position: Point,
            viewport: &iced_native::Rectangle,
        ) -> <$renderer as iced_native::Renderer>::Output {
            <$type as iced_native::Widget<$message, $renderer>>::draw(
                &self.$record,
                renderer,
                defaults,
                layout,
                cursor_position,
                viewport,
            )
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn hash_layout(&self, zustand: &mut iced_native::Hasher) {
            <$type as iced_native::Widget<$message, $renderer>>::hash_layout(&self.$record, zustand)
        }
    };
}
pub(crate) use reexport_no_event_methods;
