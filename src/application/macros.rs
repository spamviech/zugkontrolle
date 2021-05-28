//! Macros zur einfacheren Implementierung des Widget-Traits.

macro_rules! reexport_no_event_methods {
    ($type:ty, $record:ident, $message:ty, $renderer:ty) => {
        fn width(&self) -> Length {
            <$type as Widget<$message, $renderer>>::width(&self.$record)
        }

        fn height(&self) -> Length {
            <$type as Widget<$message, $renderer>>::height(&self.$record)
        }

        fn layout(
            &self,
            renderer: &$renderer,
            limits: &iced_native::layout::Limits,
        ) -> iced_native::layout::Node {
            <$type as Widget<$message, $renderer>>::layout(&self.$record, renderer, limits)
        }

        fn draw(
            &self,
            renderer: &mut $renderer,
            defaults: &<$renderer as iced_native::Renderer>::Defaults,
            layout: iced_native::Layout<'_>,
            cursor_position: iced_native::Point,
            viewport: &iced_native::Rectangle,
        ) -> <$renderer as iced_native::Renderer>::Output {
            <$type as Widget<$message, $renderer>>::draw(
                &self.$record,
                renderer,
                defaults,
                layout,
                cursor_position,
                viewport,
            )
        }

        fn hash_layout(&self, state: &mut iced_native::Hasher) {
            <$type as Widget<$message, $renderer>>::hash_layout(&self.$record, state)
        }
    };
}
pub(crate) use reexport_no_event_methods;
