//! Macros zur einfacheren Implementierung des Widget-Traits.

/// Implementiere alle benötigten Methoden des [Widget](iced_native::Widget)-Traits für $type,
/// indem ein Widget unter $record mit identischer $message und $renderer verwendet wird.
macro_rules! widget_newtype_methods {
    ($type:ty, $record:tt, $message:ty, $renderer:ty $(,)?) => {
        #[inline(always)]
        #[allow(unused_qualifications)]
        fn width(&self) -> iced_native::Length {
            <$type as iced_pure::Widget<$message, $renderer>>::width(&self.$record)
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn height(&self) -> iced_native::Length {
            <$type as iced_pure::Widget<$message, $renderer>>::height(&self.$record)
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn layout(
            &self,
            renderer: &$renderer,
            limits: &iced_native::layout::Limits,
        ) -> iced_native::layout::Node {
            <$type as iced_pure::Widget<$message, $renderer>>::layout(
                &self.$record,
                renderer,
                limits,
            )
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn draw(
            &self,
            state: &iced_pure::widget::Tree,
            renderer: &mut $renderer,
            style: &iced_native::renderer::Style,
            layout: iced_native::layout::Layout<'_>,
            cursor_position: Point,
            viewport: &iced::Rectangle,
        ) {
            <$type as iced_pure::Widget<$message, $renderer>>::draw(
                &self.$record,
                state,
                renderer,
                style,
                layout,
                cursor_position,
                viewport,
            )
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn children(&self) -> Vec<iced_pure::widget::Tree> {
            vec![Tree::new(&self.$record)]
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn diff(&self, tree: &mut iced_pure::widget::Tree) {
            tree.diff_children(&[self.$record])
        }
    };
}
pub(crate) use widget_newtype_methods;
