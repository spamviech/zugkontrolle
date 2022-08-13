//! Macros zur einfacheren Implementierung des Widget-Traits.

/// Implementiere alle benötigten Methoden des [Widget](iced_pure::Widget)-Traits,
/// indem ein [Element](iced_pure::Element) unter $record mit $renderer verwendet wird.
/// Wenn zusätzlich noch ein $message-Typ gegeben ist wird auch die
/// [on_event](iced_pure::Widget::on_event) implementiert wird.
macro_rules! widget_newtype_methods {
    ($record:tt, $renderer:ty $(,)?) => {
        #[inline(always)]
        #[allow(unused_qualifications)]
        fn width(&self) -> iced_native::Length {
            self.$record.as_widget().width()
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn height(&self) -> iced_native::Length {
            self.$record.as_widget().height()
        }

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn layout(
            &self,
            renderer: &$renderer,
            limits: &iced_native::layout::Limits,
        ) -> iced_native::layout::Node {
            self.$record.as_widget().layout(renderer, limits)
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
            self.$record.as_widget().draw(state, renderer, style, layout, cursor_position, viewport)
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

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn mouse_interaction(
            &self,
            state: &iced_pure::widget::Tree,
            layout: iced_native::Layout<'_>,
            cursor_position: iced::Point,
            viewport: &iced::Rectangle,
            renderer: &$renderer,
        ) -> iced_native::mouse::Interaction {
            self.$record.as_widget().mouse_interaction(
                state,
                layout,
                cursor_position,
                viewport,
                renderer,
            )
        }
    };
    ($record:tt, $renderer:ty, $message: ty $(,)?) => {
        widget_newtype_methods! {$record, $renderer}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn on_event(
            &mut self,
            state: &mut iced_pure::widget::Tree,
            event: iced_native::Event,
            layout: iced_native::Layout<'_>,
            cursor_position: iced::Point,
            renderer: &$renderer,
            clipboard: &mut dyn iced_native::Clipboard,
            shell: &mut iced_native::Shell<'_, $message>,
        ) -> iced_native::event::Status {
            self.$record.as_widget_mut().on_event(
                &mut state.children[0],
                event,
                layout,
                cursor_position,
                renderer,
                clipboard,
                shell,
            )
        }
    };
}
pub(crate) use widget_newtype_methods;
