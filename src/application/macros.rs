//! Macros zur einfacheren Implementierung des Widget-Traits.

/// Implementiere alle benötigten Methoden des [Widget](iced_pure::Widget)-Traits,
/// indem ein [Element](iced_pure::Element) unter $record mit $renderer verwendet wird.
/// Wenn zusätzlich noch ein $message-Typ gegeben ist werden auch die Methoden
/// [on_event](iced_pure::Widget::on_event) und [overlay](iced_pure::Widget::overlay) implementiert.
macro_rules! widget_newtype_methods {
    ($record:tt, $renderer:ty $(,)?) => {
        widget_newtype_methods! {
            $record, $renderer
            => [width, height, layout, draw, children, diff, mouse_interaction]
        }
    };
    ($record:tt, $renderer:ty, $message:ty $(,)?) => {
        widget_newtype_methods! {
            $record, $renderer, $message
            => [width, height, layout, draw, children, diff, mouse_interaction, overlay, on_event]
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => []) => {
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [width $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn width(&self) -> iced_native::Length {
            self.$record.as_widget().width()
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [height $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn height(&self) -> iced_native::Length {
            self.$record.as_widget().height()
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [layout $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn layout(
            &self,
            renderer: &$renderer,
            limits: &iced_native::layout::Limits,
        ) -> iced_native::layout::Node {
            self.$record.as_widget().layout(renderer, limits)
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [draw $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

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
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [children $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn children(&self) -> Vec<iced_pure::widget::Tree> {
            vec![iced_pure::widget::Tree::new(&self.$record)]
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [diff $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn diff(&self, tree: &mut iced_pure::widget::Tree) {
            tree.diff_children(&[&self.$record])
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [mouse_interaction $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

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
                &state.children[0],
                layout,
                cursor_position,
                viewport,
                renderer,
            )
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [state $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn state(&self) -> iced_pure::widget::tree::State {
            iced_pure::widget::tree::State::None
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [tag $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn tag(&self) -> iced_pure::widget::tree::Tag {
            iced_pure::widget::tree::Tag::stateless()
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [overlay $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

        #[inline(always)]
        #[allow(unused_qualifications)]
        fn overlay<'macro_overlay_lt>(
            &'macro_overlay_lt self,
            state: &'macro_overlay_lt mut iced_pure::widget::Tree,
            layout: iced_native::Layout<'_>,
            renderer: &$renderer,
        ) -> Option<iced_native::overlay::Element<'macro_overlay_lt, $($message)?, $renderer>> {
            self.$record.as_widget().overlay(state, layout, renderer)
        }
    };
    ($record:tt, $renderer:ty $(, $message:ty)? => [on_event $(, $($methods: ident),+)?]) => {
        widget_newtype_methods! {$record, $renderer $(, $message)? => [$($($methods),+)?]}

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
            shell: &mut iced_native::Shell<'_, $($message)?>,
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
