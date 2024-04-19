//! Hilfs-Typ für [`Widget::operate`](iced_core::Widget::operate) mit unterschiedlichen Nachricht-Typen.

use std::any::Any;

use iced_core::{
    widget::{self, operation::Operation},
    Rectangle, Vector,
};

/// Hilfs-Typ für [`Widget::operate`](iced_core::Widget::operate) mit unterschiedlichen Nachricht-Typen.
/// Kopiert von [`iced_core`](https://docs.rs/iced_core/latest/src/iced_core/element.rs.html#322)
pub(crate) struct MapOperation<'a, B> {
    /// [Operation]
    pub(crate) operation: &'a mut dyn Operation<B>,
}

impl<T, B> Operation<T> for MapOperation<'_, B> {
    fn container(
        &mut self,
        id: Option<&widget::Id>,
        bounds: Rectangle,
        operate_on_children: &mut dyn FnMut(&mut dyn Operation<T>),
    ) {
        self.operation.container(id, bounds, &mut |operation| {
            operate_on_children(&mut MapOperation { operation });
        });
    }

    fn focusable(&mut self, state: &mut dyn widget::operation::Focusable, id: Option<&widget::Id>) {
        self.operation.focusable(state, id);
    }

    fn scrollable(
        &mut self,
        state: &mut dyn widget::operation::Scrollable,
        id: Option<&widget::Id>,
        bounds: Rectangle,
        translation: Vector,
    ) {
        self.operation.scrollable(state, id, bounds, translation);
    }

    fn text_input(
        &mut self,
        state: &mut dyn widget::operation::TextInput,
        id: Option<&widget::Id>,
    ) {
        self.operation.text_input(state, id);
    }

    fn custom(&mut self, state: &mut dyn Any, id: Option<&widget::Id>) {
        self.operation.custom(state, id);
    }
}
