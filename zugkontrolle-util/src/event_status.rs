//! Internal type for tracking if an [`iced::Widget::update`] method should capture an event.

/// Internal type for tracking if an [`iced::Widget::update`] method should capture an event.
///
/// The type was removed from iced, it is re-introduced here to simplify the transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventStatus {
    /// Capture the event by calling [`iced_widget::action::Action::and_capture`].
    Captured,
    /// Allow other widgets to handle the Event.
    Ignored,
}
