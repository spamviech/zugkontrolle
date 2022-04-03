//! Style Strukturen für den Rand eines [iced::Container].

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]


use iced::{container, Color};

#[derive(Debug, Clone, Copy)]
pub struct Border {
    pub border_color: Color,
    pub border_width: f32,
    pub border_radius: f32,
}
impl container::StyleSheet for Border {
    fn style(&self) -> container::Style {
        let Border { border_color, border_radius, border_width } = *self;
        container::Style { border_color, border_width, border_radius, ..Default::default() }
    }
}
