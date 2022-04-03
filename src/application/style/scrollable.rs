//! Style-Strukturen für ein [iced::Scrollable].

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use iced::{
    scrollable::{Scrollbar, Scroller, StyleSheet},
    Color,
};

#[derive(Debug, Clone, Copy)]
pub struct Collection {
    width: u16,
}

impl Collection {
    pub fn new(width: u16) -> Self {
        Collection { width }
    }

    pub fn width(&self) -> u16 {
        self.width
    }

    fn scrollbar(&self, grey_value: f32) -> Scrollbar {
        let scroller_color = Color::from_rgb(grey_value, grey_value, grey_value);
        Scrollbar {
            background: None,
            border_radius: 0.,
            border_width: 0.,
            border_color: Color::BLACK,
            scroller: Scroller {
                color: scroller_color,
                border_radius: 0.25 * (self.width as f32),
                border_width: 0.,
                border_color: scroller_color,
            },
        }
    }
}

impl StyleSheet for Collection {
    fn active(&self) -> Scrollbar {
        self.scrollbar(0.7)
    }

    fn hovered(&self) -> Scrollbar {
        self.scrollbar(0.6)
    }

    fn dragging(&self) -> Scrollbar {
        self.scrollbar(0.5)
    }
}
