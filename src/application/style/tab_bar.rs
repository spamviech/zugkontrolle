//! Style-Strukturen zur Auswahl eines Anschlusses.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use iced::{Background, Color};

#[derive(Debug, Clone, Copy)]
pub struct TabBar;

impl TabBar {
    fn style(&self, tab_label_background: Color) -> iced_aw::tab_bar::Style {
        iced_aw::tab_bar::Style {
            background: Some(Background::Color(Color::WHITE)),
            border_color: Some(Color::BLACK),
            border_width: 0.,
            tab_label_background: Background::Color(tab_label_background),
            tab_label_border_color: Color::BLACK,
            tab_label_border_width: 1.,
            icon_color: Color::BLACK,
            text_color: Color::BLACK,
        }
    }
}

impl iced_aw::tab_bar::StyleSheet for TabBar {
    fn active(&self, is_active: bool) -> iced_aw::tab_bar::Style {
        let grey_value: f32;
        if is_active {
            grey_value = 0.8;
        } else {
            grey_value = 0.9;
        }
        self.style(Color::from_rgb(grey_value, grey_value, grey_value))
    }

    fn hovered(&self, _is_active: bool) -> iced_aw::tab_bar::Style {
        let grey_value = 0.7;
        self.style(Color::from_rgb(grey_value, grey_value, grey_value))
    }
}
