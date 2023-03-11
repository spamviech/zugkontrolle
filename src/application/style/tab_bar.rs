//! Style-Struktur für eine [TabBar](iced_aw::tab_bar::TabBar)
//! bei der Auswahl eines [Anschlusses](crate::anschluss::Anschluss).

use iced::{Background, Color, Theme};
use iced_aw::style::tab_bar::{Appearance, StyleSheet};

/// Style-Struktur für eine [TabBar](iced_aw::tab_bar::TabBar)
/// bei der Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug, Clone, Copy)]
pub struct TabBar;

impl TabBar {
    fn style(&self, tab_label_background: Color) -> Appearance {
        Appearance {
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

impl StyleSheet for TabBar {
    // TODO Style verwenden
    type Style = Theme;

    fn active(&self, _style: Self::Style, is_active: bool) -> Appearance {
        let grey_value: f32;
        if is_active {
            grey_value = 0.8;
        } else {
            grey_value = 0.9;
        }
        self.style(Color::from_rgb(grey_value, grey_value, grey_value))
    }

    fn hovered(&self, _style: Self::Style, _is_active: bool) -> Appearance {
        let grey_value = 0.7;
        self.style(Color::from_rgb(grey_value, grey_value, grey_value))
    }
}
