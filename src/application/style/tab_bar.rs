//! Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
//! bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).

use iced::{Background, Color};
use iced_aw::style::tab_bar::{Appearance, StyleSheet};
use iced_core::BorderRadius;

use crate::application::style::thema::Thema;

/// Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
/// bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug, Clone, Copy, Default)]
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
            icon_background: None,
            icon_border_radius: BorderRadius::default(),
            text_color: Color::BLACK,
        }
    }
}

impl StyleSheet for Thema {
    type Style = TabBar;

    fn active(&self, style: &Self::Style, is_active: bool) -> Appearance {
        match self {
            Thema::Hell => {
                let grey_value: f32;
                if is_active {
                    grey_value = 0.8;
                } else {
                    grey_value = 0.9;
                }
                style.style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
        }
    }

    fn hovered(&self, style: &Self::Style, _is_active: bool) -> Appearance {
        match self {
            Thema::Hell => {
                let grey_value = 0.7;
                style.style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
        }
    }
}
