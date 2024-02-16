//! Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
//! bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).

use iced::{Background, Color};
use iced_aw::style::tab_bar::{Appearance, StyleSheet};
use iced_core::BorderRadius;

use crate::style::thema::Thema;

/// Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
/// bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug, Clone, Copy, Default)]
pub struct TabBar;

impl TabBar {
    /// Erhalte die `TabBar`-[`Appearance`] für die gegebene Hintergrund-Farbe.
    fn style(tab_label_background: Color) -> Appearance {
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

    fn active(&self, _style: &Self::Style, is_active: bool) -> Appearance {
        match self {
            Thema::Hell => {
                let grey_value = if is_active { 0.8 } else { 0.9 };
                Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
            Thema::Dunkel => {
                let grey_value = if is_active { 0.2 } else { 0.1 };
                Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
        }
    }

    fn hovered(&self, _style: &Self::Style, _is_active: bool) -> Appearance {
        match self {
            Thema::Hell => {
                let grey_value = 0.7;
                Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
            Thema::Dunkel => {
                let grey_value = 0.3;
                Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
            },
        }
    }
}
