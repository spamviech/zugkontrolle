//! Style-Strukturen für ein [`iced::widget::Scrollable`].

use iced::{
    widget::scrollable::{Scrollbar, Scroller, StyleSheet},
    BorderRadius, Color,
};

use crate::application::style::thema::Thema;

/// Style-Struktur für ein [`iced::widget::Scrollable`]
/// mit fester [`Scroller-Breite`](iced_native::widget::scrollable::Properties::scroller_width).
#[derive(Debug, Clone, Copy)]
pub struct Sammlung {
    /// Die [`Scroller-Breite`](iced_native::widget::scrollable::Properties::scroller_width).
    pub breite: f32,
}

impl Default for Sammlung {
    fn default() -> Self {
        Self { breite: 10. }
    }
}

impl Sammlung {
    /// Erstelle eine neue [`Sammlung`] Style-Struktur.
    #[must_use]
    pub fn neu(breite: f32) -> Self {
        Sammlung { breite }
    }

    /// Die [Scroller-Breite](iced_native::widget::scrollable::Properties::scroller_width) des [`Scrollable`](iced::widget::Scrollable).
    #[must_use]
    pub fn breite(&self) -> f32 {
        self.breite
    }

    /// Erhalte den [Scrollbar]-Style mit dem gegebenen `grey_value`.
    fn scrollbar(self, grey_value: f32) -> Scrollbar {
        let scroller_color = Color::from_rgb(grey_value, grey_value, grey_value);
        Scrollbar {
            background: None,
            border_radius: BorderRadius::from(0.),
            border_width: 0.,
            border_color: Color::BLACK,
            scroller: Scroller {
                color: scroller_color,
                border_radius: BorderRadius::from(0.25 * self.breite),
                border_width: 0.,
                border_color: scroller_color,
            },
        }
    }
}

impl StyleSheet for Thema {
    type Style = Sammlung;

    fn active(&self, style: &Self::Style) -> Scrollbar {
        match self {
            Thema::Hell => style.scrollbar(0.7),
            Thema::Dunkel => style.scrollbar(0.3),
        }
    }

    fn hovered(&self, style: &Self::Style, _is_mouse_over_scrollbar: bool) -> Scrollbar {
        match self {
            Thema::Hell => style.scrollbar(0.6),
            Thema::Dunkel => style.scrollbar(0.4),
        }
    }

    fn dragging(&self, style: &Self::Style) -> Scrollbar {
        match self {
            Thema::Hell | Thema::Dunkel => style.scrollbar(0.5),
        }
    }
}
