//! Style-Strukturen für ein [iced::widget::Scrollable].

use iced::{
    widget::scrollable::{Scrollbar, Scroller, StyleSheet},
    Color,
};

/// Style-Struktur für ein [iced::widget::Scrollable]
/// mit fester [Scroller-Breite](iced::widget::Scrollable::scroller_width).
#[derive(Debug, Clone, Copy)]
pub struct Sammlung {
    /// Die [Scroller-Breite](iced::widget::Scrollable::scroller_width).
    pub breite: u16,
}

impl Sammlung {
    /// Erstelle eine neue [Sammlung] Style-Struktur.
    pub fn neu(breite: u16) -> Self {
        Sammlung { breite }
    }

    /// Die [Scroller-Breite](iced::widget::Scrollable::scroller_width) des [iced::widget::Scrollable].
    #[inline(always)]
    pub fn breite(&self) -> u16 {
        self.breite
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
                border_radius: 0.25 * (self.breite as f32),
                border_width: 0.,
                border_color: scroller_color,
            },
        }
    }
}

impl StyleSheet for Sammlung {
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
