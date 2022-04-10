//! Style-Strukturen für ein [iced::Scrollable].

use iced::{
    scrollable::{Scrollbar, Scroller, StyleSheet},
    Color,
};

/// Style-Struktur für ein [iced::Scrollable] mit fester Breite.
#[derive(Debug, Clone, Copy)]
pub struct Sammlung {
    /// Die Breite des [iced::Scrollable].
    pub breite: u16,
}

impl Sammlung {
    /// Erstelle eine neue [Sammlung] Style-Struktur.
    pub fn neu(breite: u16) -> Self {
        Sammlung { breite }
    }

    /// Die Breite des [iced::Scrollable].
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
