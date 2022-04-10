//! Style Strukturen für den Rand eines [iced::Container].

use iced::{
    container::{Style, StyleSheet},
    Color,
};

/// Style Strukturen für den Rand eines [iced::Container].
#[derive(Debug, Clone, Copy)]
pub struct Rand {
    /// Die Farbe des Rands.
    pub farbe: Color,
    /// Die Breite des Rands.
    pub breite: f32,
    /// Radius der abgerundeten Ecken.
    pub radius: f32,
}

impl StyleSheet for Rand {
    fn style(&self) -> Style {
        let Rand { farbe: border_color, breite: border_width, radius: border_radius } = *self;
        Style { border_color, border_width, border_radius, ..Default::default() }
    }
}
