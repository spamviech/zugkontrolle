//! Style Strukturen für den Rand eines [iced::widget::Container].

use iced::{
    widget::container::{Appearance, StyleSheet},
    Color,
};

/// Style Strukturen für den Rand eines [iced::widget::Container].
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
    type Style = ();

    fn appearance(&self, style: &Self::Style) -> Appearance {
        let Rand { farbe: border_color, breite: border_width, radius: border_radius } = *self;
        Appearance { border_color, border_width, border_radius, ..Appearance::default() }
    }
}
