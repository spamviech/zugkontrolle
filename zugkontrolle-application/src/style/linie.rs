//! Style Strukturen für eine [`iced::widget::Rule`].

use iced::{
    border,
    widget::rule::{Appearance, FillMode, StyleSheet},
    Color,
};

use crate::style::thema::Thema;

/// Style-Struktur für eine Trennlinie.
pub const TRENNLINIE: Linie = Linie { farbe: None, breite: 1, radius: 0. };

/// Eine Linie mit gegebener Farbe, Breite und Radius an den Enden.
#[derive(Debug, Clone, Copy, Default)]
pub struct Linie {
    /// Die Farbe der Linie.
    pub farbe: Option<Color>,
    /// Die Breite der Linie.
    pub breite: u16,
    /// Der Radius an den Enden.
    pub radius: f32,
}

impl StyleSheet for Thema {
    type Style = Linie;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        let Linie { farbe, breite: width, radius } = *style;
        let color = farbe.unwrap_or(match self {
            Thema::Hell => Color::BLACK,
            Thema::Dunkel => Color::WHITE,
        });
        Appearance { color, radius: border::Radius::from(radius), width, fill_mode: FillMode::Full }
    }
}
