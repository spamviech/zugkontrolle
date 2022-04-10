//! Style Strukturen für eine [iced::Rule].

use iced::{
    rule::{FillMode, Style, StyleSheet},
    Color,
};

/// Style-Struktur für eine Trennlinie.
pub const TRENNLINIE: Linie = Linie { farbe: Color::BLACK, breite: 1, radius: 0. };

/// Eine Linie mit gegebener Farbe, Breite und Radius an den Enden.
#[derive(Debug, Clone, Copy)]
pub struct Linie {
    /// Die Farbe der Linie.
    pub farbe: Color,
    /// Die Breite der Linie.
    pub breite: u16,
    /// Der Radius an den Enden.
    pub radius: f32,
}

impl StyleSheet for Linie {
    fn style(&self) -> Style {
        let Linie { farbe: color, breite: width, radius } = *self;
        Style { color, radius, width, fill_mode: FillMode::Full }
    }
}
