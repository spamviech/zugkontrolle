//! Style Strukturen für eine [`iced::widget::Rule`].

use iced::{
    Color, border,
    widget::rule::{self, FillMode},
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

impl From<Linie> for rule::Style {
    fn from(value: Linie) -> Self {
        // TODO: Re-introduce Theme handling
        // Thema::Hell => Color::BLACK,
        // Thema::Dunkel => Color::WHITE,
        let color = Color::BLACK;
        rule::Style { color, ..Default::default() }
    }
}
