//! Style Strukturen für eine [iced::Rule].

use iced::{
    rule::{FillMode, Style, StyleSheet},
    Color,
};

pub const SEPARATOR: Rule = Rule { color: Color::BLACK, width: 1, radius: 0. };

#[derive(Debug, Clone, Copy)]
pub struct Rule {
    pub color: Color,
    pub width: u16,
    pub radius: f32,
}
impl StyleSheet for Rule {
    fn style(&self) -> Style {
        let Rule { color, radius, width } = *self;
        Style { color, radius, width, fill_mode: FillMode::Full }
    }
}
