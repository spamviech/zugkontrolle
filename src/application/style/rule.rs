//! Style Strukturen für eine iced::Rule

pub const SEPARATOR: Rule = Rule { color: iced::Color::BLACK, width: 1, radius: 0. };

#[derive(Debug, Clone, Copy)]
pub struct Rule {
    pub color: iced::Color,
    pub width: u16,
    pub radius: f32,
}
impl iced::rule::StyleSheet for Rule {
    fn style(&self) -> iced::rule::Style {
        let Rule { color, radius, width } = *self;
        iced::rule::Style { color, radius, width, fill_mode: iced::rule::FillMode::Full }
    }
}
