//! Style Strukturen für den Rand eines iced::Container

pub struct Border {
    pub border_color: iced::Color,
    pub border_width: f32,
    pub border_radius: f32,
}
impl iced::container::StyleSheet for Border {
    fn style(&self) -> iced::container::Style {
        let Border { border_color, border_radius, border_width } = *self;
        iced::container::Style { border_color, border_width, border_radius, ..Default::default() }
    }
}
