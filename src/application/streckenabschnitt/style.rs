//! Style-Strukturen zur Anzeige eines Streckenabschnittes.

pub enum Style {
    Farbe(iced::Color),
    Deaktiviert,
}
impl iced::container::StyleSheet for Style {
    fn style(&self) -> iced::container::Style {
        match self {
            Style::Farbe(farbe) => iced::container::Style {
                background: Some(iced::Background::Color(*farbe)),
                ..Default::default()
            },
            Style::Deaktiviert => iced::container::Style {
                text_color: Some(iced::Color::from_rgb(0.5, 0.5, 0.5)),
                ..Default::default()
            },
        }
    }
}
