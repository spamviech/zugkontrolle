//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

#[derive(Debug, Clone)]
pub enum Anzeige {
    Farbe(iced::Color),
    Deaktiviert,
}
impl iced::container::StyleSheet for Anzeige {
    fn style(&self) -> iced::container::Style {
        match self {
            Anzeige::Farbe(farbe) => iced::container::Style {
                background: Some(iced::Background::Color(*farbe)),
                ..Default::default()
            },
            Anzeige::Deaktiviert => iced::container::Style {
                text_color: Some(iced::Color::from_rgb(0.5, 0.5, 0.5)),
                ..Default::default()
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Auswahl(pub iced::Color);
impl iced::button::StyleSheet for Auswahl {
    fn active(&self) -> iced::button::Style {
        iced::button::Style {
            background: Some(iced::Background::Color(self.0)),
            ..Default::default()
        }
    }
}
