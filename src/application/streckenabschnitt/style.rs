//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use crate::farbe::Farbe;

#[derive(Debug, Clone, Copy)]
pub struct Beschreibung;
impl iced::container::StyleSheet for Beschreibung {
    fn style(&self) -> iced::container::Style {
        iced::container::Style { text_color: Some(iced::Color::BLACK), ..Default::default() }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Anzeige {
    Farbe(Farbe),
    Deaktiviert,
}
impl iced::container::StyleSheet for Anzeige {
    fn style(&self) -> iced::container::Style {
        match self {
            Anzeige::Farbe(farbe) => iced::container::Style {
                background: Some(iced::Background::Color((*farbe).into())),
                ..Default::default()
            },
            Anzeige::Deaktiviert => iced::container::Style {
                text_color: Some(iced::Color::from_rgb(0.5, 0.5, 0.5)),
                ..Default::default()
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Auswahl(pub Farbe);
impl iced::container::StyleSheet for Auswahl {
    fn style(&self) -> iced::container::Style {
        iced::container::Style {
            background: Some(iced::Background::Color(self.0.into())),
            ..Default::default()
        }
    }
}
impl iced::button::StyleSheet for Auswahl {
    fn active(&self) -> iced::button::Style {
        iced::button::Style {
            background: Some(iced::Background::Color(self.0.into())),
            ..Default::default()
        }
    }
}
