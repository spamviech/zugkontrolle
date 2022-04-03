//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use iced::{
    button,
    container::{Style, StyleSheet},
    Background, Color,
};

use crate::typen::farbe::Farbe;

#[derive(Debug, Clone, Copy)]
pub struct Beschreibung;
impl StyleSheet for Beschreibung {
    fn style(&self) -> Style {
        Style { text_color: Some(Color::BLACK), ..Default::default() }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Anzeige {
    Farbe(Farbe),
    Deaktiviert,
}
impl StyleSheet for Anzeige {
    fn style(&self) -> Style {
        match self {
            Anzeige::Farbe(farbe) => {
                Style { background: Some(Background::Color((*farbe).into())), ..Default::default() }
            },
            Anzeige::Deaktiviert => {
                Style { text_color: Some(Color::from_rgb(0.5, 0.5, 0.5)), ..Default::default() }
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Auswahl(pub Farbe);
impl StyleSheet for Auswahl {
    fn style(&self) -> Style {
        Style { background: Some(Background::Color(self.0.into())), ..Default::default() }
    }
}
impl button::StyleSheet for Auswahl {
    fn active(&self) -> button::Style {
        button::Style { background: Some(Background::Color(self.0.into())), ..Default::default() }
    }
}
