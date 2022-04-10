//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use iced::{
    button,
    container::{Style, StyleSheet},
    Background, Color,
};

use crate::typen::farbe::Farbe;

/// Style-Struktur für die Auswahl einer [Pcf8574-Beschreibung](crate::anschluss::pcf8574::Beschreibung).
#[derive(Debug, Clone, Copy)]
pub struct Beschreibung;

impl StyleSheet for Beschreibung {
    fn style(&self) -> Style {
        Style { text_color: Some(Color::BLACK), ..Default::default() }
    }
}

/// Hintergrund der [Anzeige](crate::application::streckenabschnitt::Anzeige)
/// basierend auf dem aktuellen Streckenabschnitt.
#[derive(Debug, Clone, Copy)]
pub enum Anzeige {
    /// Farbe des aktuellen Streckenabschnittes.
    Farbe(Farbe),
    /// Kein aktueller Streckenabschnitt.
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

/// Hintergrund der [Auswahl](crate::application::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
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
