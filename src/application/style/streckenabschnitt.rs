//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use iced::{
    widget::{
        button,
        container::{Appearance, StyleSheet},
    },
    Background, Color, Theme,
};

use crate::typen::farbe::Farbe;

/// Style-Struktur für die Auswahl einer [Pcf8574-Beschreibung](crate::anschluss::pcf8574::Beschreibung).
#[derive(Debug, Clone, Copy)]
pub struct Beschreibung;

impl StyleSheet for Beschreibung {
    // TODO Style verwenden
    type Style = Theme;
    fn appearance(&self, style: &Self::Style) -> Appearance {
        Appearance { text_color: Some(Color::BLACK), ..Appearance::default() }
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
    // TODO Style verwenden
    type Style = Theme;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        match self {
            Anzeige::Farbe(farbe) => Appearance {
                background: Some(Background::Color((*farbe).into())),
                ..Appearance::default()
            },
            Anzeige::Deaktiviert => Appearance {
                text_color: Some(Color::from_rgb(0.5, 0.5, 0.5)),
                ..Appearance::default()
            },
        }
    }
}

impl From<Anzeige> for iced::theme::Container {
    fn from(anzeige: Anzeige) -> Self {
        iced::theme::Container::Custom(Box::new(anzeige))
    }
}

/// Hintergrund der [Auswahl](crate::application::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
#[derive(Debug, Clone, Copy)]
pub struct Auswahl(pub Farbe);

impl StyleSheet for Auswahl {
    // TODO Style verwenden
    type Style = Theme;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        Appearance { background: Some(Background::Color(self.0.into())), ..Appearance::default() }
    }
}

impl button::StyleSheet for Auswahl {
    type Style = Theme;

    fn active(&self, style: &Self::Style) -> button::Appearance {
        button::Appearance {
            background: Some(Background::Color(self.0.into())),
            ..button::Appearance::default()
        }
    }
}

impl From<Auswahl> for iced::theme::Button {
    fn from(auswahl: Auswahl) -> Self {
        iced::theme::Button::Custom(Box::new(auswahl))
    }
}
