//! Style-Strukturen zur Anzeige und Auswahl eines Streckenabschnittes.

use iced::{
    theme::{self, Theme},
    widget::{
        button,
        container::{Appearance, StyleSheet},
    },
    Background, Color,
};

use crate::typen::farbe::Farbe;

/// Style-Struktur für die Auswahl einer [Pcf8574-Beschreibung](crate::anschluss::pcf8574::Beschreibung).
#[derive(Debug, Clone, Copy)]
pub struct Beschreibung;

impl StyleSheet for Beschreibung {
    type Style = ();
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

/// Hintergrund der [Auswahl](crate::application::streckenabschnitt::Auswahl) eines Streckenabschnittes
/// zeigt die gewählte Farbe an.
#[derive(Debug, Clone, Copy)]
pub struct Auswahl(pub Farbe);

impl StyleSheet for Auswahl {
    type Style = ();

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
