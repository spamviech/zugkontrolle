//! Style Strukturen für die Hintergrund-Farbe eines [`iced::widget::Button`].

use iced::{
    theme::{self, Theme},
    widget::button,
    Background, Color,
};

use crate::application::style::thema::Thema;

/// Weißer Hintergrund.
#[allow(non_upper_case_globals)]
pub const WEIß: Button = Button::hintergrund_grau(1.);
/// Schwarzer Hintergrund.
pub const SCHWARZ: Button = Button::hintergrund_grau(0.);
/// Roter Hintergrund.
pub const ROT: Button = Button::hintergrund_rot(0.7);
/// Grüner Hintergrund.
pub const GRÜN: Button = Button::hintergrund_grün(0.7);
/// Blauer Hintergrund.
pub const BLAU: Button = Button::hintergrund_blau(0.7);

/// Style Strukturen für einen [`iced::widget::Button`].
#[derive(Debug, Clone, Copy, Default)]
pub enum Button {
    /// Die Standard-Darstellung des korrespondierenden [`iced::Theme`].
    #[default]
    Standard,
    /// Ändere die Hintergrundfarbe.
    Hintergrund {
        /// Die Hintergrundfarbe.
        farbe: Color,
    },
}

impl Button {
    /// Ein grauer Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_grau(grau: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(grau, grau, grau) }
    }

    /// Ein roter Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_rot(rot: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(rot, 0., 0.) }
    }

    /// Ein grüner Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_grün(grün: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(0., grün, 0.) }
    }

    /// Ein blauer Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_blau(blau: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(0., 0., blau) }
    }

    /// Ein grauer Hintergrund mit Transparenz.
    #[must_use]
    pub const fn hintergrund_grau_transparent(grau: f32, alpha: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgba(grau, grau, grau, alpha) }
    }
}

impl button::StyleSheet for Thema {
    type Style = Button;

    fn active(&self, style: &Self::Style) -> button::Appearance {
        match (self, style) {
            (Thema::Hell, Button::Standard) => {
                button::StyleSheet::active(&Theme::Light, &theme::Button::default())
            },
            (Thema::Dunkel, Button::Standard) => {
                button::StyleSheet::active(&Theme::Dark, &theme::Button::default())
            },
            (Thema::Hell | Thema::Dunkel, Button::Hintergrund { farbe }) => button::Appearance {
                background: Some(Background::Color(*farbe)),
                ..button::Appearance::default()
            },
        }
    }

    fn hovered(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::hovered(&Theme::Light, &theme::Button::default()),
            Thema::Dunkel => button::StyleSheet::hovered(&Theme::Dark, &theme::Button::default()),
        }
    }

    fn pressed(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::pressed(&Theme::Light, &theme::Button::default()),
            Thema::Dunkel => button::StyleSheet::pressed(&Theme::Dark, &theme::Button::default()),
        }
    }

    fn disabled(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::disabled(&Theme::Light, &theme::Button::default()),
            Thema::Dunkel => button::StyleSheet::disabled(&Theme::Dark, &theme::Button::default()),
        }
    }
}
