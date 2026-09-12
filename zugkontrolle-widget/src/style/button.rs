//! Style Strukturen für die Hintergrund-Farbe eines [`iced::widget::Button`].

use iced::{
    Background, Color,
    theme::{self, Theme},
    widget::button,
};

use crate::style::thema::Thema;

/// Weißer Hintergrund.
pub const WEIẞ: Button = Button::hintergrund_grau(1.);
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

impl From<Button> for button::Style {
    fn from(value: Button) -> Self {
        let mut style = button::Style::default();
        if let Button::Hintergrund { farbe } = value {
            style = style.with_background(Background::Color(farbe))
        }
        style
    }
}
