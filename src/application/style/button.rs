//! Style Strukturen für die Hintergrund-Farbe eines [iced::widget::Button].

use iced::{widget::button, Color};

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

/// Style Strukturen für einen [iced::widget::Button].
#[derive(Debug, Clone, Copy, Default)]
pub enum Button {
    /// Die Standard-Darstellung des korrespondierenden [iced::Theme].
    #[default]
    Standard,
    /// Ändere die Hintergrundfarbe.
    Hintergrund { farbe: Color },
}

impl Button {
    /// Ein grauer Hintergrund ohne Transparenz.
    pub fn hintergrund_grau(grau: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(grau, grau, grau) }
    }

    /// Ein roter Hintergrund ohne Transparenz.
    pub fn hintergrund_rot(rot: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(rot, 0., 0.) }
    }

    /// Ein grüner Hintergrund ohne Transparenz.
    pub fn hintergrund_grün(grün: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(0., grün, 0.) }
    }

    /// Ein blauer Hintergrund ohne Transparenz.
    pub fn hintergrund_blau(blau: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgb(0., 0., blau) }
    }

    /// Ein grauer Hintergrund mit Transparenz.
    pub fn hintergrund_grau_transparent(grau: f32, alpha: f32) -> Button {
        Button::Hintergrund { farbe: Color::from_rgba(grau, grau, grau, alpha) }
    }
}

impl button::StyleSheet for Thema {
    type Style = Button;

    fn active(&self, style: &Self::Style) -> button::Appearance {
        match (self, style) {
            (Thema::Hell, Button::Standard) => {
                button::StyleSheet::active(&iced::Theme::Light, &Default::default())
            },
            (Thema::Hell, Button::Hintergrund { farbe }) => button::Appearance {
                background: Some(iced::Background::Color(*farbe)),
                ..button::Appearance::default()
            },
        }
    }

    fn hovered(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::hovered(&iced::Theme::Light, &Default::default()),
        }
    }

    fn pressed(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::pressed(&iced::Theme::Light, &Default::default()),
        }
    }

    fn disabled(&self, _style: &Self::Style) -> button::Appearance {
        match self {
            Thema::Hell => button::StyleSheet::disabled(&iced::Theme::Light, &Default::default()),
        }
    }
}
