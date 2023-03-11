//! Style Strukturen für die Hintergrund-Farbe eines [iced::widget::Container] oder [iced::widget::Button].

use iced::{
    widget::{button, container},
    Color, Theme,
};

/// Weißer Hintergrund.
#[allow(non_upper_case_globals)]
pub const WEIß: Hintergrund = Hintergrund::Grau(1.);
/// Schwarzer Hintergrund.
pub const SCHWARZ: Hintergrund = Hintergrund::Grau(0.);
/// Roter Hintergrund.
pub const ROT: Hintergrund = Hintergrund::Rot(0.7);
/// Grüner Hintergrund.
pub const GRÜN: Hintergrund = Hintergrund::Grün(0.7);
/// Blauer Hintergrund.
pub const BLAU: Hintergrund = Hintergrund::Blau(0.7);
/// Standard Hintergrund (leicht grau).
pub const STANDARD: Hintergrund = Hintergrund::Grau(0.85);

/// Style Strukturen für die Hintergrund-Farbe eines [iced::Container] oder [iced::widget::Button].
#[derive(Debug, Clone, Copy)]
pub enum Hintergrund {
    /// Ein grauer Hintergrund.
    Grau(f32),
    /// Ein roter Hintergrund.
    Rot(f32),
    /// Ein grüner Hintergrund.
    Grün(f32),
    /// Ein blauer Hintergrund.
    Blau(f32),
    /// Ein transparenter, grauer Hintergrund.
    GrauTransparent {
        /// Die Grau-Anteil, 0.=weiß, 1.=schwarz.
        grau: f32,
        /// Der Transparenz-Wert, 0.=unsichtbar, 1.=vollständig sichtbar.
        alpha: f32,
    },
}

impl Hintergrund {
    /// Erzeuge die entsprechende Hintergrundfarbe.
    pub fn color(&self) -> Color {
        match self {
            Hintergrund::Grau(grey) => Color::from_rgb(*grey, *grey, *grey),
            Hintergrund::Rot(red) => Color::from_rgb(*red, 0., 0.),
            Hintergrund::Grün(green) => Color::from_rgb(0., *green, 0.),
            Hintergrund::Blau(blue) => Color::from_rgb(0., 0., *blue),
            Hintergrund::GrauTransparent { grau, alpha } => {
                Color::from_rgba(*grau, *grau, *grau, *alpha)
            },
        }
    }
}

impl container::StyleSheet for Hintergrund {
    // TODO Style verwenden
    type Style = Theme;

    fn appearance(&self, style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(iced::Background::Color(self.color())),
            ..container::Appearance::default()
        }
    }
}
impl button::StyleSheet for Hintergrund {
    // TODO Style verwenden
    type Style = Theme;

    fn active(&self, style: &Self::Style) -> button::Appearance {
        button::Appearance {
            background: Some(iced::Background::Color(self.color())),
            ..button::Appearance::default()
        }
    }
}

impl From<Hintergrund> for iced::theme::Container {
    fn from(hintergrund: Hintergrund) -> Self {
        iced::theme::Container::Custom(Box::new(hintergrund))
    }
}

impl From<Hintergrund> for iced::theme::Button {
    fn from(hintergrund: Hintergrund) -> Self {
        iced::theme::Button::Custom(Box::new(hintergrund))
    }
}
