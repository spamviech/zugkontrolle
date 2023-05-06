//! Style Strukturen für die Hintergrund-Farbe eines [iced::widget::Container].

use iced::{widget::container, Color};

use crate::application::style::thema::Thema;

/// Weißer Hintergrund.
#[allow(non_upper_case_globals)]
pub const WEIß: Container = Container::hintergrund_grau(1.);
/// Schwarzer Hintergrund.
pub const SCHWARZ: Container = Container::hintergrund_grau(0.);
/// Roter Hintergrund.
pub const ROT: Container = Container::hintergrund_rot(0.7);
/// Grüner Hintergrund.
pub const GRÜN: Container = Container::hintergrund_grün(0.7);
/// Blauer Hintergrund.
pub const BLAU: Container = Container::hintergrund_blau(0.7);

/// Style Strukturen für einen [iced::widget::Container].
#[derive(Debug, Clone, Copy, Default)]
pub enum Container {
    /// Die Standard-Darstellung des korrespondierenden [iced::Theme].
    #[default]
    Standard,
    /// Ändere die Hintergrundfarbe.
    Hintergrund { farbe: Color },
    /// Zeige einen Rand in der spezifizierten Darstellung.
    Rand {
        /// Die Farbe des Rands.
        farbe: Color,
        /// Die Breite des Rands.
        breite: f32,
        /// Radius der abgerundeten Ecken.
        radius: f32,
    },
}

impl Container {
    /// Ein grauer Hintergrund ohne Transparenz.
    pub fn hintergrund_grau(grau: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(grau, grau, grau) }
    }

    /// Ein roter Hintergrund ohne Transparenz.
    pub fn hintergrund_rot(rot: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(rot, 0., 0.) }
    }

    /// Ein grüner Hintergrund ohne Transparenz.
    pub fn hintergrund_grün(grün: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(0., grün, 0.) }
    }

    /// Ein blauer Hintergrund ohne Transparenz.
    pub fn hintergrund_blau(blau: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(0., 0., blau) }
    }

    /// Ein grauer Hintergrund mit Transparenz.
    pub fn hintergrund_grau_transparent(grau: f32, alpha: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgba(grau, grau, grau, alpha) }
    }
}

impl container::StyleSheet for Thema {
    type Style = Container;

    fn appearance(&self, style: &Self::Style) -> container::Appearance {
        match (self, style) {
            (Thema::Hell, Container::Standard) => {
                container::StyleSheet::appearance(&iced::Theme::Light, &Default::default())
            },
            (Thema::Hell, Container::Hintergrund { farbe }) => container::Appearance {
                background: Some(iced::Background::Color(*farbe)),
                ..container::Appearance::default()
            },
            (Thema::Hell, Container::Rand { farbe, breite, radius }) => container::Appearance {
                border_color: *farbe,
                border_width: *breite,
                border_radius: *radius,
                ..container::Appearance::default()
            },
        }
    }
}
