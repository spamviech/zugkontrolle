//! Style Strukturen für die Hintergrund-Farbe eines [`iced::widget::Container`].

use iced_core::{
    Background, Color,
    border::{self, Border},
    theme::Base as _,
};
use iced_widget::container;

use crate::style::thema::Thema;

#[doc(inline)]
pub use container::{Style, StyleFn};

/// Weißer Hintergrund.
pub const WEIẞ: Container = Container::hintergrund_grau(1.);
/// Schwarzer Hintergrund.
pub const SCHWARZ: Container = Container::hintergrund_grau(0.);
/// Roter Hintergrund.
pub const ROT: Container = Container::hintergrund_rot(0.7);
/// Grüner Hintergrund.
pub const GRÜN: Container = Container::hintergrund_grün(0.7);
/// Blauer Hintergrund.
pub const BLAU: Container = Container::hintergrund_blau(0.7);

/// Style Strukturen für einen [`iced::widget::Container`].
#[derive(Debug, Clone, Copy, Default)]
pub enum Container {
    /// Die Standard-Darstellung des korrespondierenden [`iced::Theme`].
    #[default]
    Standard,
    /// Ändere die Hintergrundfarbe ausgehend vom Thema.
    HintergrundThema,
    /// Ändere die Hintergrundfarbe.
    Hintergrund {
        /// Die Hintergrundfarbe.
        farbe: Color,
    },
    /// Zeige einen Rand in der spezifizierten Darstellung.
    Rand {
        /// Die Farbe des Rands.
        farbe: Color,
        /// Die Breite des Rands.
        breite: f32,
        /// Radius der abgerundeten Ecken.
        radius: border::Radius,
    },
    /// Style-Struktur für die Auswahl einer [`Pcf8574-Beschreibung`](crate::anschluss::pcf8574::Beschreibung).
    Pcf8574Beschreibung,
}

impl Container {
    /// Ein grauer Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_grau(grau: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(grau, grau, grau) }
    }

    /// Ein roter Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_rot(rot: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(rot, 0., 0.) }
    }

    /// Ein grüner Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_grün(grün: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(0., grün, 0.) }
    }

    /// Ein blauer Hintergrund ohne Transparenz.
    #[must_use]
    pub const fn hintergrund_blau(blau: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgb(0., 0., blau) }
    }

    /// Ein grauer Hintergrund mit Transparenz.
    #[must_use]
    pub const fn hintergrund_grau_transparent(grau: f32, alpha: f32) -> Container {
        Container::Hintergrund { farbe: Color::from_rgba(grau, grau, grau, alpha) }
    }
}

/// Erlaube Verwendung in [`iced_widget::Container::style`]).
pub trait StyleProvider<'a, Thema>
where
    Thema: container::Catalog<Class<'a> = StyleFn<'a, Thema>>,
{
    /// Gebe die styling function mit den aktuell Einstellungen zurück.
    #[must_use]
    fn style_fn(self) -> StyleFn<'static, Thema>;
}

impl StyleProvider<'_, Thema> for Container {
    fn style_fn(self) -> StyleFn<'static, Thema> {
        Box::new(move |thema| {
            let default_style = <Thema as container::Catalog>::default()(thema);
            match self {
                Container::Standard => default_style,
                Container::HintergrundThema => {
                    default_style.background(thema.base().background_color)
                },
                Container::Hintergrund { farbe } => {
                    default_style.background(Background::Color(farbe))
                },
                Container::Rand { farbe, breite, radius } => {
                    default_style.border(Border { color: farbe, width: breite, radius })
                },
                Container::Pcf8574Beschreibung => default_style.color(thema.base().text_color),
            }
        })
    }
}
