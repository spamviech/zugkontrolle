//! Style Strukturen für eine [`iced::widget::Rule`].

use iced_core::Color;
use iced_widget::rule;

use crate::style::thema::Thema;

#[doc(inline)]
pub use rule::{Style, StyleFn};

/// Style-Struktur für eine Trennlinie.
pub const TRENNLINIE: Linie = Linie { farbe: None, breite: 1, radius: 0. };

/// Eine Linie mit gegebener Farbe, Breite und Radius an den Enden.
#[derive(Debug, Clone, Copy, Default)]
pub struct Linie {
    /// Die Farbe der Linie.
    pub farbe: Option<Color>,
    /// Die Breite der Linie.
    pub breite: u16,
    /// Der Radius an den Enden.
    pub radius: f32,
}

/// Erlaube Verwendung in [`iced_widget::Rule::style`]).
pub trait StyleProvider<'a, Thema>
where
    Thema: rule::Catalog<Class<'a> = StyleFn<'a, Thema>>,
{
    /// Gebe die styling function mit den aktuell Einstellungen zurück.
    #[must_use]
    fn style_fn(self) -> StyleFn<'static, Thema>;
}

impl StyleProvider<'_, Thema> for Linie {
    fn style_fn(self) -> StyleFn<'static, Thema> {
        Box::new(|thema| {
            let default_style = <Thema as rule::Catalog>::default()(thema);
            let color = match thema {
                Thema::Hell => Color::BLACK,
                Thema::Dunkel => Color::WHITE,
            };
            Style { color, ..default_style }
        })
    }
}
