//! Icons aus der Schriftart bootstrap-icons (https://icons.getbootstrap.com/).

use iced_core::{text, Element, Font, Renderer};
use iced_widget::Text;

use crate::application::fonts::BOOTSTRAP;

/// Icons aus der Schriftart bootstrap-icons (https://icons.getbootstrap.com/).
///
/// Der Varianten-Namen ist die CamelCase version des bootstrap-Namens in kebab-case.
/// Bisher sind nur aktuell verwendete Varianten definiert.
///
/// Es kann sein, dass in Zukunft die Fill-Variante bei einem alternativen
/// [Thema](crate::application::style::thema::Thema) angezeigt wird.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Bootstrap {
    Feather,
    ExclamationTriangle,
}

impl Bootstrap {
    fn als_str(self) -> &'static str {
        match self {
            Bootstrap::Feather => "\u{F7BF}",
            Bootstrap::ExclamationTriangle => "\u{F33B}",
        }
    }
}

/// Ein Widget, dass ein Icon darstellt.
#[allow(missing_debug_implementations)]
pub struct Icon<'a, R>(Text<'a, R>)
where
    R: text::Renderer,
    <R as Renderer>::Theme: iced_widget::text::StyleSheet;

impl<'a, R> Icon<'_, R>
where
    R: text::Renderer,
    <R as Renderer>::Theme: iced_widget::text::StyleSheet,
    <R as text::Renderer>::Font: From<Font>,
{
    /// Erzeuge ein neues Widget, dass das gewählte [Bootstrap]-Icon anzeigt.
    pub fn neu(bootstrap: Bootstrap) -> Icon<'a, R> {
        Icon(Text::new(bootstrap.als_str()).font(BOOTSTRAP))
    }
}

impl<'a, M, R> From<Icon<'a, R>> for Element<'a, M, R>
where
    R: 'a + text::Renderer,
    <R as Renderer>::Theme: iced_widget::text::StyleSheet,
{
    fn from(icon: Icon<'a, R>) -> Self {
        Element::new(icon.0)
    }
}
