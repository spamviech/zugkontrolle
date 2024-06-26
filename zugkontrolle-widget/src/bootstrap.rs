//! Icons aus der Schriftart bootstrap-icons <https://icons.getbootstrap.com/>.

use iced_core::{text as text_core, Element, Font};
use iced_widget::{text, Text};

use crate::fonts::BOOTSTRAP;

/// Icons aus der Schriftart bootstrap-icons <https://icons.getbootstrap.com/>.
///
/// Der Varianten-Namen ist die CamelCase version des bootstrap-Namens in kebab-case.
/// Bisher sind nur aktuell verwendete Varianten definiert.
///
/// Es kann sein, dass in Zukunft die Fill-Variante bei einem alternativen
/// [`Thema`](crate::style::thema::Thema) angezeigt wird.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Bootstrap {
    Feather,
    FileEarmark,
    Floppy,
    ExclamationTriangle,
    Trash,
}

impl Bootstrap {
    /// Erzeuge einen [&str](str), das den [char] für das gewünschte Symbol enthält.
    fn als_str(self) -> &'static str {
        match self {
            Bootstrap::Feather => "\u{F7BF}",
            Bootstrap::FileEarmark => "\u{f392}",
            Bootstrap::Floppy => "\u{f7d8}",
            Bootstrap::ExclamationTriangle => "\u{F33B}",
            Bootstrap::Trash => "\u{F5DE}",
        }
    }
}

/// Ein Widget, dass ein Icon darstellt.
///
/// Damit das Icon richtig angezeigt wird muss vorher die [BOOTSTRAP]-Schriftart [`geladen`](iced::font::load) werden.
#[allow(missing_debug_implementations)]
pub struct Icon<'a, Thema, R>(Text<'a, Thema, R>)
where
    R: text_core::Renderer,
    Thema: text::StyleSheet;

impl<'a, Thema, R> Icon<'_, Thema, R>
where
    R: text_core::Renderer,
    Thema: text::StyleSheet,
    <R as text_core::Renderer>::Font: From<Font>,
{
    /// Erzeuge ein neues Widget, dass das gewählte [`Bootstrap`]-Icon anzeigt.
    #[must_use]
    pub fn neu(bootstrap: Bootstrap) -> Icon<'a, Thema, R> {
        Icon(Text::new(bootstrap.als_str()).font(BOOTSTRAP))
    }
}

impl<'a, M, Thema, R> From<Icon<'a, Thema, R>> for Element<'a, M, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a + text::StyleSheet,
{
    fn from(icon: Icon<'a, Thema, R>) -> Self {
        Element::new(icon.0)
    }
}
