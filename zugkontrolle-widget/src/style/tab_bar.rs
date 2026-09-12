//! Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
//! bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).

use iced_aw::style::{self, tab_bar};
use iced_core::{Background, Color};

use crate::style::thema::Thema;

/// Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
/// bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug, Clone, Copy, Default)]
pub struct TabBar;

impl TabBar {
    #[must_use]
    pub fn style(&self) -> style::status::StyleFn<'_, Thema, tab_bar::Style> {
        Box::new(|thema, status| {
            let default_style = <Thema as tab_bar::Catalog>::default()(thema, status);
            use style::Status;
            let grey_value = match (thema, status) {
                (Thema::Hell, Status::Active) => 0.8,
                (Thema::Hell, Status::Hovered | Status::Focused | Status::Selected) => 0.85,
                (Thema::Hell, Status::Pressed) => 0.9,
                (Thema::Hell | Thema::Dunkel, Status::Disabled) => 0.5,
                (Thema::Dunkel, Status::Active) => 0.2,
                (Thema::Dunkel, Status::Hovered | Status::Focused | Status::Selected) => 0.15,
                (Thema::Dunkel, Status::Pressed) => 0.1,
            };
            tab_bar::Style {
                border_width: 0.,
                tab_label_border_width: 1.,
                tab_label_background: Background::Color(Color::from_rgb(
                    grey_value, grey_value, grey_value,
                )),
                icon_background: None,
                ..default_style
            }
        })
    }
}
