//! Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
//! bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).

use iced::{Background, Color, border::Radius};
use iced_aw::style::tab_bar;
use iced_core::border;

use crate::style::thema::Thema;

/// Style-Struktur für eine [`TabBar`](iced_aw::tab_bar::TabBar)
/// bei der Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug, Clone, Copy, Default)]
pub struct TabBar;

impl From<TabBar> for tab_bar::Style {
    fn from(TabBar: TabBar) -> Self {
        // TODO: Re-introduce Theme handling
        // Thema::Hell => {
        //     let grey_value = if is_active { 0.8 } else { 0.9 };
        //     Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
        // },
        // Thema::Dunkel => {
        //     let grey_value = if is_active { 0.2 } else { 0.1 };
        //     Self::Style::style(Color::from_rgb(grey_value, grey_value, grey_value))
        // },
        tab_bar::Style {
            background: Some(Background::Color(Color::WHITE)),
            border_color: Some(Color::BLACK),
            border_width: 0.,
            tab_label_border_color: Color::BLACK,
            tab_label_border_width: 1.,
            icon_color: Color::BLACK,
            icon_background: None,
            icon_border_radius: border::Radius::default(),
            text_color: Color::BLACK,
            ..Default::default()
        }
    }
}
