//! Style-Strukturen für ein [`iced::widget::Scrollable`].

use iced::{
    border::{self, Border},
    widget::{
        container,
        scrollable::{Appearance, Scrollbar, Scroller, StyleSheet},
    },
    Color,
};

use crate::style::thema::Thema;

/// Style-Struktur für ein [`iced::widget::Scrollable`]
/// mit fester [`Scroller-Breite`](iced_native::widget::scrollable::Properties::scroller_width).
#[derive(Debug, Clone, Copy)]
pub struct Sammlung {
    /// Die [`Scroller-Breite`](iced_native::widget::scrollable::Properties::scroller_width).
    pub breite: f32,
}

impl Default for Sammlung {
    fn default() -> Self {
        Self { breite: 10. }
    }
}

impl Sammlung {
    /// Erstelle eine neue [`Sammlung`] Style-Struktur.
    #[must_use]
    pub fn neu(breite: f32) -> Self {
        Sammlung { breite }
    }

    /// Die [Scroller-Breite](iced_native::widget::scrollable::Properties::scroller_width) des [`Scrollable`](iced::widget::Scrollable).
    #[must_use]
    pub fn breite(&self) -> f32 {
        self.breite
    }

    /// Erhalte den [Scrollbar]-Style mit dem gegebenen `grey_value`.
    #[must_use]
    fn scrollbar(self, grey_value: f32) -> Scrollbar {
        let scroller_color = Color::from_rgb(grey_value, grey_value, grey_value);
        Scrollbar {
            background: None,
            border: Border { radius: border::Radius::from(0.), width: 0., color: Color::BLACK },
            scroller: Scroller {
                color: scroller_color,
                border: Border {
                    radius: border::Radius::from(0.25 * self.breite),
                    width: 0.,
                    color: scroller_color,
                },
            },
        }
    }

    /// [`Self::scrollbar`], mit `container` und `gap` als [`Default::default`]-Werten.
    #[must_use]
    fn appearance(self, grey_value: f32) -> Appearance {
        Appearance {
            scrollbar: self.scrollbar(grey_value),
            container: container::Appearance::default(),
            gap: None,
        }
    }
}

impl StyleSheet for Thema {
    type Style = Sammlung;

    fn active(&self, style: &Self::Style) -> Appearance {
        match self {
            Thema::Hell => style.appearance(0.7),
            Thema::Dunkel => style.appearance(0.3),
        }
    }

    fn hovered(&self, style: &Self::Style, _is_mouse_over_scrollbar: bool) -> Appearance {
        match self {
            Thema::Hell => style.appearance(0.6),
            Thema::Dunkel => style.appearance(0.4),
        }
    }

    fn dragging(&self, style: &Self::Style) -> Appearance {
        match self {
            Thema::Hell | Thema::Dunkel => style.appearance(0.5),
        }
    }
}
