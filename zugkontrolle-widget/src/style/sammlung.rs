//! Style-Strukturen für ein [`iced::widget::Scrollable`].

use iced::{
    Color,
    border::{self, Border},
    widget::{
        container,
        scrollable::{self, Scrollbar, Scroller},
    },
};
use iced_widget::scrollable::Rail;

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
}

impl From<Sammlung> for scrollable::Style {
    fn from(value: Sammlung) -> Self {
        // TODO: Re-introduce Theme handling
        // Thema::Hell => 0.7,
        // Thema::Dunkel => 0.3,
        let grey_value = 0.7;
        let rail = Rail {
            background: None,
            border: Border { radius: border::Radius::from(0.), width: 0., color: Color::BLACK },
            scroller: Scroller {
                border: Border {
                    radius: border::Radius::from(0.25 * value.breite),
                    width: 0.,
                    color: Color::from_rgb(grey_value, grey_value, grey_value),
                },
                background: Default::default(),
            },
        };
        scrollable::Style { vertical_rail: rail, ..Default::default() }
    }
}
