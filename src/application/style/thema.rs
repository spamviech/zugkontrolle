//! Unterstützte Graphik-Themen.

use iced::{
    application::{Appearance, StyleSheet},
    widget::{checkbox, radio, slider, text, text_input},
    Theme,
};
use iced_aw::{card, number_input};
use iced_widget::vertical_slider;

use crate::typen::farbe::Farbe;

/// Unterstützte Graphik-Themen, sehr nah am built-in [`iced::Theme`].
#[derive(Debug, Clone, Copy, Default)]
pub enum Thema {
    /// Die helle Variante.
    #[default]
    Hell,
    // /// Die dunkle Variante.
    // Dunkel,
}

impl Thema {
    /// Die Farbe für generische Striche (z.B. Text).
    #[must_use]
    pub fn strich(&self) -> Farbe {
        match self {
            Thema::Hell => Farbe { rot: 0., grün: 0., blau: 0. },
        }
    }

    /// Die Farbe für Hintergrund eines Widgets.
    #[must_use]
    pub fn hintergrund(&self, aktiv: bool, in_bounds: bool) -> Farbe {
        let grey_value = match self {
            Thema::Hell if aktiv => 0.5,
            Thema::Hell if in_bounds => 0.7,
            Thema::Hell => 0.9,
        };
        Farbe { rot: grey_value, grün: grey_value, blau: grey_value }
    }
}

impl StyleSheet for Thema {
    type Style = <Theme as StyleSheet>::Style;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        match self {
            Thema::Hell => StyleSheet::appearance(&Theme::Light, style),
        }
    }
}

impl checkbox::StyleSheet for Thema {
    type Style = <Theme as checkbox::StyleSheet>::Style;

    fn active(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Thema::Hell => checkbox::StyleSheet::active(&Theme::Light, style, is_checked),
        }
    }

    fn hovered(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Thema::Hell => checkbox::StyleSheet::hovered(&Theme::Light, style, is_checked),
        }
    }
}

impl text::StyleSheet for Thema {
    type Style = <Theme as text::StyleSheet>::Style;

    fn appearance(&self, style: Self::Style) -> text::Appearance {
        match self {
            Thema::Hell => text::StyleSheet::appearance(&Theme::Light, style),
        }
    }
}

impl text_input::StyleSheet for Thema {
    type Style = <Theme as text_input::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::active(&Theme::Light, style),
        }
    }

    fn focused(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::focused(&Theme::Light, style),
        }
    }

    fn placeholder_color(&self, style: &Self::Style) -> iced_core::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::placeholder_color(&Theme::Light, style),
        }
    }

    fn value_color(&self, style: &Self::Style) -> iced_core::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::value_color(&Theme::Light, style),
        }
    }

    fn disabled_color(&self, style: &Self::Style) -> iced_core::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::disabled_color(&Theme::Light, style),
        }
    }

    fn selection_color(&self, style: &Self::Style) -> iced_core::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::selection_color(&Theme::Light, style),
        }
    }

    fn disabled(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::disabled(&Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::hovered(&Theme::Light, style),
        }
    }
}

impl card::StyleSheet for Thema {
    type Style = <Theme as card::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> card::Appearance {
        match self {
            Thema::Hell => card::StyleSheet::active(&Theme::Light, style),
        }
    }
}

impl slider::StyleSheet for Thema {
    type Style = <Theme as slider::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::active(&Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::hovered(&Theme::Light, style),
        }
    }

    fn dragging(&self, style: &Self::Style) -> vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::dragging(&Theme::Light, style),
        }
    }
}

impl radio::StyleSheet for Thema {
    type Style = <Theme as radio::StyleSheet>::Style;

    fn active(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
        match self {
            Thema::Hell => radio::StyleSheet::active(&Theme::Light, style, is_selected),
        }
    }

    fn hovered(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
        match self {
            Thema::Hell => radio::StyleSheet::hovered(&Theme::Light, style, is_selected),
        }
    }
}

impl number_input::StyleSheet for Thema {
    type Style = <Theme as number_input::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::active(&Theme::Light, style),
        }
    }

    fn pressed(&self, style: &Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::pressed(&Theme::Light, style),
        }
    }

    fn disabled(&self, style: &Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::disabled(&Theme::Light, style),
        }
    }
}
