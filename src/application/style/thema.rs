//! Unterstützte Graphik-Themen.

use iced::{
    application::{Appearance, StyleSheet},
    widget::{checkbox, radio, slider, text, text_input},
};
use iced_aw::{card, number_input};

/// Unterstützte Graphik-Themen, sehr nah am built-in [iced::Theme].
#[derive(Debug, Clone, Copy, Default)]
pub enum Thema {
    /// Die helle Variante.
    #[default]
    Hell,
    // /// Die dunkle Variante.
    // Dunkel,
}

impl StyleSheet for Thema {
    type Style = <iced::Theme as StyleSheet>::Style;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        match self {
            Thema::Hell => StyleSheet::appearance(&iced::Theme::Light, style),
        }
    }
}

impl checkbox::StyleSheet for Thema {
    type Style = <iced::Theme as checkbox::StyleSheet>::Style;

    fn active(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Thema::Hell => checkbox::StyleSheet::active(&iced::Theme::Light, style, is_checked),
        }
    }

    fn hovered(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Thema::Hell => checkbox::StyleSheet::hovered(&iced::Theme::Light, style, is_checked),
        }
    }
}

impl text::StyleSheet for Thema {
    type Style = <iced::Theme as text::StyleSheet>::Style;

    fn appearance(&self, style: Self::Style) -> text::Appearance {
        match self {
            Thema::Hell => text::StyleSheet::appearance(&iced::Theme::Light, style),
        }
    }
}

impl text_input::StyleSheet for Thema {
    type Style = <iced::Theme as text_input::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::active(&iced::Theme::Light, style),
        }
    }

    fn focused(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::focused(&iced::Theme::Light, style),
        }
    }

    fn placeholder_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::placeholder_color(&iced::Theme::Light, style),
        }
    }

    fn value_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::value_color(&iced::Theme::Light, style),
        }
    }

    fn disabled_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::disabled_color(&iced::Theme::Light, style),
        }
    }

    fn selection_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Thema::Hell => text_input::StyleSheet::selection_color(&iced::Theme::Light, style),
        }
    }

    fn disabled(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::disabled(&iced::Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Thema::Hell => text_input::StyleSheet::hovered(&iced::Theme::Light, style),
        }
    }
}

impl card::StyleSheet for Thema {
    type Style = <iced::Theme as card::StyleSheet>::Style;

    fn active(&self, style: Self::Style) -> card::Appearance {
        match self {
            Thema::Hell => card::StyleSheet::active(&iced::Theme::Light, style),
        }
    }
}

impl slider::StyleSheet for Thema {
    type Style = <iced::Theme as slider::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> iced_native::widget::vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::active(&iced::Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> iced_native::widget::vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::hovered(&iced::Theme::Light, style),
        }
    }

    fn dragging(&self, style: &Self::Style) -> iced_native::widget::vertical_slider::Appearance {
        match self {
            Thema::Hell => slider::StyleSheet::dragging(&iced::Theme::Light, style),
        }
    }
}

impl radio::StyleSheet for Thema {
    type Style = <iced::Theme as radio::StyleSheet>::Style;

    fn active(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
        match self {
            Thema::Hell => radio::StyleSheet::active(&iced::Theme::Light, style, is_selected),
        }
    }

    fn hovered(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
        match self {
            Thema::Hell => radio::StyleSheet::hovered(&iced::Theme::Light, style, is_selected),
        }
    }
}

impl number_input::StyleSheet for Thema {
    type Style = <iced::Theme as number_input::StyleSheet>::Style;

    fn active(&self, style: Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::active(&iced::Theme::Light, style),
        }
    }

    fn pressed(&self, style: Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::pressed(&iced::Theme::Light, style),
        }
    }

    fn disabled(&self, style: Self::Style) -> number_input::Appearance {
        match self {
            Thema::Hell => number_input::StyleSheet::disabled(&iced::Theme::Light, style),
        }
    }
}
