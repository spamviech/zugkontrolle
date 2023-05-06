//! Unterstützte Graphik-Modi.

use iced::{
    application::{Appearance, StyleSheet},
    widget::{button, checkbox, container, text, text_input},
};
use iced_aw::card;

/// Unterstützte Graphik-Modi, sehr nah am built-in [iced::Theme].
#[derive(Debug, Clone, Copy, Default)]
pub enum Theme2 {
    /// The light variant.
    #[default]
    Light,
    // /// The dark variant.
    // Dark,
}

impl StyleSheet for Theme2 {
    type Style = <iced::Theme as StyleSheet>::Style;

    fn appearance(&self, style: &Self::Style) -> Appearance {
        match self {
            Theme2::Light => StyleSheet::appearance(&iced::Theme::Light, style),
        }
    }
}

impl container::StyleSheet for Theme2 {
    type Style = <iced::Theme as container::StyleSheet>::Style;

    fn appearance(&self, style: &Self::Style) -> container::Appearance {
        match self {
            Theme2::Light => container::StyleSheet::appearance(&iced::Theme::Light, style),
        }
    }
}

impl button::StyleSheet for Theme2 {
    type Style = <iced::Theme as button::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> button::Appearance {
        match self {
            Theme2::Light => button::StyleSheet::active(&iced::Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> button::Appearance {
        match self {
            Theme2::Light => button::StyleSheet::hovered(&iced::Theme::Light, style),
        }
    }

    fn pressed(&self, style: &Self::Style) -> button::Appearance {
        match self {
            Theme2::Light => button::StyleSheet::pressed(&iced::Theme::Light, style),
        }
    }

    fn disabled(&self, style: &Self::Style) -> button::Appearance {
        match self {
            Theme2::Light => button::StyleSheet::disabled(&iced::Theme::Light, style),
        }
    }
}

impl checkbox::StyleSheet for Theme2 {
    type Style = <iced::Theme as checkbox::StyleSheet>::Style;

    fn active(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Theme2::Light => checkbox::StyleSheet::active(&iced::Theme::Light, style, is_checked),
        }
    }

    fn hovered(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
        match self {
            Theme2::Light => checkbox::StyleSheet::hovered(&iced::Theme::Light, style, is_checked),
        }
    }
}

impl text::StyleSheet for Theme2 {
    type Style = <iced::Theme as text::StyleSheet>::Style;

    fn appearance(&self, style: Self::Style) -> text::Appearance {
        match self {
            Theme2::Light => text::StyleSheet::appearance(&iced::Theme::Light, style),
        }
    }
}

impl text_input::StyleSheet for Theme2 {
    type Style = <iced::Theme as text_input::StyleSheet>::Style;

    fn active(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Theme2::Light => text_input::StyleSheet::active(&iced::Theme::Light, style),
        }
    }

    fn focused(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Theme2::Light => text_input::StyleSheet::focused(&iced::Theme::Light, style),
        }
    }

    fn placeholder_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Theme2::Light => text_input::StyleSheet::placeholder_color(&iced::Theme::Light, style),
        }
    }

    fn value_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Theme2::Light => text_input::StyleSheet::value_color(&iced::Theme::Light, style),
        }
    }

    fn disabled_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Theme2::Light => text_input::StyleSheet::disabled_color(&iced::Theme::Light, style),
        }
    }

    fn selection_color(&self, style: &Self::Style) -> iced_native::Color {
        match self {
            Theme2::Light => text_input::StyleSheet::selection_color(&iced::Theme::Light, style),
        }
    }

    fn disabled(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Theme2::Light => text_input::StyleSheet::disabled(&iced::Theme::Light, style),
        }
    }

    fn hovered(&self, style: &Self::Style) -> text_input::Appearance {
        match self {
            Theme2::Light => text_input::StyleSheet::hovered(&iced::Theme::Light, style),
        }
    }
}

impl card::StyleSheet for Theme2 {
    type Style = <iced::Theme as card::StyleSheet>::Style;

    fn active(&self, style: Self::Style) -> card::Appearance {
        match self {
            Theme2::Light => card::StyleSheet::active(&iced::Theme::Light, style),
        }
    }
}
