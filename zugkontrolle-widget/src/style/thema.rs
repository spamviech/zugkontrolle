//! Unterstützte Graphik-Themen.

use std::fmt::{self, Debug, Display};

use enum_iterator::Sequence;
use iced_aw::{card, number_input};
use iced_core::theme::{Base, Mode, Palette, Style, Theme};
use iced_widget::{
    canvas::Text, checkbox, container, overlay::menu, pick_list, radio, scrollable, slider, text,
    text_input, vertical_slider,
};
use int_enum::IntEnum;

use zugkontrolle_argumente::ThemaArgument;
use zugkontrolle_gleise::knopf;
use zugkontrolle_typen::farbe::Farbe;

use crate::fonts::standard_text;

/// Unterstützte Graphik-Themen, sehr nah am built-in [`iced::Theme`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, IntEnum, Sequence)]
#[repr(u8)]
pub enum Thema {
    /// Die helle Variante.
    #[default]
    Hell = 0,
    /// Die dunkle Variante.
    Dunkel = 1,
}

impl Display for Thema {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Thema as Debug>::fmt(self, formatter)
    }
}

impl From<ThemaArgument> for Thema {
    fn from(value: ThemaArgument) -> Self {
        match value {
            ThemaArgument::Hell => Thema::Hell,
            ThemaArgument::Dunkel => Thema::Dunkel,
        }
    }
}

impl knopf::Thema for Thema {
    fn standard_text(&self) -> Text {
        standard_text()
    }

    fn strich(&self) -> Farbe {
        match self {
            Thema::Hell => Farbe { rot: 0., grün: 0., blau: 0. },
            Thema::Dunkel => Farbe { rot: 1., grün: 1., blau: 1. },
        }
    }

    fn hintergrund(&self, aktiv: bool, in_bounds: bool) -> Farbe {
        let grey_value = match self {
            Thema::Hell | Thema::Dunkel if aktiv => 0.5,
            Thema::Hell if in_bounds => 0.7,
            Thema::Hell => 0.9,
            Thema::Dunkel if in_bounds => 0.3,
            Thema::Dunkel => 0.2,
        };
        Farbe { rot: grey_value, grün: grey_value, blau: grey_value }
    }
}

impl From<Thema> for Theme {
    fn from(value: Thema) -> Self {
        match value {
            Thema::Hell => Theme::Light,
            Thema::Dunkel => Theme::Dark,
        }
    }
}

impl Base for Thema {
    fn default(preference: Mode) -> Self {
        match preference {
            Mode::None | Mode::Light => Thema::Hell,
            Mode::Dark => Thema::Dunkel,
        }
    }

    fn mode(&self) -> Mode {
        match self {
            Thema::Hell => Mode::Light,
            Thema::Dunkel => Mode::Dark,
        }
    }

    fn base(&self) -> Style {
        Theme::from(*self).base()
    }

    fn palette(&self) -> Option<Palette> {
        Some(Theme::from(*self).palette())
    }

    fn name(&self) -> &str {
        match self {
            Thema::Hell => "Hell",
            Thema::Dunkel => "Dunkel",
        }
    }
}

impl container::Catalog for Thema {
    type Class<'a> = container::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>) -> container::Style {
        todo!()
    }
}

impl checkbox::Catalog for Thema {
    type Class<'a> = fn(&Thema, checkbox::Status) -> checkbox::Style;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: checkbox::Status) -> checkbox::Style {
        todo!()
    }
    // type Style = <Theme as checkbox::Catalog>::Style;

    // fn active(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
    //     match self {
    //         Thema::Hell => checkbox::Catalog::active(&Theme::Light, style, is_checked),
    //         Thema::Dunkel => checkbox::Catalog::active(&Theme::Dark, style, is_checked),
    //     }
    // }

    // fn hovered(&self, style: &Self::Style, is_checked: bool) -> checkbox::Appearance {
    //     match self {
    //         Thema::Hell => checkbox::Catalog::hovered(&Theme::Light, style, is_checked),
    //         Thema::Dunkel => checkbox::Catalog::hovered(&Theme::Dark, style, is_checked),
    //     }
    // }
}

impl text::Catalog for Thema {
    type Class<'a> = text::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, item: &Self::Class<'_>) -> text::Style {
        todo!()
    }
    // type Style = <Theme as text::Catalog>::Style;

    // fn appearance(&self, style: Self::Style) -> text::Appearance {
    //     match self {
    //         Thema::Hell => text::Catalog::appearance(&Theme::Light, style),
    //         Thema::Dunkel => text::Catalog::appearance(&Theme::Dark, style),
    //     }
    // }
}

impl text_input::Catalog for Thema {
    type Class<'a> = fn(&Thema, text_input::Status) -> text_input::Style;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: text_input::Status) -> text_input::Style {
        todo!()
    }
    // type Style = <Theme as text_input::Catalog>::Style;

    // fn active(&self, style: &Self::Style) -> text_input::Appearance {
    //     match self {
    //         Thema::Hell => text_input::Catalog::active(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::active(&Theme::Dark, style),
    //     }
    // }

    // fn focused(&self, style: &Self::Style) -> text_input::Appearance {
    //     match self {
    //         Thema::Hell => text_input::Catalog::focused(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::focused(&Theme::Dark, style),
    //     }
    // }

    // fn placeholder_color(&self, style: &Self::Style) -> iced_core::Color {
    //     match self {
    //         Thema::Hell => text_input::Catalog::placeholder_color(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::placeholder_color(&Theme::Dark, style),
    //     }
    // }

    // fn value_color(&self, style: &Self::Style) -> iced_core::Color {
    //     match self {
    //         Thema::Hell => text_input::Catalog::value_color(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::value_color(&Theme::Dark, style),
    //     }
    // }

    // fn disabled_color(&self, style: &Self::Style) -> iced_core::Color {
    //     match self {
    //         Thema::Hell => text_input::Catalog::disabled_color(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::disabled_color(&Theme::Dark, style),
    //     }
    // }

    // fn selection_color(&self, style: &Self::Style) -> iced_core::Color {
    //     match self {
    //         Thema::Hell => text_input::Catalog::selection_color(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::selection_color(&Theme::Dark, style),
    //     }
    // }

    // fn disabled(&self, style: &Self::Style) -> text_input::Appearance {
    //     match self {
    //         Thema::Hell => text_input::Catalog::disabled(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::disabled(&Theme::Dark, style),
    //     }
    // }

    // fn hovered(&self, style: &Self::Style) -> text_input::Appearance {
    //     match self {
    //         Thema::Hell => text_input::Catalog::hovered(&Theme::Light, style),
    //         Thema::Dunkel => text_input::Catalog::hovered(&Theme::Dark, style),
    //     }
    // }
}

impl card::Catalog for Thema {
    type Class<'a> = container::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: card::Status) -> card::Style {
        todo!()
    }
    // type Style = <Theme as card::Catalog>::Style;

    // fn active(&self, style: &Self::Style) -> card::Appearance {
    //     match self {
    //         Thema::Hell => card::Catalog::active(&Theme::Light, style),
    //         Thema::Dunkel => card::Catalog::active(&Theme::Dark, style),
    //     }
    // }
}

impl slider::Catalog for Thema {
    type Class<'a> = fn(&Thema, slider::Status) -> slider::Style;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: slider::Status) -> slider::Style {
        todo!()
    }
    // type Style = <Theme as slider::Catalog>::Style;

    // fn active(&self, style: &Self::Style) -> vertical_slider::Appearance {
    //     match self {
    //         Thema::Hell => slider::Catalog::active(&Theme::Light, style),
    //         Thema::Dunkel => slider::Catalog::active(&Theme::Dark, style),
    //     }
    // }

    // fn hovered(&self, style: &Self::Style) -> vertical_slider::Appearance {
    //     match self {
    //         Thema::Hell => slider::Catalog::hovered(&Theme::Light, style),
    //         Thema::Dunkel => slider::Catalog::hovered(&Theme::Dark, style),
    //     }
    // }

    // fn dragging(&self, style: &Self::Style) -> vertical_slider::Appearance {
    //     match self {
    //         Thema::Hell => slider::Catalog::dragging(&Theme::Light, style),
    //         Thema::Dunkel => slider::Catalog::dragging(&Theme::Dark, style),
    //     }
    // }
}

impl radio::Catalog for Thema {
    type Class<'a> = container::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: radio::Status) -> radio::Style {
        todo!()
    }
    // type Style = <Theme as radio::Catalog>::Style;

    // fn active(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
    //     match self {
    //         Thema::Hell => radio::Catalog::active(&Theme::Light, style, is_selected),
    //         Thema::Dunkel => radio::Catalog::active(&Theme::Dark, style, is_selected),
    //     }
    // }

    // fn hovered(&self, style: &Self::Style, is_selected: bool) -> radio::Appearance {
    //     match self {
    //         Thema::Hell => radio::Catalog::hovered(&Theme::Light, style, is_selected),
    //         Thema::Dunkel => radio::Catalog::hovered(&Theme::Dark, style, is_selected),
    //     }
    // }
}

impl number_input::Catalog for Thema {
    type Class<'a> = container::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: card::Status) -> number_input::Style {
        todo!()
    }
    // type Style = <Theme as number_input::Catalog>::Style;

    // fn active(&self, style: &Self::Style) -> number_input::Appearance {
    //     match self {
    //         Thema::Hell => number_input::Catalog::active(&Theme::Light, style),
    //         Thema::Dunkel => number_input::Catalog::active(&Theme::Dark, style),
    //     }
    // }

    // fn pressed(&self, style: &Self::Style) -> number_input::Appearance {
    //     match self {
    //         Thema::Hell => number_input::Catalog::pressed(&Theme::Light, style),
    //         Thema::Dunkel => number_input::Catalog::pressed(&Theme::Dark, style),
    //     }
    // }

    // fn disabled(&self, style: &Self::Style) -> number_input::Appearance {
    //     match self {
    //         Thema::Hell => number_input::Catalog::disabled(&Theme::Light, style),
    //         Thema::Dunkel => number_input::Catalog::disabled(&Theme::Dark, style),
    //     }
    // }
}

impl iced_aw::style::number_input::ExtendedCatalog for Thema {
    fn style(
        &self,
        class: &<Self as number_input::number_input::Catalog>::Class<'_>,
        status: card::Status,
    ) -> number_input::Style {
        todo!()
    }

    fn default_input<'a>() -> <Self as text_input::Catalog>::Class<'a> {
        <Self as text_input::Catalog>::default()
    }
}

impl scrollable::Catalog for Thema {
    type Class<'a> = scrollable::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        todo!()
    }

    fn style(&self, class: &Self::Class<'_>, status: scrollable::Status) -> scrollable::Style {
        todo!()
    }
}

impl pick_list::Catalog for Thema {
    type Class<'a> = pick_list::StyleFn<'a, Thema>;

    fn default<'a>() -> <Self as pick_list::Catalog>::Class<'a> {
        todo!()
    }

    fn style(
        &self,
        class: &<Self as pick_list::Catalog>::Class<'_>,
        status: pick_list::Status,
    ) -> pick_list::Style {
        todo!()
    }

    fn default_menu<'a>() -> <Self as menu::Catalog>::Class<'a> {
        <Self as menu::Catalog>::default()
    }
    // type Style = <Theme as pick_list::Catalog>::Style;

    // fn active(&self, style: &Self::Style) -> pick_list::Appearance {
    //     match self {
    //         Thema::Hell => pick_list::Catalog::active(&Theme::Light, style),
    //         Thema::Dunkel => pick_list::Catalog::active(&Theme::Dark, style),
    //     }
    // }

    // fn hovered(&self, style: &Self::Style) -> pick_list::Appearance {
    //     match self {
    //         Thema::Hell => pick_list::Catalog::hovered(&Theme::Light, style),
    //         Thema::Dunkel => pick_list::Catalog::hovered(&Theme::Dark, style),
    //     }
    // }
}

impl menu::Catalog for Thema {
    type Class<'a> = fn(&Thema) -> menu::StyleFn<'a, Thema>;

    fn default<'a>() -> <Self as menu::Catalog>::Class<'a> {
        todo!()
    }

    fn style(&self, class: &<Self as menu::Catalog>::Class<'_>) -> menu::Style {
        todo!()
    }

    fn default_scrollable<'a>() -> <Self as scrollable::Catalog>::Class<'a> {
        <Self as scrollable::Catalog>::default()
    }
    // type Style = <Theme as menu::Catalog>::Style;

    // fn appearance(&self, style: &Self::Style) -> menu::Appearance {
    //     match self {
    //         Thema::Hell => menu::Catalog::appearance(&Theme::Light, style),
    //         Thema::Dunkel => menu::Catalog::appearance(&Theme::Dark, style),
    //     }
    // }
}
