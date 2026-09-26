//! Unterstützte Graphik-Themen.

use std::fmt::{self, Debug, Display};

use enum_iterator::Sequence;
use iced_aw::{
    card, number_input,
    style::{self, tab_bar},
};
use iced_core::theme::{Base, Mode, Palette, Style, Theme};
use iced_widget::{
    button, canvas::Text, checkbox, container, overlay::menu, pick_list, radio, rule, scrollable,
    slider, text, text_input,
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

impl knopf::Catalog for Thema {
    fn standard_text(&self) -> Text {
        standard_text()
    }

    fn strich(&self) -> Farbe {
        self.base().text_color.into()
    }

    fn hintergrund(&self, aktiv: bool, in_bounds: bool) -> Farbe {
        let grey_value = match self {
            Thema::Hell | Thema::Dunkel if aktiv => 0.5,
            Thema::Hell if in_bounds => 0.7,
            Thema::Hell => 0.8,
            Thema::Dunkel if in_bounds => 0.4,
            Thema::Dunkel => 0.3,
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
        Box::new(|thema| <Theme as container::Catalog>::default()(&Theme::from(*thema)))
    }

    fn style(&self, class: &Self::Class<'_>) -> container::Style {
        class(self)
    }
}

impl button::Catalog for Thema {
    type Class<'a> = button::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as button::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: button::Status) -> button::Style {
        class(self, status)
    }
}

impl rule::Catalog for Thema {
    type Class<'a> = rule::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema| <Theme as rule::Catalog>::default()(&Theme::from(*thema)))
    }

    fn style(&self, class: &Self::Class<'_>) -> rule::Style {
        class(self)
    }
}

impl checkbox::Catalog for Thema {
    type Class<'a> = checkbox::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as checkbox::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: checkbox::Status) -> checkbox::Style {
        class(self, status)
    }
}

impl text::Catalog for Thema {
    type Class<'a> = text::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema| <Theme as text::Catalog>::default()(&Theme::from(*thema)))
    }

    fn style(&self, class: &Self::Class<'_>) -> text::Style {
        class(self)
    }
}

impl text_input::Catalog for Thema {
    type Class<'a> = text_input::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as text_input::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: text_input::Status) -> text_input::Style {
        class(self, status)
    }
}

impl card::Catalog for Thema {
    type Class<'a> = card::StyleFn<'a, Thema, card::Style>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| <Theme as card::Catalog>::default()(&Theme::from(*thema), status))
    }

    fn style(&self, class: &Self::Class<'_>, status: card::Status) -> card::Style {
        class(self, status)
    }
}

impl slider::Catalog for Thema {
    type Class<'a> = slider::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as slider::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: slider::Status) -> slider::Style {
        class(self, status)
    }
}

impl radio::Catalog for Thema {
    type Class<'a> = radio::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| <Theme as radio::Catalog>::default()(&Theme::from(*thema), status))
    }

    fn style(&self, class: &Self::Class<'_>, status: radio::Status) -> radio::Style {
        class(self, status)
    }
}

impl number_input::Catalog for Thema {
    type Class<'a> = number_input::StyleFn<'a, Thema, number_input::Style>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as number_input::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: card::Status) -> number_input::Style {
        class(self, status)
    }
}

impl style::number_input::ExtendedCatalog for Thema {
    fn style(
        &self,
        class: &<Self as number_input::Catalog>::Class<'_>,
        status: card::Status,
    ) -> number_input::Style {
        class(self, status)
    }
}

impl scrollable::Catalog for Thema {
    type Class<'a> = scrollable::StyleFn<'a, Thema>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as scrollable::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: scrollable::Status) -> scrollable::Style {
        class(self, status)
    }
}

impl pick_list::Catalog for Thema {
    type Class<'a> = pick_list::StyleFn<'a, Thema>;

    fn default<'a>() -> <Self as pick_list::Catalog>::Class<'a> {
        Box::new(|thema, status| {
            <Theme as pick_list::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(
        &self,
        class: &<Self as pick_list::Catalog>::Class<'_>,
        status: pick_list::Status,
    ) -> pick_list::Style {
        class(self, status)
    }

    fn default_menu<'a>() -> <Self as menu::Catalog>::Class<'a> {
        <Self as menu::Catalog>::default()
    }
}

impl menu::Catalog for Thema {
    type Class<'a> = menu::StyleFn<'a, Thema>;

    fn default<'a>() -> <Self as menu::Catalog>::Class<'a> {
        Box::new(|thema| <Theme as menu::Catalog>::default()(&Theme::from(*thema)))
    }

    fn style(&self, class: &<Self as menu::Catalog>::Class<'_>) -> menu::Style {
        class(self)
    }
}

impl tab_bar::Catalog for Thema {
    type Class<'a> = style::status::StyleFn<'a, Thema, tab_bar::Style>;

    fn default<'a>() -> Self::Class<'a> {
        Box::new(|thema, status| {
            <Theme as tab_bar::Catalog>::default()(&Theme::from(*thema), status)
        })
    }

    fn style(&self, class: &Self::Class<'_>, status: card::Status) -> tab_bar::Style {
        class(self, status)
    }
}
