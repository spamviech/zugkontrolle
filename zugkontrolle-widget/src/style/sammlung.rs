//! Style-Strukturen für ein [`iced::widget::Scrollable`].

use iced_core::{
    Background, Color,
    border::{self, Border},
};
use iced_widget::scrollable::{self, Rail, Scroller};

use crate::style::thema::Thema;

#[doc(inline)]
pub use scrollable::{Style, StyleFn};

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

pub trait StyleProvider<'a, Thema>
where
    Thema: scrollable::Catalog<Class<'a> = StyleFn<'a, Thema>>,
{
    #[must_use]
    fn style_fn(self) -> StyleFn<'static, Thema>;
}

impl StyleProvider<'_, Thema> for Sammlung {
    fn style_fn(self) -> StyleFn<'static, Thema> {
        Box::new(move |thema, status| {
            let mut style = <Thema as scrollable::Catalog>::default()(thema, status);
            let (grey_border, grey_background) = match thema {
                Thema::Hell => (0.7, 0.6),
                Thema::Dunkel => (0.3, 0.2),
            };
            style.vertical_rail = Rail {
                background: None,
                border: Border { radius: border::Radius::from(0.), width: 0., color: Color::BLACK },
                scroller: Scroller {
                    border: Border {
                        radius: border::Radius::from(0.25 * self.breite),
                        width: 0.,
                        color: Color::from_rgb(grey_border, grey_border, grey_border),
                    },
                    background: Background::Color(Color::from_rgb(
                        grey_background,
                        grey_background,
                        grey_background,
                    )),
                },
            };
            style
        })
    }
}
