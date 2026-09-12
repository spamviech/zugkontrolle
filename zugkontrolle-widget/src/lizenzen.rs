//! Zeige alle Lizenzen verwendeter Open-Source Bibliotheken.

use std::borrow::Cow;

use iced::Pixels;
use iced_core::{
    Element, Length, event, text as text_core,
    widget::text::{self, Text},
};
use iced_widget::{
    Column, Row, Space,
    button::{self, Button},
    container::{self, Container},
    rule::{self, Rule},
    scrollable::{self, Scrollable},
};

use zugkontrolle_lizenzen::{LizenzenMap, TARGET_LIZENZEN};
use zugkontrolle_util::unicase_ord::UniCaseOrd;

use crate::{
    map_mit_zustand::MapMitZustand,
    style::{
        self, Sammlung,
        linie::{Linie, TRENNLINIE},
    },
};

// #[cfg(test)]
// mod test;
// pub mod texte;

/// Interne Nachricht zur Interaktion mit einem [`Lizenzen`]-Widget.
#[derive(Debug, Clone)]
enum InterneNachricht {
    /// Zeige den übergebenen Lizenz-Text an.
    Aktuell(UniCaseOrd<String>, &'static str),
    /// Schließe das Dialog-Fenster.
    Schließen,
}

/// Nachricht, die von einem [`Lizenzen`]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [`Lizenzen`]-Anzeige.
    Schließen,
}

/// Zustand eines [`Lizenzen`]-Widgets.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Zustand {
    /// Die aktuell gezeigte Lizenz.
    aktuell: Option<(UniCaseOrd<String>, Cow<'static, str>)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [`Lizenzen`]-Widgets.
    fn neu(lizenzen: &LizenzenMap) -> Self {
        let aktuell = lizenzen
            .iter()
            .next()
            .map(|(name, lizenztext)| (name.clone(), Cow::Borrowed(*lizenztext)));
        Zustand { aktuell }
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
#[derive(Debug)]
pub struct Lizenzen<'a, Thema, R>(
    MapMitZustand<'a, Zustand, InterneNachricht, Nachricht, Thema, R>,
);

/// Der [`Abstand`](Space) zwischen Widgets in Pixel.
const PADDING: f32 = 5.;
/// Die Breite der [`Trennlinie`](Rule) zwischen der Auswahl-Liste und dem aktuell gezeigten Lizenztext.
const TRENNLINIE_BREITE: u32 = 1;

impl<'a, Thema, R> Lizenzen<'a, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a
        + container::Catalog
        + button::Catalog
        + scrollable::Catalog
        + rule::Catalog
        + text::Catalog,
{
    /// Erstelle ein neues [`Lizenzen`]-Widget mit den verwendeten Lizenzen.
    pub fn neu_mit_verwendeten_lizenzen(scrollable_style: Sammlung) -> Self {
        Self::neu(&TARGET_LIZENZEN, scrollable_style)
    }

    /// Erstelle ein neues [`Lizenzen`]-Widget.
    pub fn neu(lizenzen: &'a LizenzenMap, scrollable_style: Sammlung) -> Self {
        let erzeuge_element = move |zustand: &Zustand| -> Element<'a, InterneNachricht, Thema, R> {
            Self::erzeuge_element(zustand, lizenzen, scrollable_style.clone())
        };
        let mapper = |interne_nachricht, zustand: &mut Zustand, status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Aktuell(name, lizenz_text) => {
                    zustand.aktuell = Some((name, Cow::Borrowed(lizenz_text)));
                    Vec::new()
                },
                InterneNachricht::Schließen => vec![Nachricht::Schließen],
            }
        };
        Lizenzen(MapMitZustand::neu(Zustand::neu(lizenzen), erzeuge_element, mapper))
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Lizenzen`]-Widget.
    fn erzeuge_element(
        zustand: &Zustand,
        lizenzen: &'a LizenzenMap,
        scrollable_style: Sammlung,
    ) -> Element<'a, InterneNachricht, Thema, R> {
        let Zustand { aktuell } = zustand;
        let mut buttons = Column::new().width(Length::Shrink).height(Length::Shrink);
        let (aktuell_name, aktuell_text) = if let Some((name, text)) = aktuell {
            (Some(name.clone()), Some(text.clone().into_owned()))
        } else {
            (None, None)
        };
        for (name, lizenz_text) in lizenzen {
            buttons = buttons.push({
                let button = Button::new(Text::new(name.as_ref()));
                if Some(name) == aktuell_name.as_ref() {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name.clone(), lizenz_text))
                }
            });
        }
        let buttons = Scrollable::new(buttons).height(Length::Fill).style(scrollable_style.style());
        let column = Column::new()
            .push(buttons)
            .push(Space::new().height(Length::Fixed(PADDING)))
            .push(Button::new(Text::new("Schließen")).on_press(InterneNachricht::Schließen))
            .width(Length::Shrink)
            .height(Length::Fill);
        let mut column_aktuell = Column::new().width(Length::Fill).height(Length::Shrink);
        if let Some(aktuell_text) = aktuell_text {
            let text_mit_horizontalem_padding = Row::new()
                .push(Space::new().width(Length::Fixed(PADDING)))
                .push(Text::new(aktuell_text).width(Length::Fill).height(Length::Shrink))
                .push(Space::new().width(Length::Fixed(PADDING)))
                .width(Length::Fill)
                .height(Length::Shrink);
            column_aktuell = column_aktuell
                .push(Space::new().height(Length::Fixed(PADDING)))
                .push(text_mit_horizontalem_padding)
                .push(Space::new().height(Length::Fixed(PADDING)));
        }
        let container = Container::new(
            Row::new()
                .push(column)
                .push(rule::vertical(TRENNLINIE_BREITE).style(TRENNLINIE.style()))
                .push(Scrollable::new(column_aktuell)),
        )
        .style(style::container::WEIẞ.style());
        Element::<'a, InterneNachricht, Thema, R>::from(container)
    }
}

impl<'a, Thema, R> From<Lizenzen<'a, Thema, R>> for Element<'a, Nachricht, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema: 'a
        + container::Catalog
        + button::Catalog
        + scrollable::Catalog
        + rule::Catalog
        + text::Catalog,
{
    fn from(lizenzen: Lizenzen<'a, Thema, R>) -> Self {
        Element::from(lizenzen.0)
    }
}
