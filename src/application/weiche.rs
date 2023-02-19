//! Einstellen der Steuerung einer [Weiche](crate::steuerung::Weiche).

use std::{
    fmt::{Debug, Display},
    ops::DerefMut,
};

use iced_aw::{
    native::{
        card::{self, Card},
        number_input,
    },
    tab_bar,
};
use iced_native::{
    event,
    widget::{
        button::{self, Button},
        container, radio,
        text::{self, Text},
        text_input::{self, TextInput},
        Column, Row,
    },
    Element, Font, Length, Renderer,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{anschluss, map_mit_zustand::MapMitZustand, style::tab_bar::TabBar},
    nachschlagen::Nachschlagen,
    steuerung::weiche::{Name, WeicheSerialisiert},
};

/// Zustand eines Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
#[derive(Debug, Clone)]
struct Zustand<AnschlüsseSerialisiert> {
    name: String,
    anschlüsse: AnschlüsseSerialisiert,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert: Default + Clone> Zustand<AnschlüsseSerialisiert> {
    /// Erstelle einen neuen [Zustand], potentiell mit voreingestellten Anschlüssen.
    fn neu<Richtung>(
        option_weiche: &Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        let (name, anschlüsse, hat_steuerung) =
            if let Some(WeicheSerialisiert { name, anschlüsse, .. }) = option_weiche {
                (name.0.clone(), anschlüsse.clone(), true)
            } else {
                (String::new(), AnschlüsseSerialisiert::default(), false)
            };
        Zustand { name, anschlüsse, hat_steuerung }
    }
}

#[derive(Debug, Clone)]
enum InterneNachricht<Richtung> {
    Name(String),
    Anschluss(Richtung, OutputSerialisiert),
    Festlegen,
    Entfernen,
    Schließen,
}

/// Nachricht einer [Auswahl].
#[derive(Debug, Clone)]
pub enum Nachricht<Richtung, AnschlüsseSerialisiert> {
    /// Steuerung einer Weiche anpassen.
    Festlegen(Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>),
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
}

/// Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
#[derive(Debug)]
pub struct Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>(
    MapMitZustand<
        't,
        Zustand<AnschlüsseSerialisiert>,
        InterneNachricht<Richtung>,
        Nachricht<Richtung, AnschlüsseSerialisiert>,
        R,
    >,
);

impl<'t, Richtung, AnschlüsseSerialisiert, R, Style>
    Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    AnschlüsseSerialisiert: Default + Clone + Nachschlagen<Richtung, OutputSerialisiert>,
    Richtung: 'static + Clone + Default + Display,
    R: 't + Renderer + iced_native::text::Renderer<Font = Font>,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet
        + radio::StyleSheet
        + card::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet<Style = TabBar<Style>>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<AnschlüsseAuswahlZustand>(
        weiche: &'t Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        let erzeuge_zustand = || Zustand::neu(weiche);
        let erzeuge_element = Self::erzeuge_element;
        let mapper = |interne_nachricht,
                      zustand: &mut dyn DerefMut<Target = Zustand<AnschlüsseSerialisiert>>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Name(name) => {
                    zustand.name = name;
                    None
                },
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    *zustand.anschlüsse.erhalte_mut(&richtung) = anschluss;
                    None
                },
                InterneNachricht::Festlegen => {
                    Some(Nachricht::Festlegen(Some(WeicheSerialisiert::neu(
                        Name(zustand.name.clone()),
                        Richtung::default(),
                        zustand.anschlüsse.clone(),
                    ))))
                },
                InterneNachricht::Entfernen => Some(Nachricht::Festlegen(None)),
                InterneNachricht::Schließen => Some(Nachricht::Schließen),
            }
        };
        Auswahl(MapMitZustand::neu(&erzeuge_zustand, &erzeuge_element, &mapper))
    }

    fn erzeuge_element(
        zustand: &'t Zustand<AnschlüsseSerialisiert>,
    ) -> Element<'t, InterneNachricht<Richtung>, R> {
        let Zustand { name, anschlüsse, hat_steuerung } = zustand;
        let mut column = Column::new()
            .push(TextInput::new("<Name>", name, InterneNachricht::Name).width(Length::Units(200)));
        for (richtung, anschluss) in anschlüsse.referenzen() {
            column = column.push(Row::new().push(Text::new(richtung.to_string())).push(
                Element::from(anschluss::Auswahl::neu_output_s(Some(anschluss))).map(
                    move |anschluss_serialisiert| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_serialisiert)
                    },
                ),
            ))
        }
        column = column.push(
            Row::new()
                .push(Button::new(Text::new("Festlegen")).on_press(InterneNachricht::Festlegen))
                .push(
                    Button::new(Text::new(if *hat_steuerung {
                        "Entfernen"
                    } else {
                        "Keine Anschlüsse"
                    }))
                    .on_press(InterneNachricht::Entfernen),
                ),
        );
        let card: Card<'t, InterneNachricht<Richtung>, R> = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        card.into()
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> From<Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<Richtung, AnschlüsseSerialisiert>, R>
where
    R: Renderer,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>) -> Self {
        Element::new(anzeige.0)
    }
}
