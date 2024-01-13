//! Einstellen der Steuerung einer [`Weiche`](crate::steuerung::weiche::Weiche).

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
use iced_core::{
    event,
    widget::text::{self, Text},
    Element, Font, Length, Renderer,
};
use iced_widget::{
    button::{self, Button},
    container, radio, scrollable,
    text_input::{self, TextInput},
    Column, Row,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{
        anschluss,
        map_mit_zustand::MapMitZustand,
        style::{sammlung::Sammlung, tab_bar::TabBar},
    },
    argumente::I2cSettings,
    steuerung::weiche::{Name, WeicheSerialisiert},
    util::nachschlagen::Nachschlagen,
};

/// Zustand eines Widgets zur [Auswahl] der Anschlüsse einer [`Weiche`](crate::steuerung::weiche::Weiche).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Zustand<AnschlüsseSerialisiert> {
    name: String,
    anschlüsse: AnschlüsseSerialisiert,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert: Default + Clone> Zustand<AnschlüsseSerialisiert> {
    /// Erstelle einen neuen [`Zustand`], potentiell mit voreingestellten Anschlüssen.
    fn neu<Richtung>(
        option_weiche: &Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
        hat_steuerung: bool,
    ) -> Self {
        let (name, anschlüsse) =
            if let Some(WeicheSerialisiert { name, anschlüsse, .. }) = option_weiche {
                (name.0.clone(), anschlüsse.clone())
            } else {
                (String::new(), AnschlüsseSerialisiert::default())
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

/// Nachricht einer [`Auswahl`].
#[derive(Debug, Clone)]
pub enum Nachricht<Richtung, AnschlüsseSerialisiert> {
    /// Steuerung einer Weiche anpassen.
    Festlegen(Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>),
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
}

/// Widget zur Auswahl der Anschlüsse einer [`Weiche`](crate::steuerung::weiche::Weiche).
#[derive(Debug)]
pub struct Auswahl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>(
    MapMitZustand<
        't,
        Zustand<AnschlüsseSerialisiert>,
        InterneNachricht<Richtung>,
        Nachricht<RichtungInformation, AnschlüsseSerialisiert>,
        R,
    >,
);

impl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>
    Auswahl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>
where
    AnschlüsseSerialisiert: 't + Clone + Default + Nachschlagen<Richtung, OutputSerialisiert>,
    Richtung: 'static + Clone + Display,
    RichtungInformation: 't + Clone + Default,
    R: 't + Renderer + iced_core::text::Renderer<Font = Font>,
    <R as Renderer>::Theme: button::StyleSheet
        + card::StyleSheet
        + container::StyleSheet
        + number_input::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + tab_bar::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle eine neue [`Auswahl`].
    pub fn neu(
        weichen_art: &'t str,
        weiche: Option<WeicheSerialisiert<RichtungInformation, AnschlüsseSerialisiert>>,
        hat_steuerung: bool,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_zustand = move || Zustand::neu(&weiche.clone(), hat_steuerung);
        let erzeuge_element = move |zustand: &_| {
            Self::erzeuge_element(weichen_art, zustand, scrollable_style, settings)
        };
        let mapper = |interne_nachricht: InterneNachricht<Richtung>,
                      zustand: &mut dyn DerefMut<Target = Zustand<AnschlüsseSerialisiert>>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Name(name) => {
                    zustand.name = name;
                    Vec::new()
                },
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    *zustand.anschlüsse.erhalte_mut(&richtung) = anschluss;
                    Vec::new()
                },
                InterneNachricht::Festlegen => {
                    vec![Nachricht::Festlegen(Some(WeicheSerialisiert::neu(
                        Name(zustand.name.clone()),
                        RichtungInformation::default(),
                        zustand.anschlüsse.clone(),
                    )))]
                },
                InterneNachricht::Entfernen => {
                    vec![Nachricht::Festlegen(None)]
                },
                InterneNachricht::Schließen => vec![Nachricht::Schließen],
            }
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    fn erzeuge_element(
        weichen_art: &'t str,
        zustand: &Zustand<AnschlüsseSerialisiert>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'t, InterneNachricht<Richtung>, R> {
        let Zustand { name, anschlüsse, hat_steuerung } = zustand;
        let mut column: Column<'t, InterneNachricht<Richtung>, R> = Column::new();
        let text_input: TextInput<'t, InterneNachricht<Richtung>, R> =
            TextInput::new("<Name>", name)
                .on_input(InterneNachricht::Name)
                .width(Length::Fixed(200.));
        column = column.push(text_input);
        for (richtung, anschluss) in anschlüsse.referenzen() {
            column = column.push(Row::new().push(Text::new(richtung.to_string())).push({
                let anschluss_auswahl = anschluss::Auswahl::neu_output_s(
                    Some(anschluss.clone()),
                    scrollable_style,
                    settings,
                );
                Element::from(anschluss_auswahl).map(move |anschluss_serialisiert| {
                    InterneNachricht::Anschluss(richtung.clone(), anschluss_serialisiert)
                })
            }))
        }
        column = column.push(
            Row::new()
                .push(Button::new(Text::new("Festlegen")).on_press(InterneNachricht::Festlegen))
                .push(if *hat_steuerung {
                    Button::new(Text::new("Entfernen")).on_press(InterneNachricht::Entfernen)
                } else {
                    Button::new(Text::new("Keine Anschlüsse")).on_press(InterneNachricht::Schließen)
                }),
        );
        let card: Card<'t, InterneNachricht<Richtung>, R> =
            Card::new(Text::new(weichen_art), column)
                .on_close(InterneNachricht::Schließen)
                .width(Length::Shrink)
                .height(Length::Shrink);
        card.into()
    }
}

impl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>
    From<Auswahl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<RichtungInformation, AnschlüsseSerialisiert>, R>
where
    AnschlüsseSerialisiert: 'static + PartialEq,
    Richtung: 't,
    RichtungInformation: 't,
    R: 't + Renderer,
{
    fn from(
        anzeige: Auswahl<'t, Richtung, RichtungInformation, AnschlüsseSerialisiert, R>,
    ) -> Self {
        Element::from(anzeige.0)
    }
}
