//! Einstellen der Steuerung eines [Kontaktes](crate::steuerung::kontakt::Kontakt).

use std::{collections::HashMap, fmt::Debug, ops::DerefMut};

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
    anschluss::{
        pcf8574::{Beschreibung, Lager},
        trigger::Trigger,
        InputSerialisiert,
    },
    application::{
        anschluss::{self, make_radios},
        map_mit_zustand::MapMitZustand,
        style::{sammlung::Sammlung, tab_bar::TabBar},
    },
    argumente::I2cSettings,
    steuerung::kontakt::{KontaktSerialisiert, Name},
};

/// Zustand eines Widgets zur [Auswahl] der Anschlüsse eines [Kontaktes](crate::steuerung::kontakt::Kontakt).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Zustand {
    name: String,
    anschluss: InputSerialisiert,
    trigger: Trigger,
    hat_steuerung: bool,
}

impl Zustand {
    /// Erstelle einen neuen [Zustand], potentiell mit voreingestellten Anschlüssen.
    fn neu(option_kontakt: &Option<KontaktSerialisiert>) -> Self {
        let (name, anschluss, trigger, hat_steuerung) =
            if let Some(KontaktSerialisiert { name, anschluss, trigger }) = option_kontakt {
                (name.0.clone(), anschluss.clone(), *trigger, true)
            } else {
                (String::new(), InputSerialisiert::Pin { pin: 0 }, Trigger::RisingEdge, false)
            };
        Zustand { name, anschluss, trigger, hat_steuerung }
    }
}

#[derive(Debug, Clone)]
#[allow(variant_size_differences)]
enum InterneNachricht {
    Name(String),
    Anschluss(InputSerialisiert),
    Trigger(Trigger),
    Festlegen,
    Entfernen,
    Schließen,
}

/// Nachricht einer [Auswahl].
#[derive(Debug, Clone)]
pub enum Nachricht {
    /// Steuerung einer Weiche anpassen.
    Festlegen(Option<KontaktSerialisiert>),
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
}

/// Widget zur Auswahl der Anschlüsse eines [Kontaktes](crate::steuerung::kontakt::Kontakt).
#[derive(Debug)]
pub struct Auswahl<'t, R>(MapMitZustand<'t, Zustand, InterneNachricht, Nachricht, R>);

impl<'t, R> Auswahl<'t, R>
where
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
    /// Erstelle eine neue [Auswahl].
    pub fn neu(
        gleis_art: &'t str,
        kontakt: Option<KontaktSerialisiert>,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_zustand = move || Zustand::neu(&kontakt.clone());
        let erzeuge_element = move |zustand: &_| {
            Self::erzeuge_element(gleis_art, zustand, lager, scrollable_style, settings)
        };
        let mapper = |interne_nachricht: InterneNachricht,
                      zustand: &mut dyn DerefMut<Target = Zustand>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::Name(name) => {
                    zustand.name = name;
                    Vec::new()
                },
                InterneNachricht::Anschluss(anschluss) => {
                    zustand.anschluss = anschluss;
                    Vec::new()
                },
                InterneNachricht::Trigger(trigger) => {
                    zustand.trigger = trigger;
                    Vec::new()
                },
                InterneNachricht::Festlegen => {
                    vec![
                        Nachricht::Festlegen(Some(KontaktSerialisiert::neu(
                            Name(zustand.name.clone()),
                            zustand.anschluss.clone(),
                            zustand.trigger,
                        ))),
                        Nachricht::Schließen,
                    ]
                },
                InterneNachricht::Entfernen => {
                    vec![Nachricht::Festlegen(None), Nachricht::Schließen]
                },
                InterneNachricht::Schließen => vec![Nachricht::Schließen],
            }
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    fn erzeuge_element(
        weichen_art: &'t str,
        zustand: &Zustand,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'t, InterneNachricht, R> {
        let Zustand { name, anschluss, trigger, hat_steuerung } = zustand;
        let mut column: Column<'t, InterneNachricht, R> = Column::new();
        let text_input: TextInput<'t, InterneNachricht, R> = TextInput::new("<Name>", name)
            .on_input(InterneNachricht::Name)
            .width(Length::Fixed(200.));
        column = column.push(text_input);
        let anschluss_auswahl = Element::from(anschluss::Auswahl::neu_input_s(
            Some(anschluss.clone()),
            lager,
            scrollable_style,
            settings,
        ))
        .map(InterneNachricht::Anschluss);
        let trigger_auswahl = make_radios(
            trigger,
            [
                ("Deaktiviert", Trigger::Disabled),
                ("RisingEdge", Trigger::RisingEdge),
                ("FallingEdge", Trigger::FallingEdge),
                ("Jede Änderung", Trigger::Both),
            ],
            InterneNachricht::Trigger,
        );
        column = column.push(Row::new().push(anschluss_auswahl).push(trigger_auswahl));
        column = column.push(
            Row::new()
                .push(Button::new(Text::new("Festlegen")).on_press(InterneNachricht::Festlegen))
                .push(if *hat_steuerung {
                    Button::new(Text::new("Entfernen")).on_press(InterneNachricht::Entfernen)
                } else {
                    Button::new(Text::new("Keine Anschlüsse")).on_press(InterneNachricht::Schließen)
                }),
        );
        let card: Card<'t, InterneNachricht, R> = Card::new(Text::new(weichen_art), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        card.into()
    }
}

impl<'t, R: 't + Renderer> From<Auswahl<'t, R>> for Element<'t, Nachricht, R> {
    fn from(anzeige: Auswahl<'t, R>) -> Self {
        Element::from(anzeige.0)
    }
}
