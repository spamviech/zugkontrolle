//! Einstellen der Steuerung eines [`Kontaktes`](crate::steuerung::kontakt::Kontakt).

use std::{fmt::Debug, ops::DerefMut};

use iced_aw::{
    native::{
        card::{self, Card},
        number_input,
    },
    tab_bar,
};
use iced_core::{
    event, text as text_core,
    widget::text::{self, Text},
    Element, Font, Length, Renderer,
};
use iced_widget::{
    button::{self, Button},
    container, radio, scrollable,
    text_input::{self, TextInput},
    Column, Row,
};

use zugkontrolle_anschluss::{pcf8574::Lager, trigger::Trigger, InputSerialisiert};
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_gleis::steuerung::kontakt::{KontaktSerialisiert, Name};

use crate::{
    anschluss::{self, make_radios},
    map_mit_zustand::MapMitZustand,
    style::{sammlung::Sammlung, tab_bar::TabBar},
};

/// Zustand eines Widgets zur [`Auswahl`] der Anschlüsse eines [`Kontaktes`](crate::steuerung::kontakt::Kontakt).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Zustand {
    /// Der aktuelle gewählte Name.
    name: String,
    /// Der aktuell gewählte Anschluss.
    anschluss: InputSerialisiert,
    /// Der aktuell gewählte [`Trigger`],
    /// wann [`warte_auf_trigger`](crate::steuerung::kontakt::Kontakt::warte_auf_trigger) ein Ergebnis liefert.
    trigger: Trigger,
    /// Hat das zu bearbeitende Gleis aktuell eine Steuerung. Beeinflusst den Namen des Knopfes für
    /// "Keine Anschlüsse"/"Anschlüsse entfernen".
    hat_steuerung: bool,
}

impl Zustand {
    /// Erstelle einen neuen [`Zustand`], potentiell mit voreingestellten Anschlüssen.
    fn neu(option_kontakt: &Option<KontaktSerialisiert>, hat_steuerung: bool) -> Self {
        let (name, anschluss, trigger) =
            if let Some(KontaktSerialisiert { name, anschluss, trigger }) = option_kontakt {
                (name.0.clone(), anschluss.clone(), *trigger)
            } else {
                (String::new(), InputSerialisiert::Pin { pin: 0 }, Trigger::RisingEdge)
            };
        Zustand { name, anschluss, trigger, hat_steuerung }
    }
}

/// Interne Nachricht für Interaktion mit einem [`Auswahl`]-Widget.
#[derive(Debug, Clone)]
#[allow(variant_size_differences)]
enum InterneNachricht {
    /// Neuer gewählter Name.
    Name(String),
    /// Neuer gewählter Anschluss.
    Anschluss(InputSerialisiert),
    /// Neuer gewählter Trigger.
    Trigger(Trigger),
    /// Festlegen des aktuell gewählten Kontaktes.
    Festlegen,
    /// Entfernen des aktuellen Kontaktes für das bearbeitete Gleis.
    Entfernen,
    /// Schließen des Auswahl-Dialogs.
    Schließen,
}

/// Nachricht einer [`Auswahl`].
#[derive(Debug, Clone)]
pub enum Nachricht {
    /// Steuerung einer Weiche anpassen.
    Festlegen(Option<KontaktSerialisiert>),
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
}

/// Widget zur Auswahl der Anschlüsse eines [`Kontaktes`](crate::steuerung::kontakt::Kontakt).
#[derive(Debug)]
pub struct Auswahl<'t, Thema, R>(MapMitZustand<'t, Zustand, InterneNachricht, Nachricht, Thema, R>);

impl<'t, Thema, R> Auswahl<'t, Thema, R>
where
    R: 't + Renderer + text_core::Renderer<Font = Font>,
    Thema: 't
        + button::StyleSheet
        + card::StyleSheet
        + container::StyleSheet
        + number_input::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + tab_bar::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <Thema as scrollable::StyleSheet>::Style: From<Sammlung>,
    <Thema as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle eine neue [`Auswahl`].
    #[must_use]
    pub fn neu(
        gleis_art: &'t str,
        kontakt: Option<KontaktSerialisiert>,
        hat_steuerung: bool,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_zustand = move || Zustand::neu(&kontakt.clone(), hat_steuerung);
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
                    vec![Nachricht::Festlegen(Some(KontaktSerialisiert::neu(
                        Name(zustand.name.clone()),
                        zustand.anschluss.clone(),
                        zustand.trigger,
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

    /// Erzeuge die Widget-Hierarchie für ein [`Auswahl`]-Widget.
    fn erzeuge_element(
        weichen_art: &'t str,
        zustand: &Zustand,
        lager: &'t Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'t, InterneNachricht, Thema, R> {
        let Zustand { name, anschluss, trigger, hat_steuerung } = zustand;
        let mut column = Column::new();
        let text_input = TextInput::new("<Name>", name)
            .on_input(InterneNachricht::Name)
            .width(Length::Fixed(200.));
        column = column.push(text_input);
        let anschluss_auswahl = Element::from(anschluss::Auswahl::neu_input_s(
            Some(anschluss),
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
        let card: Card<'t, InterneNachricht, Thema, R> = Card::new(Text::new(weichen_art), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        card.into()
    }
}

impl<'t, Thema: 't, R: 't + Renderer> From<Auswahl<'t, Thema, R>>
    for Element<'t, Nachricht, Thema, R>
{
    fn from(anzeige: Auswahl<'t, Thema, R>) -> Self {
        Element::from(anzeige.0)
    }
}
