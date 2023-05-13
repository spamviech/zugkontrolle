//! Anzeige & Erstellen eines [Streckenabschnittes](Streckenabschnitt).

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
    ops::DerefMut,
};

use iced_aw::{
    native::card::{self, Card},
    style::{number_input, tab_bar},
};
use iced_native::{
    event,
    widget::{
        button::{self, Button},
        checkbox::{self, Checkbox},
        container::{self, Container},
        radio,
        scrollable::{self, Scrollable},
        text::{self, Text},
        text_input::{self, TextInput},
        Column, Row,
    },
    Alignment, Element, Font, Length, Renderer,
};

use crate::{
    anschluss::{polarität::Polarität, OutputSerialisiert},
    application::{anschluss, farbwahl::Farbwahl, map_mit_zustand::MapMitZustand, modal, style},
    gleis::gleise::{id::StreckenabschnittId, Gleise},
    steuerung::geschwindigkeit::{self, Leiter},
    typen::farbe::Farbe,
    unicase_ord::UniCaseOrd,
};

pub use crate::steuerung::streckenabschnitt::{Name, Streckenabschnitt};

/// Eine Nachricht des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub enum AnzeigeNachricht {
    /// Einstellen ob ein Klick auf ein Gleis den [Streckenabschnitt]
    /// zum aktuellen Streckenabschnitt ändert.
    Festlegen(bool),
}

/// Widget zur Anzeige des aktuellen [Streckenabschnittes](Streckenabschnitt),
/// sowie Buttons zum Öffnen des Auswahl-Fensters.
pub struct Anzeige<'a, Overlay, R> {
    element: Element<'a, modal::Nachricht<Overlay, AnzeigeNachricht>, R>,
}

impl<Overlay, R> Debug for Anzeige<'_, Overlay, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'a, Overlay, R> Anzeige<'a, Overlay, R>
where
    Overlay: 'a + Clone,
    R: 'a + iced_native::text::Renderer,
    <R as Renderer>::Theme:
        container::StyleSheet + button::StyleSheet + checkbox::StyleSheet + text::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    /// Erstelle eine neue [Anzeige].
    pub fn neu(
        zustand: &'a Option<(StreckenabschnittId, Farbe)>,
        festlegen: bool,
        overlay: Overlay,
    ) -> Self {
        let mut children = Vec::new();
        // TODO Assoziierte Geschwindigkeit berücksichtigen
        let style = if let Some((streckenabschnitt_id, farbe)) = zustand {
            children.push(Text::new(&streckenabschnitt_id.name.0).into());
            style::streckenabschnitt::anzeige_farbe(*farbe)
        } else {
            children.push(
                Container::new(Text::new("<Streckenabschnitt>"))
                    .style(style::Container::Pcf8574Beschreibung)
                    .into(),
            );
            style::streckenabschnitt::anzeige_deaktiviert()
        };
        children.push(
            Row::new()
                .push(
                    Button::new(Text::new("Auswählen"))
                        .on_press(modal::Nachricht::ZeigeOverlay(overlay)),
                )
                .push(
                    Checkbox::new("Festlegen", festlegen, |festlegen| {
                        modal::Nachricht::Underlay(AnzeigeNachricht::Festlegen(festlegen))
                    })
                    .spacing(0),
                )
                .spacing(1)
                .into(),
        );
        let container = Container::new(
            Column::with_children(children).spacing(1).align_items(Alignment::Center),
        )
        .padding(1)
        .style(style);
        Anzeige { element: container.into() }
    }
}

impl<'a, Overlay, R: 'a + Renderer> From<Anzeige<'a, Overlay, R>>
    for Element<'a, modal::Nachricht<Overlay, AnzeigeNachricht>, R>
{
    fn from(auswahl: Anzeige<'a, Overlay, R>) -> Self {
        auswahl.element
    }
}

/// Zustand des Auswahl-Fensters für [Streckenabschnitte](Streckenabschnitt).
#[derive(Debug, PartialEq)]
struct AuswahlZustand {
    neu_name: String,
    neu_farbe: Farbe,
    neu_anschluss: OutputSerialisiert,
    streckenabschnitte: BTreeMap<UniCaseOrd<Name>, (String, Farbe)>,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [AuswahlZustand].
    fn neu<L: Leiter>(gleise: &Gleise<L>) -> AuswahlZustand {
        // TODO assoziierte Geschwindigkeit berücksichtigen
        AuswahlZustand {
            neu_name: String::new(),
            neu_farbe: Farbe { rot: 1., grün: 1., blau: 1. },
            neu_anschluss: OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
            streckenabschnitte: gleise.aus_allen_streckenabschnitten(
                |streckenabschnitt_id, streckenabschnitt| {
                    Self::iter_map((streckenabschnitt_id.name, streckenabschnitt))
                },
            ),
        }
    }

    fn iter_map(
        (name, streckenabschnitt): (&Name, &Streckenabschnitt),
    ) -> (UniCaseOrd<Name>, (String, Farbe)) {
        (
            UniCaseOrd::neu(name.clone()),
            (streckenabschnitt.lock_anschluss().to_string(), streckenabschnitt.farbe.clone()),
        )
    }

    /// Ersetze die angezeigten Streckenabschnitte mit dem Argument.
    fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(Self::iter_map));
    }

    /// Entferne den Streckenabschnitt mit übergebenen Namen.
    fn entfernen(&mut self, name: &Name) {
        let _ = self.streckenabschnitte.remove(&UniCaseOrd::neu(name.clone()));
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let (key, value) = Self::iter_map((name, streckenabschnitt));
        let _ = self.streckenabschnitte.insert(key, value);
    }

    /// Erhalte den aktuell konfigurierten Streckenabschnitt.
    fn streckenabschnitt(&self) -> (Name, Farbe, OutputSerialisiert) {
        (Name(self.neu_name.clone()), self.neu_farbe, self.neu_anschluss.clone())
    }
}

#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    Schließe,
    Wähle(Option<(Name, Farbe)>),
    Hinzufügen,
    Lösche(Name),
    Name(String),
    FarbeBestimmen(Farbe),
    Anschluss(OutputSerialisiert),
}

/// Nachricht des Auswahl-Fensters für [Streckenabschnitte](Streckenabschnitt).
#[derive(Debug)]
pub enum AuswahlNachricht {
    /// Schließe das Auswahl-Fenster.
    Schließe,
    /// Wähle den aktuellen Streckenabschnitt.
    Wähle(Option<(StreckenabschnittId, Farbe)>),
    /// Füge einen neuen Streckenabschnitt hinzu.
    Hinzufügen(Option<geschwindigkeit::Name>, Name, Farbe, OutputSerialisiert),
    /// Lösche einen Streckenabschnitt.
    Lösche(StreckenabschnittId),
}

/// Auswahl-Fenster für [Streckenabschnitte](Streckenabschnitt).
#[derive(Debug)]
pub struct Auswahl<'a, R: Renderer>(
    MapMitZustand<'a, AuswahlZustand, InterneAuswahlNachricht, AuswahlNachricht, R>,
);

impl<'a, R> Auswahl<'a, R>
where
    R: 'a + iced_native::text::Renderer<Font = Font>,
    <R as Renderer>::Theme: card::StyleSheet
        + text::StyleSheet
        + scrollable::StyleSheet
        + container::StyleSheet
        + button::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + radio::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
    <<R as Renderer>::Theme as button::StyleSheet>::Style: From<style::Button>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<style::TabBar>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<L: Leiter>(gleise: &'a Gleise<L>) -> Self {
        let erzeuge_zustand = || AuswahlZustand::neu(gleise);
        let erzeuge_element = Self::erzeuge_element;
        let mapper = |interne_nachricht,
                      zustand: &mut dyn DerefMut<Target = AuswahlZustand>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            let erstelle_id = |name| StreckenabschnittId { geschwindigkeit: None, name };
            match interne_nachricht {
                InterneAuswahlNachricht::Schließe => vec![AuswahlNachricht::Schließe],
                InterneAuswahlNachricht::Wähle(wahl) => {
                    vec![
                        AuswahlNachricht::Wähle(
                            wahl.map(|(name, farbe)| (erstelle_id(name), farbe)),
                        ),
                        AuswahlNachricht::Schließe,
                    ]
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        None,
                        Name(zustand.neu_name.clone()),
                        zustand.neu_farbe,
                        zustand.neu_anschluss.clone(),
                    );
                    vec![nachricht]
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    vec![AuswahlNachricht::Lösche(erstelle_id(name))]
                },
                InterneAuswahlNachricht::Name(name) => {
                    zustand.neu_name = name;
                    Vec::new()
                },
                InterneAuswahlNachricht::FarbeBestimmen(farbe) => {
                    zustand.neu_farbe = farbe;
                    Vec::new()
                },
                InterneAuswahlNachricht::Anschluss(anschluss) => {
                    zustand.neu_anschluss = anschluss;
                    Vec::new()
                },
            }
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    fn erzeuge_element(
        auswahl_zustand: &AuswahlZustand,
    ) -> Element<'a, InterneAuswahlNachricht, R> {
        let AuswahlZustand { neu_name, neu_farbe, neu_anschluss, streckenabschnitte } =
            auswahl_zustand;

        let einstellungen = Row::new()
            .push(
                TextInput::new("<Name>", neu_name)
                    .on_input(InterneAuswahlNachricht::Name)
                    .width(Length::Fixed(200.)),
            )
            .push(Farbwahl::neu(&InterneAuswahlNachricht::FarbeBestimmen).durchmesser(50))
            .push(
                Element::from(anschluss::Auswahl::neu_output_s(Some(neu_anschluss.clone())))
                    .map(InterneAuswahlNachricht::Anschluss),
            );
        let hinzufügen =
            Button::new(Text::new("Hinzufügen")).on_press(InterneAuswahlNachricht::Hinzufügen);

        let mut column = Column::new()
            .push(
                Container::new(Column::new().push(einstellungen).push(hinzufügen))
                    .style(style::streckenabschnitt::auswahl_container(*neu_farbe)),
            )
            .push(Button::new(Text::new("Keinen")).on_press(InterneAuswahlNachricht::Wähle(None)))
            .width(Length::Shrink);
        for (name, (anschluss, farbe)) in streckenabschnitte {
            column = column.push(
                Row::new()
                    .push(
                        Button::new(Text::new(format!("{name}: {anschluss:?}")))
                            .on_press(InterneAuswahlNachricht::Wähle(Some((
                                name.clone().into_inner(),
                                *farbe,
                            ))))
                            .style(style::streckenabschnitt::auswahl_button(*farbe).into()),
                    )
                    .push(
                        Button::new(Text::new("X"))
                            .on_press(InterneAuswahlNachricht::Lösche(name.clone().into_inner())),
                    ),
            );
        }

        let card = Card::new(
            Text::new("Streckenabschnitt").width(Length::Fill),
            Scrollable::new(column).width(Length::Shrink),
        )
        .on_close(InterneAuswahlNachricht::Schließe)
        .width(Length::Shrink);
        card.into()
    }
}

impl<'a, R> From<Auswahl<'a, R>> for Element<'a, AuswahlNachricht, R>
where
    R: 'a + Renderer,
{
    fn from(auswahl: Auswahl<'a, R>) -> Self {
        Element::from(auswahl.0)
    }
}
