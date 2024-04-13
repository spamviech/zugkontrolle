//! Anzeige & Erstellen eines [`Streckenabschnittes`](Streckenabschnitt).

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_aw::{
    style::{number_input, tab_bar},
    widgets::card::{self, Card},
};
use iced_core::{
    event, text as text_core,
    widget::text::{self, Text},
    Alignment, Element, Font, Length, Renderer,
};
use iced_widget::{
    button::{self, Button},
    checkbox::{self, Checkbox},
    container::{self, Container},
    radio,
    scrollable::{self, Scrollable},
    text_input::{self, TextInput},
    Column, Row,
};

use zugkontrolle_anschluss::{
    de_serialisieren::Serialisiere, polarität::Polarität, OutputSerialisiert,
};
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_gleis::steuerung::{
    geschwindigkeit::{self, Leiter},
    streckenabschnitt::{Name, Streckenabschnitt, StreckenabschnittSerialisiert},
};
use zugkontrolle_gleise::Gleise;
use zugkontrolle_typen::farbe::Farbe;
use zugkontrolle_util::unicase_ord::UniCaseOrd;

use crate::{
    anschluss,
    bootstrap::{Bootstrap, Icon},
    farbwahl::Farbwahl,
    map_mit_zustand::MapMitZustand,
    style,
};

/// Eine Nachricht des [`Anzeige`]-Widgets.
#[derive(Debug, Clone, Copy)]
pub enum AnzeigeNachricht {
    /// Zeige das Overlay zur Streckenabschnitt-[`Auswahl`].
    ZeigeOverlay,
    /// Einstellen ob ein Klick auf ein Gleis den [`Streckenabschnitt`]
    /// zum aktuellen Streckenabschnitt ändert.
    Festlegen(bool),
}

/// Widget zur Anzeige des aktuellen [`Streckenabschnittes`](Streckenabschnitt),
/// sowie Buttons zum Öffnen des Auswahl-Fensters.
pub struct Anzeige<'a, Thema, R> {
    /// Das [`iced_core::widget::Widget`].
    element: Element<'a, AnzeigeNachricht, Thema, R>,
}

impl<Thema, R> Debug for Anzeige<'_, Thema, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'a, Thema, R> Anzeige<'a, Thema, R>
where
    R: 'a + text_core::Renderer,
    Thema:
        'a + container::StyleSheet + button::StyleSheet + checkbox::StyleSheet + text::StyleSheet,
    <Thema as container::StyleSheet>::Style: From<style::Container>,
{
    /// Erstelle eine neue [`Anzeige`].
    #[must_use]
    pub fn neu(zustand: &'a Option<(Name, Farbe)>, festlegen: bool) -> Self {
        let mut children = Vec::new();
        let style = if let Some((streckenabschnitt_name, farbe)) = zustand {
            children.push(Text::new(&streckenabschnitt_name.0).into());
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
                .push(Button::new(Text::new("Auswählen")).on_press(AnzeigeNachricht::ZeigeOverlay))
                .push(
                    Checkbox::new("Festlegen", festlegen)
                        .on_toggle(AnzeigeNachricht::Festlegen)
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

impl<'a, Thema, R: 'a + Renderer> From<Anzeige<'a, Thema, R>>
    for Element<'a, AnzeigeNachricht, Thema, R>
{
    fn from(auswahl: Anzeige<'a, Thema, R>) -> Self {
        auswahl.element
    }
}

/// Zustand des Auswahl-Fensters für [`Streckenabschnitte`](Streckenabschnitt).
#[derive(Debug, Default, PartialEq)]
struct AuswahlZustand {
    /// Der aktuell gewählte Name.
    name: String,
    /// Die aktuell gewählte Farbe.
    farbe: Farbe,
    /// Der aktuell gewählte Anschluss.
    anschluss: OutputSerialisiert,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [`AuswahlZustand`].
    fn neu(
        startwert: &Option<(Name, StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>)>,
    ) -> AuswahlZustand {
        let (name, farbe, anschluss) =
            if let Some((name, streckenabschnitt, _geschwindigkeit)) = startwert {
                (name.0.clone(), streckenabschnitt.farbe(), streckenabschnitt.clone().anschluss())
            } else {
                (
                    String::new(),
                    Farbe { rot: 1., grün: 1., blau: 1. },
                    OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
                )
            };
        AuswahlZustand { name, farbe, anschluss }
    }
}

/// Interne Nachricht für die Interaktion mit einem [`Auswahl`]-Widget.
#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    /// Schließe das Auswahl-Fenster.
    Schließe,
    /// Wähle den aktuellen Streckenabschnitt.
    Wähle(Option<(Name, Farbe)>),
    /// Füge einen neuen Streckenabschnitt hinzu.
    Hinzufügen,
    /// Lösche einen Streckenabschnitt.
    Lösche(Name),
    /// Neuer aktuell gewählter Name.
    Name(String),
    /// Neue aktuell gewählte Farbe.
    FarbeBestimmen(Farbe),
    /// Neuer aktuell gewählter Anschluss.
    Anschluss(OutputSerialisiert),
    /// Bearbeite einen bekannten Streckenabschnitt.
    Bearbeiten(Option<geschwindigkeit::Name>, Name, Farbe, OutputSerialisiert),
}

/// Nachricht des Auswahl-Fensters für [`Streckenabschnitte`](Streckenabschnitt).
#[derive(Debug)]
pub enum AuswahlNachricht {
    /// Schließe das Auswahl-Fenster.
    Schließe,
    /// Wähle den aktuellen Streckenabschnitt.
    Wähle(Option<(Name, Farbe)>),
    /// Füge einen neuen Streckenabschnitt hinzu.
    Hinzufügen(Option<geschwindigkeit::Name>, Name, Farbe, OutputSerialisiert),
    /// Lösche einen Streckenabschnitt.
    Lösche(Name),
}

/// Auswahl-Fenster für [`Streckenabschnitte`](Streckenabschnitt).
#[derive(Debug)]
pub struct Auswahl<'a, Thema, R: Renderer>(
    MapMitZustand<'a, AuswahlZustand, InterneAuswahlNachricht, AuswahlNachricht, Thema, R>,
);

impl<'a, Thema, R> Auswahl<'a, Thema, R>
where
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + card::StyleSheet
        + text::StyleSheet
        + scrollable::StyleSheet
        + container::StyleSheet
        + button::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + radio::StyleSheet,
    <Thema as button::StyleSheet>::Style: From<style::Button>,
    <Thema as container::StyleSheet>::Style: From<style::Container>,
    <Thema as scrollable::StyleSheet>::Style: From<style::Sammlung>,
    <Thema as tab_bar::StyleSheet>::Style: From<style::TabBar>,
{
    /// Erstelle eine neue [`Auswahl`].
    pub fn neu<L: Leiter, AktualisierenNachricht>(
        startwert: &Option<(Name, StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>)>,
        gleise: &'a Gleise<L, AktualisierenNachricht>,
        scrollable_style: style::Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_element = move |zustand: &AuswahlZustand| {
            Self::erzeuge_element(
                &gleise.aus_allen_streckenabschnitten(|name, streckenabschnitt| {
                    Self::iter_map((name, streckenabschnitt))
                }),
                zustand,
                scrollable_style,
                settings,
            )
        };
        let mapper = |interne_nachricht,
                      zustand: &mut AuswahlZustand,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneAuswahlNachricht::Schließe => vec![AuswahlNachricht::Schließe],
                InterneAuswahlNachricht::Wähle(wahl) => {
                    vec![AuswahlNachricht::Wähle(wahl), AuswahlNachricht::Schließe]
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        None,
                        Name(zustand.name.clone()),
                        zustand.farbe,
                        zustand.anschluss.clone(),
                    );
                    vec![nachricht]
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    vec![AuswahlNachricht::Lösche(name)]
                },
                InterneAuswahlNachricht::Name(name) => {
                    zustand.name = name;
                    Vec::new()
                },
                InterneAuswahlNachricht::FarbeBestimmen(farbe) => {
                    zustand.farbe = farbe;
                    Vec::new()
                },
                InterneAuswahlNachricht::Anschluss(anschluss) => {
                    zustand.anschluss = anschluss;
                    Vec::new()
                },
                InterneAuswahlNachricht::Bearbeiten(_geschwindigkeit, name, farbe, anschluss) => {
                    zustand.name = name.0;
                    zustand.farbe = farbe;
                    zustand.anschluss = anschluss;
                    Vec::new()
                },
            }
        };
        Auswahl(MapMitZustand::neu(AuswahlZustand::neu(startwert), erzeuge_element, mapper))
    }

    /// Extrahiere zur Anzeige benötigte Informationen aus einen [`Streckenabschnitt`].
    fn iter_map(
        (name, streckenabschnitt): (&Name, &Streckenabschnitt),
    ) -> (UniCaseOrd<Name>, (String, Farbe, OutputSerialisiert)) {
        let anschluss = &*streckenabschnitt.lock_anschluss();
        (
            UniCaseOrd::neu(name.clone()),
            (anschluss.to_string(), streckenabschnitt.farbe(), anschluss.serialisiere()),
        )
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Auswahl`]-Widget.
    fn erzeuge_element(
        streckenabschnitte: &BTreeMap<UniCaseOrd<Name>, (String, Farbe, OutputSerialisiert)>,
        auswahl_zustand: &AuswahlZustand,
        scrollable_style: style::Sammlung,
        settings: I2cSettings,
    ) -> Element<'a, InterneAuswahlNachricht, Thema, R> {
        let AuswahlZustand { name: neu_name, farbe: neu_farbe, anschluss: neu_anschluss } =
            auswahl_zustand;

        let einstellungen = Row::new()
            .push(
                TextInput::new("<Name>", neu_name)
                    .on_input(InterneAuswahlNachricht::Name)
                    .width(Length::Fixed(200.)),
            )
            .push(Farbwahl::neu(&InterneAuswahlNachricht::FarbeBestimmen).durchmesser(50))
            .push(
                Element::from(anschluss::Auswahl::neu_output_s(
                    Some(neu_anschluss),
                    scrollable_style,
                    settings,
                ))
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
        for (name, (anschluss_str, farbe, anschluss)) in streckenabschnitte {
            column = column.push(
                Row::new()
                    .push(
                        Button::new(Text::new(format!("{name}: {anschluss_str:?}")))
                            .on_press(InterneAuswahlNachricht::Wähle(Some((
                                name.clone().into_inner(),
                                *farbe,
                            ))))
                            .style(style::streckenabschnitt::auswahl_button(*farbe)),
                    )
                    .push(Button::new(Icon::neu(Bootstrap::Feather)).on_press(
                        InterneAuswahlNachricht::Bearbeiten(
                            None,
                            name.clone().into_inner(),
                            *farbe,
                            anschluss.clone(),
                        ),
                    ))
                    .push(
                        Button::new(Text::new("X"))
                            .on_press(InterneAuswahlNachricht::Lösche(name.clone().into_inner())),
                    ),
            );
        }

        let card = Card::new(
            Text::new("Streckenabschnitt").width(Length::Shrink),
            Scrollable::new(column).height(Length::Fixed(400.)).width(Length::Shrink),
        )
        .on_close(InterneAuswahlNachricht::Schließe)
        .width(Length::Shrink);
        card.into()
    }
}

impl<'a, Thema, R> From<Auswahl<'a, Thema, R>> for Element<'a, AuswahlNachricht, Thema, R>
where
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(auswahl: Auswahl<'a, Thema, R>) -> Self {
        Element::from(auswahl.0)
    }
}
