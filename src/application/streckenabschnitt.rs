//! Anzeige & Erstellen eines [`Streckenabschnittes`](Streckenabschnitt).

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
    ops::DerefMut,
};

use iced_aw::{
    native::card::{self, Card},
    style::{number_input, tab_bar},
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

use crate::{
    anschluss::{de_serialisieren::Serialisiere, polarität::Polarität, OutputSerialisiert},
    application::{
        anschluss,
        bootstrap::{Bootstrap, Icon},
        farbwahl::Farbwahl,
        map_mit_zustand::MapMitZustand,
        modal, style,
    },
    argumente::I2cSettings,
    gleis::gleise::Gleise,
    steuerung::{
        geschwindigkeit::{self, Leiter},
        streckenabschnitt::{Name, Streckenabschnitt, StreckenabschnittSerialisiert},
    },
    typen::farbe::Farbe,
    util::unicase_ord::UniCaseOrd,
};

/// Eine Nachricht des [`Anzeige`]-Widgets.
#[derive(Debug, Clone, Copy)]
pub enum AnzeigeNachricht {
    /// Einstellen ob ein Klick auf ein Gleis den [`Streckenabschnitt`]
    /// zum aktuellen Streckenabschnitt ändert.
    Festlegen(bool),
}

/// Widget zur Anzeige des aktuellen [`Streckenabschnittes`](Streckenabschnitt),
/// sowie Buttons zum Öffnen des Auswahl-Fensters.
pub struct Anzeige<'a, Overlay, R> {
    /// Das [`iced_core::widget::Widget`].
    element: Element<'a, modal::Nachricht<Overlay, AnzeigeNachricht>, R>,
}

impl<Overlay, R> Debug for Anzeige<'_, Overlay, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'a, Overlay, R> Anzeige<'a, Overlay, R>
where
    Overlay: 'a + Clone,
    R: 'a + text_core::Renderer,
    <R as Renderer>::Theme:
        container::StyleSheet + button::StyleSheet + checkbox::StyleSheet + text::StyleSheet,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
{
    /// Erstelle eine neue [`Anzeige`].
    pub fn neu(zustand: &'a Option<(Name, Farbe)>, festlegen: bool, overlay: Overlay) -> Self {
        let mut children = Vec::new();
        // TODO Assoziierte Geschwindigkeit berücksichtigen
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
                .push(
                    Button::new(Text::new("Auswählen"))
                        .on_press(modal::Nachricht::ZeigeOverlay(overlay)),
                )
                .push(
                    Checkbox::new("Festlegen", festlegen, |neu_festlegen| {
                        modal::Nachricht::Underlay(AnzeigeNachricht::Festlegen(neu_festlegen))
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

/// Zustand des Auswahl-Fensters für [`Streckenabschnitte`](Streckenabschnitt).
#[derive(Debug, PartialEq)]
struct AuswahlZustand {
    /// Der aktuell gewählte Name.
    neu_name: String,
    /// Die aktuell gewählte Farbe.
    neu_farbe: Farbe,
    /// Der aktuell gewählte Anschluss.
    neu_anschluss: OutputSerialisiert,
    /// Bekannte Streckenabschnitte.
    streckenabschnitte: BTreeMap<UniCaseOrd<Name>, (String, Farbe, OutputSerialisiert)>,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [`AuswahlZustand`].
    fn neu<L: Leiter, AktualisierenNachricht>(
        startwert: Option<(Name, StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>)>,
        gleise: &Gleise<L, AktualisierenNachricht>,
    ) -> AuswahlZustand {
        // TODO assoziierte Geschwindigkeit berücksichtigen
        let (neu_name, neu_farbe, neu_anschluss) =
            if let Some((name, streckenabschnitt, _geschwindigkeit)) = startwert {
                (name.0, streckenabschnitt.farbe, streckenabschnitt.anschluss())
            } else {
                (
                    String::new(),
                    Farbe { rot: 1., grün: 1., blau: 1. },
                    OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
                )
            };
        AuswahlZustand {
            neu_name,
            neu_farbe,
            neu_anschluss,
            streckenabschnitte: gleise.aus_allen_streckenabschnitten(|name, streckenabschnitt| {
                Self::iter_map((name, streckenabschnitt))
            }),
        }
    }

    /// Extrahiere zur Anzeige benötigte Informationen aus einen [`Streckenabschnitt`].
    fn iter_map(
        (name, streckenabschnitt): (&Name, &Streckenabschnitt),
    ) -> (UniCaseOrd<Name>, (String, Farbe, OutputSerialisiert)) {
        let anschluss = &*streckenabschnitt.lock_anschluss();
        (
            UniCaseOrd::neu(name.clone()),
            (anschluss.to_string(), streckenabschnitt.farbe, anschluss.serialisiere()),
        )
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
pub struct Auswahl<'a, R: Renderer>(
    MapMitZustand<'a, AuswahlZustand, InterneAuswahlNachricht, AuswahlNachricht, R>,
);

impl<'a, R> Auswahl<'a, R>
where
    R: 'a + text_core::Renderer<Font = Font>,
    <R as Renderer>::Theme: card::StyleSheet
        + text::StyleSheet
        + scrollable::StyleSheet
        + container::StyleSheet
        + button::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + radio::StyleSheet,
    <<R as Renderer>::Theme as button::StyleSheet>::Style: From<style::Button>,
    <<R as Renderer>::Theme as container::StyleSheet>::Style: From<style::Container>,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<style::Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<style::TabBar>,
{
    /// Erstelle eine neue [`Auswahl`].
    pub fn neu<L: Leiter, AktualisierenNachricht>(
        startwert: Option<(Name, StreckenabschnittSerialisiert, Option<geschwindigkeit::Name>)>,
        gleise: &'a Gleise<L, AktualisierenNachricht>,
        scrollable_style: style::Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_zustand = move || AuswahlZustand::neu(startwert.clone(), gleise);
        let erzeuge_element = move |zustand: &AuswahlZustand| {
            Self::erzeuge_element(zustand, scrollable_style, settings)
        };
        let mapper = |interne_nachricht,
                      zustand: &mut dyn DerefMut<Target = AuswahlZustand>,
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
                        Name(zustand.neu_name.clone()),
                        zustand.neu_farbe,
                        zustand.neu_anschluss.clone(),
                    );
                    vec![nachricht]
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    vec![AuswahlNachricht::Lösche(name)]
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
                InterneAuswahlNachricht::Bearbeiten(_geschwindigkeit, name, farbe, anschluss) => {
                    // TODO assoziierte Geschwindigkeit berücksichtigen
                    zustand.neu_name = name.0;
                    zustand.neu_farbe = farbe;
                    zustand.neu_anschluss = anschluss;
                    Vec::new()
                },
            }
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Auswahl`]-Widget.
    fn erzeuge_element(
        auswahl_zustand: &AuswahlZustand,
        scrollable_style: style::Sammlung,
        settings: I2cSettings,
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
                Element::from(anschluss::Auswahl::neu_output_s(
                    Some(neu_anschluss.clone()),
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
                            .style(style::streckenabschnitt::auswahl_button(*farbe).into()),
                    )
                    .push(Button::new(Icon::neu(Bootstrap::Feather)).on_press(
                        // TODO assoziierte Geschwindigkeit berücksichtigen
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
            Text::new("Streckenabschnitt").width(Length::Fill),
            Scrollable::new(column).height(Length::Fixed(400.)).width(Length::Shrink),
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
