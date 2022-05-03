//! Anzeige & Erstellen eines [Streckenabschnittes](Streckenabschnitt).

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    event, mouse,
    text::{self, Text},
    widget::{
        button::{self, Button},
        checkbox::{self, Checkbox},
        column::{self, Column},
        container::{self, Container},
        radio,
        row::{self, Row},
        scrollable::{self, Scrollable},
        text_input::{self, TextInput},
    },
    Alignment, Clipboard, Element, Event, Layout, Length, Point, Renderer, Widget,
};

use crate::{
    anschluss::{polarität::Polarität, OutputSerialisiert},
    application::{
        anschluss, farbwahl::Farbwahl, macros::reexport_no_event_methods, style::tab_bar::TabBar,
    },
    gleis::gleise::{id::StreckenabschnittId, Gleise},
    steuerung::geschwindigkeit::{self, Leiter},
    typen::farbe::Farbe,
};
pub use crate::{
    application::style::streckenabschnitt as style,
    steuerung::streckenabschnitt::{Name, Streckenabschnitt},
};

/// Zustand des Widgets zur Anzeige des aktuellen [Streckenabschnittes](Streckenabschnitt),
/// sowie [Buttons](iced::Button) zum Öffnen des Auswahl-Fensters.
#[derive(Debug)]
pub struct AnzeigeZustand {
    /// Der aktuelle Streckenabschnitt.
    aktuell: Option<(StreckenabschnittId, Farbe)>,
    /// Zustand des Buttons zum Öffnen des Auswahl-Fensters.
    auswählen: button::State,
}

impl AnzeigeZustand {
    /// Erstelle einen neuen [AnzeigeZustand].
    pub fn neu() -> Self {
        AnzeigeZustand { aktuell: None, auswählen: button::State::new() }
    }

    /// Der aktuelle [Streckenabschnitt].
    #[inline(always)]
    pub fn aktuell(&self) -> &Option<(StreckenabschnittId, Farbe)> {
        &self.aktuell
    }

    /// Eine veränderliche Referenz auf den aktuellen [Streckenabschnitt].
    #[inline(always)]
    pub fn aktuell_mut(&mut self) -> &mut Option<(StreckenabschnittId, Farbe)> {
        &mut self.aktuell
    }

    /// Setze den aktuellen [Streckenabschnitt].
    #[inline(always)]
    pub fn setze_aktuell(&mut self, streckenabschnitt: StreckenabschnittId, farbe: Farbe) {
        self.aktuell = Some((streckenabschnitt, farbe))
    }

    /// Entferne den aktuellen [Streckenabschnitt].
    #[inline(always)]
    pub fn entferne_aktuell(&mut self) {
        self.aktuell = None
    }
}

/// Eine Nachricht des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub enum AnzeigeNachricht {
    /// Öffne das Auswahl-Fenster für [Streckenabschnitte](Streckenabschnitt).
    Auswählen,
    /// Einstellen ob ein Klick auf ein Gleis den [Streckenabschnitt]
    /// zum aktuellen Streckenabschnitt ändert.
    Festlegen(bool),
}

/// Widget zur Anzeige des aktuellen [Streckenabschnittes](Streckenabschnitt),
/// sowie Buttons zum Öffnen des Auswahl-Fensters.
pub struct Anzeige<'a, R> {
    container: Container<'a, AnzeigeNachricht, R>,
}

impl<R> Debug for Anzeige<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Anzeige").field("container", &"<Container>").finish()
    }
}

impl<'a, R> Anzeige<'a, R> {
    /// Erstelle eine neue [Anzeige].
    pub fn neu(zustand: &'a mut AnzeigeZustand, festlegen: bool) -> Self {
        let mut children = Vec::new();
        let column = Column::new()
            .push(Container::new(Text::new("Streckenabschnitt")).style(style::Beschreibung));
        // TODO Assoziierte Geschwindigkeit berücksichtigen
        let style = if let Some((streckenabschnitt_id, farbe)) = &zustand.aktuell {
            children.push(column.push(Text::new(&streckenabschnitt_id.name.0)).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(column.push(Text::new("<Name>")).into());
            style::Anzeige::Deaktiviert
        };
        children.push(
            Column::new()
                .push(
                    Button::new(&mut zustand.auswählen, Text::new("Auswählen"))
                        .on_press(AnzeigeNachricht::Auswählen),
                )
                .push(Checkbox::new(festlegen, "Festlegen", AnzeigeNachricht::Festlegen).spacing(0))
                .spacing(1)
                .into(),
        );
        let container =
            Container::new(Row::with_children(children).spacing(1).align_items(Alignment::Center))
                .style(style);
        Anzeige { container }
    }
}

impl<'a, R: Renderer> Widget<AnzeigeNachricht, R> for Anzeige<'a, R> {
    reexport_no_event_methods! {Container<'a, AnzeigeNachricht, R>, container, AnzeigeNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<AnzeigeNachricht>,
    ) -> event::Status {
        self.container.on_event(event, layout, cursor_position, renderer, clipboard, messages)
    }
}

impl<'a, R> From<Anzeige<'a, R>> for Element<'a, AnzeigeNachricht, R> {
    fn from(auswahl: Anzeige<'a, R>) -> Self {
        Element::new(auswahl)
    }
}

/// Zustand des Auswahl-Fensters für [Streckenabschnitte](Streckenabschnitt).
#[derive(Debug)]
pub struct AuswahlZustand {
    neu_name: String,
    neu_farbe: Farbe,
    neu_anschluss: OutputSerialisiert,
    neu_name_zustand: text_input::State,
    neu_anschluss_zustand: anschluss::Zustand<anschluss::Output>,
    neu_button_zustand: button::State,
    none_button_zustand: button::State,
    streckenabschnitte: BTreeMap<Name, (String, Farbe, button::State, button::State)>,
    scrollable_zustand: scrollable::State,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [AuswahlZustand].
    pub fn neu<L: Leiter>(gleise: &Gleise<L>) -> AuswahlZustand {
        // TODO assoziierte Geschwindigkeit berücksichtigen
        AuswahlZustand {
            neu_name: String::new(),
            neu_farbe: Farbe { rot: 1., grün: 1., blau: 1. },
            neu_anschluss: OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
            neu_name_zustand: text_input::State::new(),
            neu_anschluss_zustand: anschluss::Zustand::neu_output(),
            neu_button_zustand: button::State::new(),
            none_button_zustand: button::State::new(),
            streckenabschnitte: gleise
                .streckenabschnitte()
                .map(|(streckenabschnitt_id, streckenabschnitt)| {
                    Self::iter_map((streckenabschnitt_id.name, streckenabschnitt))
                })
                .collect(),
            scrollable_zustand: scrollable::State::new(),
        }
    }

    fn iter_map<'t>(
        (name, streckenabschnitt): (&'t Name, &'t Streckenabschnitt),
    ) -> (Name, (String, Farbe, button::State, button::State)) {
        (
            name.clone(),
            (
                streckenabschnitt.lock_anschluss().to_string(),
                streckenabschnitt.farbe.clone(),
                button::State::new(),
                button::State::new(),
            ),
        )
    }

    /// Ersetze die angezeigten Streckenabschnitte mit dem Argument.
    pub fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(Self::iter_map));
    }

    /// Entferne den Streckenabschnitt mit übergebenen Namen.
    pub fn entfernen(&mut self, name: &Name) {
        let _ = self.streckenabschnitte.remove(name);
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    pub fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let (key, value) = Self::iter_map((name, streckenabschnitt));
        let _ = self.streckenabschnitte.insert(key, value);
    }

    /// Erhalte den aktuell konfigurierten Streckenabschnitt.
    pub fn streckenabschnitt(&self) -> (Name, Farbe, OutputSerialisiert) {
        (Name(self.neu_name.clone()), self.neu_farbe, self.neu_anschluss_zustand.output_anschluss())
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
pub struct Auswahl<'a, R: Renderer + card::Renderer> {
    card: Card<'a, InterneAuswahlNachricht, R>,
    neu_name: &'a mut String,
    neu_farbe: &'a mut Farbe,
    neu_anschluss: &'a mut OutputSerialisiert,
}

impl<R: Renderer + card::Renderer> Debug for Auswahl<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("card", &"<Card>")
            .field("neu_name", &self.neu_name)
            .field("neu_farbe", &self.neu_farbe)
            .field("neu_anschluss", &self.neu_anschluss)
            .finish()
    }
}

impl<'a, R: Renderer + card::Renderer> Auswahl<'a, R> {
    /// Erstelle eine neue [Auswahl].
    pub fn neu(auswahl_zustand: &'a mut AuswahlZustand) -> Self {
        let AuswahlZustand {
            neu_name,
            neu_farbe,
            neu_anschluss,
            neu_name_zustand,
            neu_anschluss_zustand,
            neu_button_zustand,
            none_button_zustand,
            streckenabschnitte,
            scrollable_zustand,
        } = auswahl_zustand;
        let card = Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
            let mut scrollable = Scrollable::new(scrollable_zustand)
                .push(
                    Container::new(
                        Column::new()
                            .push(
                                Row::new()
                                    .push(
                                        TextInput::new(
                                            neu_name_zustand,
                                            "<Name>",
                                            neu_name,
                                            InterneAuswahlNachricht::Name,
                                        )
                                        .width(Length::Units(200)),
                                    )
                                    .push(
                                        Farbwahl::neu(&InterneAuswahlNachricht::FarbeBestimmen)
                                            .durchmesser(50),
                                    )
                                    .push(
                                        Element::from(anschluss::Auswahl::neu_output(
                                            neu_anschluss_zustand,
                                        ))
                                        .map(InterneAuswahlNachricht::Anschluss),
                                    ),
                            )
                            .push(
                                Button::new(neu_button_zustand, Text::new("Hinzufügen"))
                                    .on_press(InterneAuswahlNachricht::Hinzufügen),
                            ),
                    )
                    .style(style::Auswahl(*neu_farbe)),
                )
                .push(
                    Button::new(none_button_zustand, Text::new("Keinen"))
                        .on_press(InterneAuswahlNachricht::Wähle(None)),
                )
                .width(Length::Shrink);
            for (name, (anschluss, farbe, button_zustand, delete_zustand)) in streckenabschnitte {
                scrollable = scrollable.push(
                    Row::new()
                        .push(
                            Button::new(
                                button_zustand,
                                Text::new(&format!("{}: {:?}", name.0, anschluss)),
                            )
                            .on_press(InterneAuswahlNachricht::Wähle(Some((name.clone(), *farbe))))
                            .style(style::Auswahl(*farbe)),
                        )
                        .push(
                            Button::new(delete_zustand, Text::new("X"))
                                .on_press(InterneAuswahlNachricht::Lösche(name.clone())),
                        ),
                );
            }
            scrollable
        })
        .on_close(InterneAuswahlNachricht::Schließe)
        .width(Length::Shrink);
        Auswahl { card, neu_name, neu_farbe, neu_anschluss }
    }
}

impl<'a, R: Renderer + card::Renderer> Widget<AuswahlNachricht, R> for Auswahl<'a, R> {
    reexport_no_event_methods! {Card<'a, InterneAuswahlNachricht, R>, card, InterneAuswahlNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<AuswahlNachricht>,
    ) -> event::Status {
        let mut container_messages = Vec::new();
        let mut status = self.card.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut container_messages,
        );
        for message in container_messages {
            let erstelle_id = |name| StreckenabschnittId { geschwindigkeit: None, name };
            match message {
                InterneAuswahlNachricht::Schließe => messages.push(AuswahlNachricht::Schließe),
                InterneAuswahlNachricht::Wähle(wahl) => {
                    messages.push(AuswahlNachricht::Wähle(
                        wahl.map(|(name, farbe)| (erstelle_id(name), farbe)),
                    ));
                    messages.push(AuswahlNachricht::Schließe)
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    messages.push(AuswahlNachricht::Hinzufügen(
                        None,
                        Name(self.neu_name.clone()),
                        self.neu_farbe.clone(),
                        self.neu_anschluss.clone(),
                    ));
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    messages.push(AuswahlNachricht::Lösche(erstelle_id(name)))
                },
                InterneAuswahlNachricht::Name(name) => *self.neu_name = name,
                InterneAuswahlNachricht::FarbeBestimmen(farbe) => *self.neu_farbe = farbe,
                InterneAuswahlNachricht::Anschluss(anschluss) => *self.neu_anschluss = anschluss,
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, R: Renderer + card::Renderer> From<Auswahl<'a, R>> for Element<'a, AuswahlNachricht, R> {
    fn from(auswahl: Auswahl<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
