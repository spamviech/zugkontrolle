//! Anzeige & Erstellen eines [Streckenabschnittes](Streckenabschnitt).

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_aw::Card;
use iced_native::{
    event::{self, Event},
    text,
    widget::{
        tree::{self, Tag, Tree},
        Button, Checkbox, Column, Container, Row, Scrollable, Text, TextInput,
    },
    Alignment, Clipboard, Element, Font, Layout, Length, Point, Renderer, Shell, Widget,
};

use crate::{
    anschluss::{polarität::Polarität, OutputSerialisiert},
    application::{anschluss, farbwahl::Farbwahl, macros::widget_newtype_methods},
    gleis::gleise::{id::StreckenabschnittId, Gleise},
    steuerung::geschwindigkeit::{self, Leiter},
    typen::farbe::Farbe,
    unicase_ord::UniCaseOrd,
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
}

impl AnzeigeZustand {
    /// Erstelle einen neuen [AnzeigeZustand].
    pub fn neu() -> Self {
        AnzeigeZustand { aktuell: None }
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
    element: Element<'a, AnzeigeNachricht, R>,
}

impl<R> Debug for Anzeige<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'a, R: 'a + text::Renderer> Anzeige<'a, R> {
    /// Erstelle eine neue [Anzeige].
    pub fn neu(zustand: &'a mut AnzeigeZustand, festlegen: bool) -> Self {
        let mut children = Vec::new();
        // TODO Assoziierte Geschwindigkeit berücksichtigen
        let style = if let Some((streckenabschnitt_id, farbe)) = &zustand.aktuell {
            children.push(Text::new(&streckenabschnitt_id.name.0).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(
                Container::new(Text::new("<Streckenabschnitt>")).style(style::Beschreibung).into(),
            );
            style::Anzeige::Deaktiviert
        };
        children.push(
            Row::new()
                .push(Button::new(Text::new("Auswählen")).on_press(AnzeigeNachricht::Auswählen))
                .push(Checkbox::new(festlegen, "Festlegen", AnzeigeNachricht::Festlegen).spacing(0))
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

impl<R: Renderer> Widget<AnzeigeNachricht, R> for Anzeige<'_, R> {
    widget_newtype_methods! {element, R, AnzeigeNachricht}
}

impl<'a, R: 'a + Renderer> From<Anzeige<'a, R>> for Element<'a, AnzeigeNachricht, R> {
    fn from(auswahl: Anzeige<'a, R>) -> Self {
        Element::new(auswahl)
    }
}

/// Zustand des Auswahl-Fensters für [Streckenabschnitte](Streckenabschnitt).
#[derive(Debug)]
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
            streckenabschnitte: gleise
                .streckenabschnitte()
                .map(|(streckenabschnitt_id, streckenabschnitt)| {
                    Self::iter_map((streckenabschnitt_id.name, streckenabschnitt))
                })
                .collect(),
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
        (Name(self.neu_name.clone()), self.neu_farbe, self.neu_anschluss)
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
pub struct Auswahl<'a, L: Leiter, R: Renderer> {
    element: Element<'a, InterneAuswahlNachricht, R>,
    gleise: &'a Gleise<L>,
}

impl<L, R> Debug for Auswahl<'_, L, R>
where
    L: Debug + Leiter,
    <L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug,
    <L as Leiter>::UmdrehenZeit: Debug,
    <L as Leiter>::Fahrtrichtung: Debug,
    R: Renderer,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("element", &"<Element>")
            .field("gleise", &self.gleise)
            .finish()
    }
}

impl<'a, L: Leiter, R: 'a + text::Renderer<Font = Font>> Auswahl<'a, L, R> {
    /// Erstelle eine neue [Auswahl].
    pub fn neu(gleise: &Gleise<L>) -> Self {
        Auswahl { element: Self::erzeuge_element(&AuswahlZustand::neu(gleise)), gleise }
    }

    fn erzeuge_element(
        auswahl_zustand: &'a AuswahlZustand,
    ) -> Element<'a, InterneAuswahlNachricht, R> {
        let AuswahlZustand { neu_name, neu_farbe, neu_anschluss, streckenabschnitte } =
            auswahl_zustand;
        let card = Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
            let mut column = Column::new()
                .push(
                    Container::new(
                        Column::new()
                            .push(
                                Row::new()
                                    .push(
                                        TextInput::new(
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
                                        Element::from(anschluss::Auswahl::neu_output(None))
                                            .map(InterneAuswahlNachricht::Anschluss),
                                    ),
                            )
                            .push(
                                Button::new(Text::new("Hinzufügen"))
                                    .on_press(InterneAuswahlNachricht::Hinzufügen),
                            ),
                    )
                    .style(style::Auswahl(*neu_farbe)),
                )
                .push(
                    Button::new(Text::new("Keinen"))
                        .on_press(InterneAuswahlNachricht::Wähle(None)),
                )
                .width(Length::Shrink);
            for (name, (anschluss, farbe)) in streckenabschnitte {
                column =
                    column.push(
                        Row::new()
                            .push(
                                Button::new(Text::new(&format!("{name}: {anschluss:?}")))
                                    .on_press(InterneAuswahlNachricht::Wähle(Some((
                                        name.clone().into_inner(),
                                        *farbe,
                                    ))))
                                    .style(style::Auswahl(*farbe)),
                            )
                            .push(Button::new(Text::new("X")).on_press(
                                InterneAuswahlNachricht::Lösche(name.clone().into_inner()),
                            )),
                    );
            }
            Scrollable::new(column)
        })
        .on_close(InterneAuswahlNachricht::Schließe)
        .width(Length::Shrink);
        card.into()
    }
}

impl<L: Leiter, R: text::Renderer<Font = Font>> Widget<AuswahlNachricht, R> for Auswahl<'_, L, R> {
    widget_newtype_methods! {element,  R}

    fn tag(&self) -> Tag {
        Tag::of::<AuswahlZustand>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(AuswahlZustand::neu(&self.gleise))
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, AuswahlNachricht>,
    ) -> event::Status {
        let mut container_messages = Vec::new();
        let mut container_shell = Shell::new(&mut container_messages);
        let mut status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut container_shell,
        );
        if container_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if container_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }
        let zustand: &mut AuswahlZustand = state.state.downcast_mut();
        let mut zustand_geändert = false;
        for message in container_messages {
            let erstelle_id = |name| StreckenabschnittId { geschwindigkeit: None, name };
            match message {
                InterneAuswahlNachricht::Schließe => shell.publish(AuswahlNachricht::Schließe),
                InterneAuswahlNachricht::Wähle(wahl) => {
                    shell.publish(AuswahlNachricht::Wähle(
                        wahl.map(|(name, farbe)| (erstelle_id(name), farbe)),
                    ));
                    shell.publish(AuswahlNachricht::Schließe)
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        None,
                        Name(zustand.neu_name.clone()),
                        zustand.neu_farbe,
                        zustand.neu_anschluss.clone(),
                    );
                    shell.publish(nachricht);
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    shell.publish(AuswahlNachricht::Lösche(erstelle_id(name)))
                },
                InterneAuswahlNachricht::Name(name) => {
                    zustand.neu_name = name;
                    zustand_geändert = true;
                },
                InterneAuswahlNachricht::FarbeBestimmen(farbe) => {
                    zustand.neu_farbe = farbe;
                    zustand_geändert = true;
                },
                InterneAuswahlNachricht::Anschluss(anschluss) => {
                    zustand.neu_anschluss = anschluss;
                    zustand_geändert = true;
                },
            }
            status = event::Status::Captured;
        }
        if zustand_geändert {
            self.element = Self::erzeuge_element(zustand)
        }
        status
    }

    fn overlay<'a>(
        &'a self,
        _state: &'a mut Tree,
        _layout: Layout<'_>,
        _renderer: &R,
    ) -> Option<iced_native::overlay::Element<'a, AuswahlNachricht, R>> {
        None
        // TODO overlay von self.element verwenden?
    }
}

impl<'a, L, R> From<Auswahl<'a, L, R>> for Element<'a, AuswahlNachricht, R>
where
    L: Leiter,
    R: 'a + text::Renderer<Font = Font>,
{
    fn from(auswahl: Auswahl<'a, L, R>) -> Self {
        Element::new(auswahl)
    }
}
