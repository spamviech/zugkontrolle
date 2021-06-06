//! Anzeige & Erstellen eines Streckenabschnittes.

use std::collections::BTreeMap;

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button,
    checkbox,
    column,
    container,
    event,
    mouse,
    radio,
    row,
    scrollable,
    text,
    text_input,
    Align,
    Button,
    Checkbox,
    Clipboard,
    Column,
    Container,
    Element,
    Event,
    Layout,
    Length,
    Point,
    Renderer,
    Row,
    Scrollable,
    Text,
    TextInput,
    Widget,
};

use super::{anschluss, farbwahl::Farbwahl, macros::reexport_no_event_methods};
use crate::anschluss::polarity::Polarität;
use crate::farbe::Farbe;
pub use crate::steuerung::streckenabschnitt::Name;
use crate::steuerung::Streckenabschnitt;

pub mod style;

#[derive(Debug)]
pub struct AnzeigeStatus {
    pub aktuell: Option<(Name, Farbe)>,
    pub auswählen: button::State,
}

impl AnzeigeStatus {
    pub fn neu() -> Self {
        AnzeigeStatus { aktuell: None, auswählen: button::State::new() }
    }
}

#[derive(Debug, Clone)]
pub enum AnzeigeNachricht {
    Auswählen,
    Festlegen(bool),
}

pub struct Anzeige<'a, R: Renderer + container::Renderer> {
    container: Container<'a, AnzeigeNachricht, R>,
}

impl<'a, R> Anzeige<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + text::Renderer
        + button::Renderer
        + row::Renderer
        + column::Renderer
        + checkbox::Renderer,
    <R as container::Renderer>::Style: From<style::Anzeige>,
{
    pub fn neu(status: &'a mut AnzeigeStatus, festlegen: bool) -> Self {
        let mut children = Vec::new();
        let style = if let Some((name, farbe)) = &status.aktuell {
            children.push(Text::new(&name.0).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(Text::new("<Name>").into());
            style::Anzeige::Deaktiviert
        };
        children.push(
            Column::new()
                .push(
                    Button::new(&mut status.auswählen, Text::new("Auswählen"))
                        .on_press(AnzeigeNachricht::Auswählen),
                )
                .push(Checkbox::new(festlegen, "Festlegen", AnzeigeNachricht::Festlegen).spacing(0))
                .spacing(1)
                .into(),
        );
        let container =
            Container::new(Row::with_children(children).spacing(1).align_items(Align::Center))
                .style(style);
        Anzeige { container }
    }
}

impl<'a, R: Renderer + container::Renderer> Widget<AnzeigeNachricht, R> for Anzeige<'a, R> {
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

impl<'a, R: 'a + Renderer + container::Renderer> From<Anzeige<'a, R>>
    for Element<'a, AnzeigeNachricht, R>
{
    fn from(auswahl: Anzeige<'a, R>) -> Self {
        Element::new(auswahl)
    }
}

#[derive(Debug)]
pub struct AuswahlStatus {
    neu_name: String,
    neu_farbe: Farbe,
    neu_anschluss: anschluss::OutputAnschluss,
    neu_name_state: text_input::State,
    neu_anschluss_state: anschluss::Status<anschluss::Output>,
    neu_button_state: button::State,
    none_button_state: button::State,
    streckenabschnitte: BTreeMap<Name, (String, Farbe, button::State, button::State)>,
    scrollable_state: scrollable::State,
}

impl AuswahlStatus {
    pub fn neu<'t>(
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) -> Self {
        AuswahlStatus {
            neu_name: String::new(),
            neu_farbe: Farbe { r: 1., g: 1., b: 1. },
            neu_anschluss: anschluss::OutputAnschluss::Pin {
                pin: 0, polarität: Polarität::Normal
            },
            neu_name_state: text_input::State::new(),
            neu_anschluss_state: anschluss::Status::neu_output(),
            neu_button_state: button::State::new(),
            none_button_state: button::State::new(),
            streckenabschnitte: streckenabschnitte.map(Self::iter_map).collect(),
            scrollable_state: scrollable::State::new(),
        }
    }

    fn iter_map<'t>(
        (name, streckenabschnitt): (&'t Name, &'t Streckenabschnitt),
    ) -> (Name, (String, Farbe, button::State, button::State)) {
        (
            name.clone(),
            (
                format!("{}", streckenabschnitt.anschluss),
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
    pub fn entferne(&mut self, name: &Name) {
        self.streckenabschnitte.remove(name);
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    pub fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let (key, value) = Self::iter_map((name, streckenabschnitt));
        self.streckenabschnitte.insert(key, value);
    }

    pub fn streckenabschnitt(&self) -> (Name, Farbe, anschluss::OutputAnschluss) {
        (Name(self.neu_name.clone()), self.neu_farbe, self.neu_anschluss_state.output_anschluss())
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
    Anschluss(anschluss::OutputAnschluss),
}

#[derive(Debug, Clone)]
pub enum AuswahlNachricht {
    Schließe,
    Wähle(Option<(Name, Farbe)>),
    Hinzufügen(Name, Farbe, anschluss::OutputAnschluss),
    Lösche(Name),
}

pub struct Auswahl<'a, R: Renderer + card::Renderer> {
    card: Card<'a, InterneAuswahlNachricht, R>,
    neu_name: &'a mut String,
    neu_farbe: &'a mut Farbe,
    neu_anschluss: &'a mut anschluss::OutputAnschluss,
}

impl<'a, R> Auswahl<'a, R>
where
    R: 'a
        + Renderer
        + text::Renderer
        + radio::Renderer
        + column::Renderer
        + row::Renderer
        + container::Renderer
        + button::Renderer
        + scrollable::Renderer
        + number_input::Renderer
        + tabs::Renderer
        + card::Renderer,
    <R as tab_bar::Renderer>::Style: From<anschluss::style::TabBar>,
    <R as button::Renderer>::Style: From<style::Auswahl>,
    <R as iced_native::container::Renderer>::Style: From<style::Auswahl>,
    <R as Renderer>::Output: From<(iced_graphics::Primitive, mouse::Interaction)>,
{
    pub fn neu(
        AuswahlStatus {
            neu_name,
            neu_farbe,
            neu_anschluss,
            neu_name_state,
            neu_anschluss_state,
            neu_button_state,
            none_button_state,
            streckenabschnitte,
            scrollable_state,
        }: &'a mut AuswahlStatus,
    ) -> Self {
        let card = Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
            let mut scrollable = Scrollable::new(scrollable_state)
                .push(
                    Container::new(
                        Column::new()
                            .push(
                                Row::new()
                                    .push(
                                        TextInput::new(
                                            neu_name_state,
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
                                            neu_anschluss_state,
                                        ))
                                        .map(InterneAuswahlNachricht::Anschluss),
                                    ),
                            )
                            .push(
                                Button::new(neu_button_state, Text::new("Hinzufügen"))
                                    .on_press(InterneAuswahlNachricht::Hinzufügen),
                            ),
                    )
                    .style(style::Auswahl(*neu_farbe)),
                )
                .push(
                    Button::new(none_button_state, Text::new("Keinen"))
                        .on_press(InterneAuswahlNachricht::Wähle(None)),
                )
                .width(Length::Shrink);
            for (name, (anschluss, farbe, button_state, delete_state)) in streckenabschnitte {
                scrollable = scrollable.push(
                    Row::new()
                        .push(
                            Button::new(
                                button_state,
                                Text::new(&format!("{}: {:?}", name.0, anschluss)),
                            )
                            .on_press(InterneAuswahlNachricht::Wähle(Some((name.clone(), *farbe))))
                            .style(style::Auswahl(*farbe)),
                        )
                        .push(
                            Button::new(delete_state, Text::new("X"))
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

impl<'a, R: 'a + Renderer + card::Renderer> Widget<AuswahlNachricht, R> for Auswahl<'a, R> {
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
            match message {
                InterneAuswahlNachricht::Schließe => messages.push(AuswahlNachricht::Schließe),
                InterneAuswahlNachricht::Wähle(wahl) => {
                    messages.push(AuswahlNachricht::Wähle(wahl));
                    messages.push(AuswahlNachricht::Schließe)
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    messages.push(AuswahlNachricht::Hinzufügen(
                        Name(self.neu_name.clone()),
                        self.neu_farbe.clone(),
                        self.neu_anschluss.clone(),
                    ));
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    messages.push(AuswahlNachricht::Lösche(name))
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

impl<'a, R: 'a + Renderer + card::Renderer> From<Auswahl<'a, R>>
    for Element<'a, AuswahlNachricht, R>
{
    fn from(auswahl: Auswahl<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
