//! Anzeige & Erstellen eines Streckenabschnittes.

use std::collections::BTreeMap;

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button,
    column,
    container,
    event,
    radio,
    row,
    scrollable,
    text,
    text_input,
    Align,
    Button,
    Clipboard,
    Color,
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

use super::{anschluss, macros::reexport_no_event_methods, style::background};
pub use crate::steuerung::streckenabschnitt::Name;
use crate::steuerung::Streckenabschnitt;

pub mod style;

#[derive(Debug)]
pub struct AnzeigeStatus {
    pub aktuell: Option<(Name, Color)>,
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
}

pub struct Anzeige<'a, R: Renderer + container::Renderer> {
    container: Container<'a, AnzeigeNachricht, R>,
}

impl<'a, R> Anzeige<'a, R>
where
    R: 'a + Renderer + container::Renderer + text::Renderer + button::Renderer + row::Renderer,
    <R as container::Renderer>::Style: From<style::Anzeige>,
{
    pub fn neu(status: &'a mut AnzeigeStatus) -> Self {
        let mut children = Vec::new();
        let style = if let Some((name, farbe)) = &status.aktuell {
            children.push(Text::new(&name.0).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(Text::new("<Name>").into());
            style::Anzeige::Deaktiviert
        };
        children.push(
            Button::new(&mut status.auswählen, Text::new("Auswählen"))
                .on_press(AnzeigeNachricht::Auswählen)
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
    neu_name_state: text_input::State,
    neu_anschluss_state: anschluss::Status<anschluss::Output>,
    none_button_state: button::State,
    streckenabschnitte: BTreeMap<Name, (String, Color, button::State, button::State)>,
    scrollable_state: scrollable::State,
}

impl AuswahlStatus {
    pub fn neu<'t>(
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) -> Self {
        AuswahlStatus {
            neu_name: String::new(),
            neu_name_state: text_input::State::new(),
            neu_anschluss_state: anschluss::Status::neu_output(),
            none_button_state: button::State::new(),
            streckenabschnitte: streckenabschnitte.map(Self::iter_map).collect(),
            scrollable_state: scrollable::State::new(),
        }
    }

    fn iter_map<'t>(
        (name, streckenabschnitt): (&'t Name, &'t Streckenabschnitt),
    ) -> (Name, (String, Color, button::State, button::State)) {
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
}

#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    Schließe,
    Wähle(Option<(Name, iced::Color)>),
    Hinzufügen(anschluss::OutputAnschluss),
    Lösche(Name),
    Name(String),
}

#[derive(Debug, Clone)]
pub enum AuswahlNachricht {
    Schließe,
    Wähle(Option<(Name, iced::Color)>),
    Hinzufügen(Name, anschluss::OutputAnschluss),
    Lösche(Name),
}

pub struct Auswahl<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneAuswahlNachricht, R>,
    neu_name: String,
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
    <R as container::Renderer>::Style: From<background::Grey>,
{
    pub fn neu(status: &'a mut AuswahlStatus) -> Self {
        let AuswahlStatus {
            neu_name,
            neu_name_state,
            neu_anschluss_state,
            none_button_state,
            streckenabschnitte,
            scrollable_state,
        } = status;
        let container = Container::new(
            Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
                let mut scrollable = Scrollable::new(scrollable_state)
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
                                Element::from(anschluss::Auswahl::neu_output(neu_anschluss_state))
                                    .map(InterneAuswahlNachricht::Hinzufügen),
                            ),
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
                                .on_press(InterneAuswahlNachricht::Wähle(Some((
                                    name.clone(),
                                    *farbe,
                                ))))
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
            .width(Length::Shrink),
        )
        .style(background::WHITE)
        .width(Length::Shrink)
        .height(Length::Shrink);
        Auswahl { container, neu_name: String::new() }
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> Widget<AuswahlNachricht, R> for Auswahl<'a, R> {
    reexport_no_event_methods! {Container<'a, InterneAuswahlNachricht, R>, container, InterneAuswahlNachricht, R}

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
        let mut status = self.container.on_event(
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
                InterneAuswahlNachricht::Hinzufügen(anschluss) => {
                    messages.push(AuswahlNachricht::Hinzufügen(
                        Name(self.neu_name.clone()),
                        anschluss,
                    ));
                    messages.push(AuswahlNachricht::Wähle(Some((
                        Name(self.neu_name.clone()),
                        iced::Color::WHITE, // TODO farbauswahl
                    ))))
                },
                InterneAuswahlNachricht::Lösche(name) => {
                    messages.push(AuswahlNachricht::Lösche(name))
                },
                // TODO neu_name im Status anpassen!
                // neu zeichen (view anpassen)
                // analog für NumberInput, Tabs, etc. in anschluss::Auswahl
                InterneAuswahlNachricht::Name(name) => self.neu_name = name,
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> From<Auswahl<'a, R>>
    for Element<'a, AuswahlNachricht, R>
{
    fn from(auswahl: Auswahl<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
