//! Anzeige & Erstellen eines Streckenabschnittes.

use std::collections::BTreeMap;

use iced_aw::native::{card, Card};
use iced_native::{
    button,
    container,
    event,
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
use num_x::u3;

use super::{anschluss, macros::reexport_no_event_methods, style::background};
use crate::anschluss::{level::Level, pcf8574::Variante, polarity::Polarity};
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
    // TODO remove pragma
    #[allow(dead_code)]
    pub(super) fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(Self::iter_map));
    }

    /// Entferne den Streckenabschnitt mit übergebenen Namen.
    pub(super) fn entferne(&mut self, name: &Name) {
        self.streckenabschnitte.remove(name);
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    // TODO remove pragma
    #[allow(dead_code)]
    pub(super) fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let (key, value) = Self::iter_map((name, streckenabschnitt));
        self.streckenabschnitte.insert(key, value);
    }
}

#[derive(Debug, Clone)]
pub enum AuswahlNachricht {
    SchließeAuswahlStreckenabschnitt,
    WähleStreckenabschnitt(Option<(Name, iced::Color)>),
    HinzufügenStreckenabschnitt {
        name: Name,
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        port: u3,
        polarität: Polarity,
    },
    LöscheStreckenabschnitt(Name),
}

pub struct Auswahl<'a, R: Renderer + container::Renderer> {
    container: Container<'a, AuswahlNachricht, R>,
}

impl<'a, R> Auswahl<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + text::Renderer
        + button::Renderer
        + scrollable::Renderer
        + row::Renderer
        + card::Renderer,
    <R as button::Renderer>::Style: From<style::Auswahl>,
    <R as container::Renderer>::Style: From<background::Grey>,
{
    // TODO
    // Erste Zeile Leer(None Auswahl), Neu(Streckenabschnitt erstellen)
    // Über Scrollable?
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
                    // TODO Message muss Clone sein, Anschluss ist ein Singleton und kann nicht Clone sein!
                    // .push(
                    //     Row::new()
                    //         .push(TextInput::new(neu_name_state, "<Name>", neu_name, todo!()))
                    //         .push(anschluss::Auswahl::neu_output(neu_anschluss_state)),
                    // )
                    .push(
                        Button::new(none_button_state, Text::new("Keinen"))
                            .on_press(AuswahlNachricht::WähleStreckenabschnitt(None)),
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
                                .on_press(AuswahlNachricht::WähleStreckenabschnitt(Some((
                                    name.clone(),
                                    *farbe,
                                ))))
                                .style(style::Auswahl(*farbe)),
                            )
                            .push(Button::new(delete_state, Text::new("X")).on_press(
                                AuswahlNachricht::LöscheStreckenabschnitt(name.clone()),
                            )),
                    );
                }
                scrollable
            })
            .on_close(AuswahlNachricht::SchließeAuswahlStreckenabschnitt)
            .width(Length::Shrink),
        )
        .style(background::WHITE)
        .width(Length::Shrink)
        .height(Length::Shrink);
        Auswahl { container }
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> Widget<AuswahlNachricht, R> for Auswahl<'a, R> {
    reexport_no_event_methods! {Container<'a, AuswahlNachricht, R>, container, AuswahlNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<AuswahlNachricht>,
    ) -> event::Status {
        self.container.on_event(event, layout, cursor_position, renderer, clipboard, messages)
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> From<Auswahl<'a, R>>
    for Element<'a, AuswahlNachricht, R>
{
    fn from(auswahl: Auswahl<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
