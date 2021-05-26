//! Anzeige & Erstellen eines Streckenabschnittes.

use std::iter::FromIterator;

use iced::{
    button,
    scrollable,
    Align,
    Button,
    Color,
    Container,
    Element,
    Length,
    Row,
    Scrollable,
    Text,
};
use iced_aw::Card;
use iced_aw::{modal, Modal};

use super::style::background;
use super::Message;
pub use crate::steuerung::streckenabschnitt::Name;
use crate::steuerung::Streckenabschnitt;

pub mod style;

#[derive(Debug)]
pub struct Anzeige {
    pub aktuell: Option<(Name, Color)>,
    pub auswählen: button::State,
}

impl Anzeige {
    pub fn view<Msg: 'static + Clone>(&mut self, nachricht: Msg) -> Element<Msg> {
        let mut children = Vec::new();
        let style = if let Some((name, farbe)) = &self.aktuell {
            children.push(Text::new(&name.0).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(Text::new("<Name>").into());
            style::Anzeige::Deaktiviert
        };
        children.push(
            Button::new(&mut self.auswählen, Text::new("Auswählen")).on_press(nachricht).into(),
        );
        Container::new(Row::with_children(children).spacing(1).align_items(Align::Center))
            .style(style)
            .into()
    }
}

#[derive(Debug)]
pub struct Auswahl(pub modal::State<Status>);
#[derive(Debug)]
pub struct Status {
    none_button_state: button::State,
    streckenabschnitte: Vec<(Name, String, Color, button::State, button::State)>,
    scrollable_state: scrollable::State,
}

fn get_name((name, _, _, _, _): &(Name, String, Color, button::State, button::State)) -> String {
    name.0.clone()
}

impl Status {
    fn iter_map<'t>(
        (name, streckenabschnitt): (&'t Name, &'t Streckenabschnitt),
    ) -> (Name, String, Color, button::State, button::State) {
        (
            name.clone(),
            format!("{}", streckenabschnitt.anschluss),
            streckenabschnitt.farbe.clone(),
            button::State::new(),
            button::State::new(),
        )
    }

    /// Ersetze die angezeigten Streckenabschnitte mit dem Argument.
    pub(super) fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(Status::iter_map));
        self.streckenabschnitte.sort_by_cached_key(get_name)
    }

    /// Entferne den Streckenabschnitt mit übergebenen Namen.
    pub(super) fn entferne(&mut self, name: &Name) {
        if let Ok(index) = self.streckenabschnitte.binary_search_by_key(&name.0, get_name) {
            self.streckenabschnitte.remove(index);
        }
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    #[allow(dead_code)]
    pub(super) fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let value = Status::iter_map((name, streckenabschnitt));
        match self.streckenabschnitte.binary_search_by_key(&name.0, get_name) {
            Ok(index) => self.streckenabschnitte[index] = value,
            Err(index) => self.streckenabschnitte.insert(index, value),
        }
    }
}

impl Auswahl {
    pub fn neu<'t>(
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) -> Self {
        let mut streckenabschnitte = Vec::from_iter(streckenabschnitte.map(Status::iter_map));
        streckenabschnitte.sort_by_cached_key(get_name);
        Auswahl(modal::State::new(Status {
            none_button_state: button::State::new(),
            streckenabschnitte,
            scrollable_state: scrollable::State::new(),
        }))
    }

    // TODO
    // Erste Zeile Leer(None Auswahl), Neu(Streckenabschnitt erstellen)
    // Über Scrollable?
    pub fn view<'t, Z: 'static, U: Into<Element<'t, Message<Z>>>>(
        &'t mut self,
        underlay: U,
    ) -> Element<Message<Z>> {
        Modal::new(
            &mut self.0,
            underlay,
            |Status { none_button_state, streckenabschnitte, scrollable_state }| {
                Container::new(
                    Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
                        let mut scrollable = Scrollable::new(scrollable_state)
                            .push(
                                Button::new(none_button_state, Text::new("Keinen"))
                                    .on_press(Message::WähleStreckenabschnitt(None)),
                            )
                            .width(Length::Shrink);
                        for (name, anschluss, farbe, button_state, delete_state) in
                            streckenabschnitte
                        {
                            scrollable =
                                scrollable.push(
                                    Row::new()
                                        .push(
                                            Button::new(
                                                button_state,
                                                Text::new(&format!("{}: {:?}", name.0, anschluss)),
                                            )
                                            .on_press(Message::WähleStreckenabschnitt(Some((
                                                name.clone(),
                                                *farbe,
                                            ))))
                                            .style(style::Auswahl(*farbe)),
                                        )
                                        .push(Button::new(delete_state, Text::new("X")).on_press(
                                            Message::LöscheStreckenabschnitt(name.clone()),
                                        )),
                                );
                        }
                        scrollable
                    })
                    .on_close(Message::SchließeAuswahlStreckenabschnitt)
                    .width(Length::Shrink),
                )
                .style(background::WHITE)
                .width(Length::Shrink)
                .height(Length::Shrink)
                .into()
            },
        )
        .on_esc(Message::SchließeAuswahlStreckenabschnitt)
        .into()
    }
}
