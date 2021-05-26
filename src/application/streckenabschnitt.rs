//! Anzeige & Erstellen eines Streckenabschnittes.

use std::collections::BTreeMap;

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
    streckenabschnitte: BTreeMap<Name, (String, Color, button::State, button::State)>,
    scrollable_state: scrollable::State,
}

impl Status {
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
    pub(super) fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(Status::iter_map));
    }

    /// Entferne den Streckenabschnitt mit übergebenen Namen.
    pub(super) fn entferne(&mut self, name: &Name) {
        self.streckenabschnitte.remove(name);
    }

    /// Füge einen neuen Streckenabschnitt hinzu.
    /// Falls der Name bereits existiert wird der bisherige ersetzt.
    #[allow(dead_code)]
    pub(super) fn hinzufügen(&mut self, name: &Name, streckenabschnitt: &Streckenabschnitt) {
        let (key, value) = Status::iter_map((name, streckenabschnitt));
        self.streckenabschnitte.insert(key, value);
    }
}

impl Auswahl {
    pub fn neu<'t>(
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) -> Self {
        Auswahl(modal::State::new(Status {
            none_button_state: button::State::new(),
            streckenabschnitte: streckenabschnitte.map(Status::iter_map).collect(),
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
                        for (name, (anschluss, farbe, button_state, delete_state)) in
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
