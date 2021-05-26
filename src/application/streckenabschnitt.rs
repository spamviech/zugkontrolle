//! Anzeige & Erstellen eines Streckenabschnittes.

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
    streckenabschnitte: Vec<(Name, String, Color, button::State, button::State)>,
    scrollable_state: scrollable::State,
}
impl Status {
    /// Ersetze die angezeigten Streckenabschnitte mit dem Argument.
    pub(super) fn update<'t>(
        &mut self,
        streckenabschnitte: impl Iterator<Item = (&'t Name, &'t Streckenabschnitt)>,
    ) {
        self.streckenabschnitte.clear();
        self.streckenabschnitte.extend(streckenabschnitte.map(|(name, streckenabschnitt)| {
            (
                name.clone(),
                // TODO bessere Anschluss-Anzeige (impl Display?)
                format!("{:?}", streckenabschnitt.anschluss),
                streckenabschnitt.farbe.clone(),
                button::State::new(),
                button::State::new(),
            )
        }));
        self.streckenabschnitte.sort_by_cached_key(|(name, _, _, _, _)| name.0.clone())
    }
}

impl Auswahl {
    pub fn neu() -> Self {
        Auswahl(modal::State::new(Status {
            streckenabschnitte: Vec::new(),
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
        Modal::new(&mut self.0, underlay, |Status { streckenabschnitte, scrollable_state }| {
            Container::new(
                Card::new(Text::new("Streckenabschnitt").width(Length::Fill), {
                    let mut scrollable = Scrollable::new(scrollable_state).width(Length::Shrink);
                    for (name, anschluss, farbe, button_state, delete_state) in streckenabschnitte {
                        scrollable = scrollable.push(
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
                                .push(
                                    Button::new(delete_state, Text::new("X"))
                                        .on_press(Message::LöscheStreckenabschnitt(name.clone())),
                                ),
                        );
                    }
                    scrollable
                })
                .on_close(Message::AuswahlStreckenabschnitt)
                .width(Length::Shrink),
            )
            .style(background::WHITE)
            .width(Length::Shrink)
            .height(Length::Shrink)
            .into()
        })
        .on_esc(Message::AuswahlStreckenabschnitt)
        .into()
    }
}
