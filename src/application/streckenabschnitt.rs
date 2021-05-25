//! Anzeige & Erstellen eines Streckenabschnittes.

use iced::{button, Align, Button, Color, Container, Element, Row, Text};

pub use crate::steuerung::streckenabschnitt::Name;

pub mod style;

#[derive(Debug)]
pub struct Anzeige {
    pub aktuell: Option<(Name, Color)>,
    pub auswählen: button::State,
}

impl Anzeige {
    // TODO Nachrichten
    pub fn view<Msg: 'static + Clone>(&mut self, nachricht: Msg) -> Element<Msg> {
        let mut children = Vec::new();
        let style = if let Some((name, farbe)) = &self.aktuell {
            children.push(Text::new(&name.0).into());
            style::Anzeige::Farbe(*farbe)
        } else {
            children.push(Text::new("Streckenabschnitt").into());
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
