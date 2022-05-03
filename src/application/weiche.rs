//! Einstellen der Steuerung einer [Weiche](crate::steuerung::Weiche).

use std::fmt::{Debug, Display};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    event,
    text::{self, Text},
    widget::{
        button::{self, Button},
        column::{self, Column},
        container, radio,
        row::{self, Row},
        text_input::{self, TextInput},
    },
    Clipboard, Element, Event, Layout, Length, Point, Renderer, Widget,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar},
    nachschlagen::Nachschlagen,
    steuerung::weiche::{Name, WeicheSerialisiert, WeicheSteuerung},
};

/// Zustand eines Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
#[derive(Debug, Clone)]
pub struct Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand> {
    name: String,
    name_zustand: text_input::State,
    anschlüsse_save: AnschlüsseSerialisiert,
    anschlüsse_zustand: AnschlüsseAuswahlZustand,
    festlegen_zustand: button::State,
    entfernen_zustand: button::State,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>
    Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>
where
    AnschlüsseSerialisiert: Default + Clone + Into<AnschlüsseAuswahlZustand>,
{
    /// Erstelle einen neuen [Zustand], potentiell mit voreingestellten Anschlüssen.
    pub fn neu<Richtung>(
        option_weiche: Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        let (name, anschlüsse_save, hat_steuerung) =
            if let Some((name, WeicheSteuerung { anschlüsse, .. })) =
                option_weiche.map(WeicheSerialisiert::name_und_steuerung)
            {
                (name.0, anschlüsse, true)
            } else {
                (String::new(), Default::default(), false)
            };
        let anschlüsse_zustand = anschlüsse_save.clone().into();
        Zustand {
            name,
            name_zustand: text_input::State::new(),
            anschlüsse_save,
            anschlüsse_zustand,
            festlegen_zustand: button::State::new(),
            entfernen_zustand: button::State::new(),
            hat_steuerung,
        }
    }
}

#[derive(Debug, Clone)]
enum InterneNachricht<Richtung> {
    Name(String),
    Anschluss(Richtung, OutputSerialisiert),
    Festlegen,
    Entfernen,
    Schließen,
}

/// Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
pub struct Auswahl<'t, Richtung, AnschlüsseSerialisiert, R: card::Renderer> {
    card: Card<'t, InterneNachricht<Richtung>, R>,
    name: &'t mut String,
    anschlüsse: &'t mut AnschlüsseSerialisiert,
}

impl<Richtung, AnschlüsseSerialisiert, R> Debug for Auswahl<'_, Richtung, AnschlüsseSerialisiert, R>
where
    AnschlüsseSerialisiert: Debug,
    R: card::Renderer,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Auswahl")
            .field("card", &"<Card>")
            .field("name", &self.name)
            .field("anschlüsse", &self.anschlüsse)
            .finish()
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: 'static + Clone + Display,
    AnschlüsseSerialisiert: Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't
        + Renderer
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + text::Renderer
        + button::Renderer
        + text_input::Renderer
        + radio::Renderer
        + card::Renderer
        + tabs::Renderer
        + number_input::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<
        AnschlüsseAuswahlZustand: Nachschlagen<Richtung, anschluss::Zustand<anschluss::Output>>,
    >(
        zustand: &'t mut Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>,
    ) -> Self {
        let Zustand {
            name,
            name_zustand,
            anschlüsse_save,
            anschlüsse_zustand,
            festlegen_zustand,
            entfernen_zustand,
            hat_steuerung,
        } = zustand;
        let mut column = Column::new().push(
            TextInput::new(name_zustand, "<Name>", name, InterneNachricht::Name)
                .width(Length::Units(200)),
        );
        for (richtung, anschluss_zustand) in anschlüsse_zustand.referenzen_mut().into_iter() {
            column = column.push(Row::new().push(Text::new(format!("{}", richtung))).push(
                Element::from(anschluss::Auswahl::neu_output(anschluss_zustand)).map(
                    move |anschluss_save| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_save)
                    },
                ),
            ))
        }
        column = column.push(
            Row::new()
                .push(
                    Button::new(festlegen_zustand, Text::new("Festlegen"))
                        .on_press(InterneNachricht::Festlegen),
                )
                .push(
                    Button::new(
                        entfernen_zustand,
                        Text::new(if *hat_steuerung { "Entfernen" } else { "Keine Anschlüsse" }),
                    )
                    .on_press(InterneNachricht::Entfernen),
                ),
        );
        let card = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        Auswahl { card, name, anschlüsse: anschlüsse_save }
    }
}

/// Nachricht einer [Auswahl].
#[derive(Debug, Clone)]
pub enum Nachricht<Richtung, AnschlüsseSerialisiert> {
    /// Steuerung einer Weiche anpassen.
    Festlegen(Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>),
    /// Schließe das Widget, ohne eine Änderung vorzunehmen.
    Schließen,
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> Widget<Nachricht<Richtung, AnschlüsseSerialisiert>, R>
    for Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: Clone + Default,
    AnschlüsseSerialisiert: Clone + Nachschlagen<Richtung, OutputSerialisiert>,
    R: Renderer + card::Renderer,
{
    reexport_no_event_methods! {
        Card<'t, InterneNachricht<Richtung>, R>,
        card,
        InterneNachricht<Richtung>,
        R
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Nachricht<Richtung, AnschlüsseSerialisiert>>,
    ) -> event::Status {
        let mut card_messages = Vec::new();
        let mut status = self.card.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut card_messages,
        );
        for message in card_messages {
            status = event::Status::Captured;
            match message {
                InterneNachricht::Name(name) => *self.name = name,
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    *self.anschlüsse.erhalte_mut(&richtung) = anschluss
                },
                InterneNachricht::Festlegen => {
                    messages.push(Nachricht::Festlegen(Some(WeicheSerialisiert::neu(
                        Name(self.name.clone()),
                        WeicheSteuerung {
                            aktuelle_richtung: Default::default(),
                            letzte_richtung: Default::default(),
                            anschlüsse: self.anschlüsse.clone(),
                        },
                    ))))
                },
                InterneNachricht::Entfernen => messages.push(Nachricht::Festlegen(None)),
                InterneNachricht::Schließen => messages.push(Nachricht::Schließen),
            }
        }
        status
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> From<Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<Richtung, AnschlüsseSerialisiert>, R>
where
    Richtung: 't + Clone + Default,
    AnschlüsseSerialisiert: Clone + Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>) -> Self {
        Element::new(anzeige)
    }
}
