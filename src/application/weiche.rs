//! Einstellen der Steuerung einer [Weiche](crate::steuerung::Weiche).

use std::fmt::{Debug, Display};

use iced_aw::native::Card;
use iced_native::{
    event, text,
    widget::{
        button::{self, Button},
        text_input::{self, TextInput},
        Column, Row, Text,
    },
    Clipboard, Element, Event, Font, Layout, Length, Point, Shell, Widget,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{anschluss, macros::reexport_no_event_methods},
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
pub struct Auswahl<'t, Richtung, AnschlüsseSerialisiert, R> {
    card: Card<'t, InterneNachricht<Richtung>, R>,
    name: &'t mut String,
    anschlüsse: &'t mut AnschlüsseSerialisiert,
}

impl<Richtung, AnschlüsseSerialisiert, R> Debug for Auswahl<'_, Richtung, AnschlüsseSerialisiert, R>
where
    AnschlüsseSerialisiert: Debug,
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
    R: 't + text::Renderer<Font = Font>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<AnschlüsseAuswahlZustand>(
        zustand: &'t mut Zustand<AnschlüsseSerialisiert, AnschlüsseAuswahlZustand>,
    ) -> Self
    where
        AnschlüsseAuswahlZustand: Nachschlagen<Richtung, anschluss::Zustand<anschluss::Output>>,
    {
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
            column = column.push(Row::new().push(Text::new(richtung.to_string())).push(
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
    R: text::Renderer<Font = Font>,
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
        shell: &mut Shell<'_, Nachricht<Richtung, AnschlüsseSerialisiert>>,
    ) -> event::Status {
        let mut card_messages = Vec::new();
        let mut card_shell = Shell::new(&mut card_messages);
        let mut status = self.card.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut card_shell,
        );
        if card_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            card_shell.revalidate_layout(|| shell.invalidate_layout())
        }
        for message in card_messages {
            status = event::Status::Captured;
            match message {
                InterneNachricht::Name(name) => *self.name = name,
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    *self.anschlüsse.erhalte_mut(&richtung) = anschluss
                },
                InterneNachricht::Festlegen => {
                    let weiche_steuerung = WeicheSteuerung {
                        aktuelle_richtung: Richtung::default(),
                        letzte_richtung: Richtung::default(),
                        anschlüsse: self.anschlüsse.clone(),
                    };
                    let nachricht = Nachricht::Festlegen(Some(WeicheSerialisiert::neu(
                        Name(self.name.clone()),
                        weiche_steuerung,
                    )));
                    shell.publish(nachricht)
                },
                InterneNachricht::Entfernen => shell.publish(Nachricht::Festlegen(None)),
                InterneNachricht::Schließen => shell.publish(Nachricht::Schließen),
            }
        }
        status
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> From<Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<Richtung, AnschlüsseSerialisiert>, R>
where
    Richtung: 't + Clone + Default,
    AnschlüsseSerialisiert: 't + Clone + Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't + text::Renderer<Font = Font>,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>) -> Self {
        Element::new(anzeige)
    }
}
