//! Einstellen der Steuerung einer [Weiche](crate::steuerung::Weiche).

use std::fmt::{self, Debug, Display, Formatter};

use iced_aw::pure::Card;
use iced_native::{
    event, overlay, text,
    widget::{
        tree::{self, Tag, Tree},
        Button, Column, Row, Text, TextInput,
    },
    Clipboard, Element, Event, Font, Layout, Length, Point, Shell, Widget,
};

use crate::{
    anschluss::OutputSerialisiert,
    application::{anschluss, macros::widget_newtype_methods},
    nachschlagen::Nachschlagen,
    steuerung::weiche::{Name, WeicheSerialisiert},
};

/// Zustand eines Widgets zur Auswahl der Anschlüsse einer [Weiche](crate::steuerung::Weiche).
#[derive(Debug, Clone)]
struct Zustand<AnschlüsseSerialisiert> {
    name: String,
    anschlüsse: AnschlüsseSerialisiert,
    hat_steuerung: bool,
}

impl<AnschlüsseSerialisiert: Default + Clone> Zustand<AnschlüsseSerialisiert> {
    /// Erstelle einen neuen [Zustand], potentiell mit voreingestellten Anschlüssen.
    fn neu<Richtung>(
        option_weiche: &Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        let (name, anschlüsse, hat_steuerung) =
            if let Some(WeicheSerialisiert { name, anschlüsse, .. }) = option_weiche {
                (name.0.clone(), anschlüsse.clone(), true)
            } else {
                (String::new(), AnschlüsseSerialisiert::default(), false)
            };
        Zustand { name, anschlüsse, hat_steuerung }
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
    element: Element<'t, InterneNachricht<Richtung>, R>,
    weiche: &'t Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
}

impl<Richtung, AnschlüsseSerialisiert, R> Debug for Auswahl<'_, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: Debug,
    AnschlüsseSerialisiert: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("element", &"<Element>")
            .field("weiche", &self.weiche)
            .finish()
    }
}

impl<'t, Richtung, AnschlüsseSerialisiert, R> Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>
where
    AnschlüsseSerialisiert: Default + Clone + Nachschlagen<Richtung, OutputSerialisiert>,
    Richtung: 'static + Clone + Display,
    R: 't + text::Renderer<Font = Font>,
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<AnschlüsseAuswahlZustand>(
        weiche: &'t Option<WeicheSerialisiert<Richtung, AnschlüsseSerialisiert>>,
    ) -> Self {
        Auswahl { element: Self::erzeuge_element(&Zustand::neu(weiche)), weiche }
    }

    fn erzeuge_element(
        zustand: &'t Zustand<AnschlüsseSerialisiert>,
    ) -> Element<'t, InterneNachricht<Richtung>, R> {
        let Zustand { name, anschlüsse, hat_steuerung } = zustand;
        let mut column = Column::new()
            .push(TextInput::new("<Name>", name, InterneNachricht::Name).width(Length::Units(200)));
        for (richtung, anschluss) in anschlüsse.referenzen() {
            column = column.push(Row::new().push(Text::new(richtung.to_string())).push(
                Element::from(anschluss::Auswahl::neu_output_s(Some(anschluss))).map(
                    move |anschluss_serialisiert| {
                        InterneNachricht::Anschluss(richtung.clone(), anschluss_serialisiert)
                    },
                ),
            ))
        }
        column = column.push(
            Row::new()
                .push(Button::new(Text::new("Festlegen")).on_press(InterneNachricht::Festlegen))
                .push(
                    Button::new(Text::new(if *hat_steuerung {
                        "Entfernen"
                    } else {
                        "Keine Anschlüsse"
                    }))
                    .on_press(InterneNachricht::Entfernen),
                ),
        );
        let card = Card::new(Text::new("Weiche"), column)
            .on_close(InterneNachricht::Schließen)
            .width(Length::Shrink)
            .height(Length::Shrink);
        card.into()
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

impl<Richtung, Richtung2, AnschlüsseSerialisiert, R>
    Widget<Nachricht<Richtung2, AnschlüsseSerialisiert>, R>
    for Auswahl<'_, Richtung, AnschlüsseSerialisiert, R>
where
    Richtung: Clone + Display,
    Richtung2: Default,
    AnschlüsseSerialisiert: Clone + Default + Nachschlagen<Richtung, OutputSerialisiert>,
    R: text::Renderer<Font = Font>,
{
    widget_newtype_methods! {element, R}

    fn tag(&self) -> Tag {
        Tag::of::<Zustand<AnschlüsseSerialisiert>>()
    }

    fn state(&self) -> tree::State {
        tree::State::new(Zustand::neu(&self.weiche))
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Nachricht<Richtung2, AnschlüsseSerialisiert>>,
    ) -> event::Status {
        let mut card_messages = Vec::new();
        let mut card_shell = Shell::new(&mut card_messages);
        let mut status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut card_shell,
        );
        if card_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else if card_shell.is_layout_invalid() {
            shell.invalidate_layout()
        }
        let zustand: &mut Zustand<AnschlüsseSerialisiert> = state.state.downcast_mut();
        let mut zustand_geändert = false;
        for message in card_messages {
            status = event::Status::Captured;
            match message {
                InterneNachricht::Name(name) => {
                    zustand_geändert = true;
                    zustand.name = name;
                },
                InterneNachricht::Anschluss(richtung, anschluss) => {
                    zustand_geändert = true;
                    *zustand.anschlüsse.erhalte_mut(&richtung) = anschluss;
                },
                InterneNachricht::Festlegen => {
                    let nachricht = Nachricht::Festlegen(Some(WeicheSerialisiert::neu(
                        Name(zustand.name.clone()),
                        Richtung2::default(),
                        zustand.anschlüsse.clone(),
                    )));
                    shell.publish(nachricht)
                },
                InterneNachricht::Entfernen => shell.publish(Nachricht::Festlegen(None)),
                InterneNachricht::Schließen => shell.publish(Nachricht::Schließen),
            }
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
    ) -> Option<overlay::Element<'a, Nachricht<Richtung2, AnschlüsseSerialisiert>, R>> {
        // TODO
        None
    }
}

impl<'t, Richtung, Richtung2, AnschlüsseSerialisiert, R>
    From<Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>>
    for Element<'t, Nachricht<Richtung2, AnschlüsseSerialisiert>, R>
where
    Richtung: 't + Clone + Display,
    Richtung2: Default,
    AnschlüsseSerialisiert: 't + Clone + Default + Nachschlagen<Richtung, OutputSerialisiert>,
    R: 't + text::Renderer<Font = Font>,
{
    fn from(anzeige: Auswahl<'t, Richtung, AnschlüsseSerialisiert, R>) -> Self {
        Element::new(anzeige)
    }
}
