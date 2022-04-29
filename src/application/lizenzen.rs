//! Show all Licenses of dependencies.

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
};

use iced_native::{
    button::{self, Button},
    column::Column,
    container::{self, Container},
    event,
    row::{self, Row},
    rule::{self, Rule},
    scrollable::{self, Scrollable},
    text::{self, Text},
    Clipboard, Element, Event, Layout, Length, Point, Renderer, Widget,
};

use crate::application::{
    macros::reexport_no_event_methods,
    style::{
        hintergrund::{self, Hintergrund},
        linie::{Linie, TRENNLINIE},
    },
};

#[derive(Debug, Clone)]
enum InterneNachricht {
    Aktuell(&'static str, fn() -> String),
    Schließen,
}

/// Nachricht, die von einem [Lizenzen]-Widget erzeugt wird.
#[derive(Debug, Clone, Copy)]
pub enum Nachricht {
    /// Schließe die [Lizenzen]-Anzeige.
    Schließen,
}

/// Zustand eines [Lizenzen]-Widgets.
#[derive(Debug)]
pub struct Zustand {
    lizenzen_und_button_states: BTreeMap<&'static str, (button::State, fn() -> String)>,
    scrollable_buttons: scrollable::State,
    scrollable_text: scrollable::State,
    schließen: button::State,
    aktuell: Option<(&'static str, String)>,
}

impl Zustand {
    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    pub fn neu(lizenzen: impl Iterator<Item = (&'static str, fn() -> String)>) -> Self {
        let mut aktuell = None;
        let lizenzen_und_button_states = lizenzen
            .map(|(name, f)| {
                if aktuell.is_none() {
                    aktuell = Some((name, f()));
                }
                (name, (button::State::new(), f))
            })
            .collect();
        Zustand {
            lizenzen_und_button_states,
            scrollable_buttons: scrollable::State::new(),
            scrollable_text: scrollable::State::new(),
            schließen: button::State::new(),
            aktuell,
        }
    }

    /// Erstellen einen neuen [Zustand] eines [Lizenzen]-Widgets.
    #[inline(always)]
    pub fn neu_mit_verwendeten_lizenzen() -> Self {
        Self::neu(verwendete_lizenzen())
    }
}

/// Widget zur Anzeige der Lizenzen verwendeten Open-Source Bibliotheken.
pub struct Lizenzen<'a, R: Renderer + container::Renderer> {
    container: Container<'a, InterneNachricht, R>,
    aktuell: &'a mut Option<(&'static str, String)>,
}

impl<R: Renderer + container::Renderer> Debug for Lizenzen<'_, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lizenzen").field("row", &"<Row>").finish()
    }
}

impl<'a, R> Lizenzen<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + row::Renderer
        + scrollable::Renderer
        + rule::Renderer
        + button::Renderer
        + text::Renderer,
    <R as rule::Renderer>::Style: From<Linie>,
    <R as container::Renderer>::Style: From<Hintergrund>,
{
    /// Erstelle ein neues [Lizenzen]-Widget.
    pub fn neu(
        zustand: &'a mut Zustand,
        scrollable_style: impl Into<<R as scrollable::Renderer>::Style>,
    ) -> Self {
        let Zustand {
            lizenzen_und_button_states,
            scrollable_buttons,
            scrollable_text,
            schließen,
            aktuell,
        } = zustand;
        let mut buttons = Scrollable::new(scrollable_buttons)
            .width(Length::Shrink)
            .height(Length::Fill)
            .style(scrollable_style);
        let (aktuell_name, aktuell_text) =
            if let Some((name, text)) = aktuell { (Some(*name), Some(text)) } else { (None, None) };
        for (&name, (button_state, f)) in lizenzen_und_button_states {
            buttons = buttons.push({
                let button = Button::new(button_state, Text::new(name));
                if Some(name) == aktuell_name {
                    button
                } else {
                    button.on_press(InterneNachricht::Aktuell(name, *f))
                }
            });
        }
        let column = Column::new()
            .push(buttons)
            .push(
                Button::new(schließen, Text::new("Schließen"))
                    .on_press(InterneNachricht::Schließen),
            )
            .width(Length::Shrink)
            .height(Length::Fill);
        let mut scrollable_aktuell =
            Scrollable::new(scrollable_text).width(Length::Fill).height(Length::Fill);
        if let Some(aktuell_text) = aktuell_text {
            scrollable_aktuell = scrollable_aktuell
                .push(Text::new(aktuell_text.as_str()).width(Length::Fill).height(Length::Shrink))
        }
        let container = Container::new(
            Row::new()
                .push(column)
                .push(Rule::vertical(1).style(TRENNLINIE))
                .push(scrollable_aktuell),
        )
        .style(hintergrund::WEIß);
        Lizenzen { container, aktuell }
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> Widget<Nachricht, R> for Lizenzen<'a, R> {
    reexport_no_event_methods! {Container<'a, InterneNachricht, R>, container, InterneNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        nachrichten: &mut Vec<Nachricht>,
    ) -> event::Status {
        let mut interne_nachrichten = Vec::new();
        let event_status = self.container.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut interne_nachrichten,
        );
        for interne_nachricht in interne_nachrichten {
            match interne_nachricht {
                InterneNachricht::Aktuell(name, f) => *self.aktuell = Some((name, f())),
                InterneNachricht::Schließen => nachrichten.push(Nachricht::Schließen),
            }
        }
        event_status
    }
}

impl<'a, R: 'a + Renderer + container::Renderer> From<Lizenzen<'a, R>>
    for Element<'a, Nachricht, R>
{
    fn from(lizenzen: Lizenzen<'a, R>) -> Self {
        Element::new(lizenzen)
    }
}

/// Die Lizenzen der verwendeter Open-Source Bibliotheken.
pub fn verwendete_lizenzen() -> impl Iterator<Item = (&'static str, fn() -> String)> {
    // FIXME verwende echte Lizenzen
    let f: fn() -> String = || {
        String::from("Some long license text.\n\nTherefore, it needs multiple lines!\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.\n\n\n\n\n\n\n\n\n\n\nSome text in the middle.\n\n\n\n\n\n\nAnother midway text.\n\n\n\n\n\n\n\nYet another debug line.\n\n\n\nHello from the deep.\n\n\n\n\nA final last line after a lot of vertical space.")
    };
    let g: fn() -> String = || {
        String::from("Ein andere Lizenz.\nAußerdem gibt es dabei sehr lange Texte, die ausreichen sollten um neben expliziten neuen Zeilen auch automatische Zeilenumbrüche überprüfen zu können.\n\nNO WARRANTIES GIVEN, PROVIDED AS IS, ect.")
    };
    // TODO
    [("test", f), ("alternativ", g), ("noch eine", f)].into_iter()
}

// TODO Test schreiben ob, die angezeigte Lizenz mit der wirklichen übereinstimmt
