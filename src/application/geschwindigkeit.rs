//! Anzeige & Erstellen einer Geschwindigkeit.

// TODO for now
#![allow(unused_imports)]

use iced_aw::native::{card, number_input, tab_bar, tabs, Card};
use iced_native::{
    button,
    checkbox,
    column,
    container,
    event,
    mouse,
    radio,
    row,
    scrollable,
    slider,
    text,
    text_input,
    Align,
    Button,
    Checkbox,
    Clipboard,
    Column,
    Container,
    Element,
    Event,
    Layout,
    Length,
    Point,
    Renderer,
    Row,
    Scrollable,
    Slider,
    Text,
    TextInput,
    Widget,
};

use super::{anschluss, macros::reexport_no_event_methods};
use crate::anschluss::polarity::Polarität;
use crate::farbe::Farbe;
use crate::steuerung::geschwindigkeit::{Fahrtrichtung, Mittelleiter, Zweileiter};
pub use crate::steuerung::geschwindigkeit::{Geschwindigkeit, Map, Name};

// TODO Map-Typ-alias mit AnzeigeStatus?

#[derive(Debug)]
pub struct AnzeigeStatus<Umdrehen> {
    // aktueller Wert konstante Spannung in Geschwindigkeit gespeichert
    pwm_slider: slider::State,
    fahrtrichtung: Umdrehen,
}

impl Mittelleiter {
    pub fn anzeige_status_neu() -> AnzeigeStatus<button::State> {
        AnzeigeStatus { pwm_slider: slider::State::new(), fahrtrichtung: button::State::new() }
    }
}

impl Zweileiter {
    pub fn anzeige_status_neu() -> AnzeigeStatus<Fahrtrichtung> {
        AnzeigeStatus { pwm_slider: slider::State::new(), fahrtrichtung: Fahrtrichtung::Vorwärts }
    }
}

pub struct Anzeige<'t, Leiter, Umdrehen, M, R> {
    geschwindigkeit: &'t Geschwindigkeit<Leiter>,
    status: &'t mut AnzeigeStatus<Umdrehen>,
    row: Row<'t, M, R>,
}

pub enum MessageMittelleiter {
    Geschwindigkeit(u8),
    Umdrehen,
}
impl<'t, R> Anzeige<'t, Mittelleiter, button::State, MessageMittelleiter, R> {
    pub fn neu() -> Self {
        todo!()
    }
}
pub enum MessageZweileiter {
    Geschwindigkeit(u8),
    Fahrtrichtung(Fahrtrichtung),
}
impl<'t, R> Anzeige<'t, Zweileiter, Fahrtrichtung, MessageZweileiter, R> {
    pub fn neu() -> Self {
        todo!()
    }
}

impl<'t, Leiter, Umdrehen, M, R> Widget<M, R> for Anzeige<'t, Leiter, Umdrehen, M, R>
where
    R: Renderer + row::Renderer,
{
    reexport_no_event_methods! {Row<'t, M, R>, row, M, R}

    fn on_event(
        &mut self,
        _event: Event,
        _layout: Layout<'_>,
        _cursor_position: Point,
        _renderer: &R,
        _clipboard: &mut dyn Clipboard,
        _messages: &mut Vec<M>,
    ) -> event::Status {
        todo!()
    }
}
