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
pub use crate::steuerung::geschwindigkeit::{Geschwindigkeit, Map, Name};
use crate::steuerung::geschwindigkeit::{Mittelleiter, Zweileiter};

// TODO Map-Typ-alias mit AnzeigeStatus?

#[derive(Debug)]
pub struct AnzeigeStatusM {
    geschwindigkeit: Geschwindigkeit<Mittelleiter>,
    pwm_slider: slider::State,
    // Konstante Spannung über Radio
    umdrehen: button::State,
}
#[derive(Debug)]
pub struct AnzeigeStatusZ {
    geschwindigkeit: Geschwindigkeit<Zweileiter>,
    pwm_slider: slider::State,
    /* Konstante Spannung über Radio
     * Fahrtrichtung über Radio */
}
