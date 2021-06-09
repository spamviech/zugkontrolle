//! Anzeige & Erstellen einer Geschwindigkeit.

// TODO for now
#![allow(unused_imports)]

use std::iter;

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
    Radio,
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
    aktuelle_geschwindigkeit: u8,
    pwm_slider_state: slider::State,
    fahrtrichtung_state: Umdrehen,
}

impl Mittelleiter {
    pub fn anzeige_status_neu() -> AnzeigeStatus<(button::State, ())> {
        AnzeigeStatus {
            aktuelle_geschwindigkeit: 0,
            pwm_slider_state: slider::State::new(),
            fahrtrichtung_state: (button::State::new(), ()),
        }
    }
}

impl Zweileiter {
    pub fn anzeige_status_neu() -> AnzeigeStatus<Fahrtrichtung> {
        AnzeigeStatus {
            aktuelle_geschwindigkeit: 0,
            pwm_slider_state: slider::State::new(),
            fahrtrichtung_state: Fahrtrichtung::Vorwärts,
        }
    }
}

pub struct Anzeige<'t, Leiter, FahrtrichtungMarker, M, R> {
    geschwindigkeit: &'t Geschwindigkeit<Leiter>,
    aktuelle_geschwindigkeit: &'t mut u8,
    aktuelle_fahrtrichtung: &'t mut FahrtrichtungMarker,
    row: Row<'t, M, R>,
}

#[derive(Debug, Clone)]
pub enum MessageMittelleiter {
    Geschwindigkeit(u8),
    Umdrehen,
}
impl<'t, R> Anzeige<'t, Mittelleiter, (), MessageMittelleiter, R>
where
    R: 't
        + row::Renderer
        + column::Renderer
        + text::Renderer
        + button::Renderer
        + slider::Renderer
        + radio::Renderer,
{
    pub fn neu(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Mittelleiter>,
        status: &'t mut AnzeigeStatus<(button::State, ())>,
    ) -> Self {
        let AnzeigeStatus {
            aktuelle_geschwindigkeit,
            pwm_slider_state,
            fahrtrichtung_state: (umdrehen_state, unit),
        } = status;
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut row = match &geschwindigkeit.leiter {
            Mittelleiter::Pwm { pin: _, polarität: _ } => {
                Row::new().push(Text::new(&name.0)).push(Slider::new(
                    pwm_slider_state,
                    0 ..= u8::MAX,
                    *aktuelle_geschwindigkeit,
                    MessageMittelleiter::Geschwindigkeit,
                ))
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen: _ } => {
                Row::new().push(Column::with_children(
                    iter::once(())
                        .chain(geschwindigkeit.iter().map(|_| ()))
                        .enumerate()
                        .map(|(i, ())| {
                            let i_u8 = i as u8;
                            Radio::new(
                                i_u8,
                                i_u8.to_string(),
                                Some(*aktuelle_geschwindigkeit),
                                MessageMittelleiter::Geschwindigkeit,
                            )
                            .into()
                        })
                        .collect(),
                ))
            },
        };
        row = row.push(
            Button::new(umdrehen_state, Text::new("Umdrehen"))
                .on_press(MessageMittelleiter::Umdrehen),
        );
        Anzeige { geschwindigkeit, aktuelle_geschwindigkeit, aktuelle_fahrtrichtung: unit, row }
    }
}

#[derive(Debug, Clone)]
pub enum MessageZweileiter {
    Geschwindigkeit(u8),
    Fahrtrichtung(Fahrtrichtung),
}
impl<'t, R> Anzeige<'t, Zweileiter, Fahrtrichtung, MessageZweileiter, R>
where
    R: 't + column::Renderer + row::Renderer + text::Renderer + slider::Renderer + radio::Renderer,
{
    pub fn neu(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Zweileiter>,
        status: &'t mut AnzeigeStatus<Fahrtrichtung>,
    ) -> Self {
        let AnzeigeStatus { aktuelle_geschwindigkeit, pwm_slider_state, fahrtrichtung_state } =
            status;
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut row = Row::new().push(Text::new(&name.0));
        row = match &geschwindigkeit.leiter {
            Zweileiter::Pwm { geschwindigkeit: _, polarität: _, fahrtrichtung: _ } => {
                row.push(Slider::new(
                    pwm_slider_state,
                    0 ..= u8::MAX,
                    *aktuelle_geschwindigkeit,
                    MessageZweileiter::Geschwindigkeit,
                ))
            },
            Zweileiter::KonstanteSpannung {
                geschwindigkeit,
                letzter_wert: _,
                fahrtrichtung: _,
            } => row.push(Column::with_children(
                iter::once(())
                    .chain(geschwindigkeit.iter().map(|_| ()))
                    .enumerate()
                    .map(|(i, ())| {
                        let i_u8 = i as u8;
                        Radio::new(
                            i_u8,
                            i_u8.to_string(),
                            Some(*aktuelle_geschwindigkeit),
                            MessageZweileiter::Geschwindigkeit,
                        )
                        .into()
                    })
                    .collect(),
            )),
        };
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung| {
            Radio::new(
                fahrtrichtung,
                fahrtrichtung.to_string(),
                Some(*fahrtrichtung_state),
                MessageZweileiter::Fahrtrichtung,
            )
        };
        row = row
            .push(fahrtrichtung_radio(Fahrtrichtung::Vorwärts))
            .push(fahrtrichtung_radio(Fahrtrichtung::Rückwärts));
        Anzeige {
            geschwindigkeit,
            aktuelle_geschwindigkeit,
            aktuelle_fahrtrichtung: fahrtrichtung_state,
            row,
        }
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
