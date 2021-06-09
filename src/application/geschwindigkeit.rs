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
    overlay,
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
impl<'t, Leiter, FahrtrichtungMarker, M, R> Anzeige<'t, Leiter, FahrtrichtungMarker, M, R>
where
    M: 'static + Clone,
    R: 't + column::Renderer + row::Renderer + text::Renderer + slider::Renderer + radio::Renderer,
{
    pub fn neu_mit_leiter<S, Iter: Iterator>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Leiter>,
        status: &'t mut AnzeigeStatus<S>,
        ks_iter: impl FnOnce(&'t Geschwindigkeit<Leiter>) -> Option<Iter>,
        geschwindigkeits_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(&'t mut S) -> (Element<'t, M, R>, &'t mut FahrtrichtungMarker),
        // TODO overlay mit Anschlüssen?
    ) -> Self {
        let AnzeigeStatus { aktuelle_geschwindigkeit, pwm_slider_state, fahrtrichtung_state } =
            status;
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut row = Row::new().push(Text::new(&name.0));
        row = if let Some(iter) = ks_iter(geschwindigkeit) {
            row.push(Column::with_children(
                iter::once(())
                    .chain(iter.map(|_| ()))
                    .enumerate()
                    .map(|(i, ())| {
                        let i_u8 = i as u8;
                        Radio::new(
                            i_u8,
                            i_u8.to_string(),
                            Some(*aktuelle_geschwindigkeit),
                            geschwindigkeits_nachricht.clone(),
                        )
                        .into()
                    })
                    .collect(),
            ))
        } else {
            row.push(Slider::new(
                pwm_slider_state,
                0 ..= u8::MAX,
                *aktuelle_geschwindigkeit,
                geschwindigkeits_nachricht,
            ))
        };
        let (fahrtrichtung_steuerung, fahrtrichtung_status) =
            zeige_fahrtrichtung(fahrtrichtung_state);
        row = row.push(fahrtrichtung_steuerung);
        Anzeige {
            geschwindigkeit,
            aktuelle_geschwindigkeit,
            aktuelle_fahrtrichtung: fahrtrichtung_status,
            row,
        }
    }
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
        let ks_iter = |Geschwindigkeit { leiter }: &'t Geschwindigkeit<Mittelleiter>| match leiter {
            Mittelleiter::Pwm { .. } => None,
            Mittelleiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.iter()),
        };
        let zeige_fahrtrichtung = |(button_state, unit): &'t mut (button::State, ())| {
            (
                Button::new(button_state, Text::new("Umdrehen"))
                    .on_press(MessageMittelleiter::Umdrehen)
                    .into(),
                unit,
            )
        };
        Anzeige::neu_mit_leiter(
            name,
            geschwindigkeit,
            status,
            ks_iter,
            MessageMittelleiter::Geschwindigkeit,
            zeige_fahrtrichtung,
        )
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
        let ks_iter = |Geschwindigkeit { leiter }: &'t Geschwindigkeit<Zweileiter>| match leiter {
            Zweileiter::Pwm { .. } => None,
            Zweileiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.iter()),
        };
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung, aktuell: &Fahrtrichtung| {
            Radio::new(
                fahrtrichtung,
                fahrtrichtung.to_string(),
                Some(*aktuell),
                MessageZweileiter::Fahrtrichtung,
            )
        };
        let zeige_fahrtrichtung = |fahrtrichtung: &'t mut Fahrtrichtung| {
            (
                Row::new()
                    .push(fahrtrichtung_radio(Fahrtrichtung::Vorwärts, fahrtrichtung))
                    .push(fahrtrichtung_radio(Fahrtrichtung::Rückwärts, fahrtrichtung))
                    .into(),
                fahrtrichtung,
            )
        };
        Anzeige::neu_mit_leiter(
            name,
            geschwindigkeit,
            status,
            ks_iter,
            MessageZweileiter::Geschwindigkeit,
            zeige_fahrtrichtung,
        )
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
        todo!("on_event")
    }

    fn overlay(&mut self, _layout: Layout<'_>) -> Option<overlay::Element<'_, M, R>> {
        todo!("overlay")
    }
}
