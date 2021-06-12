//! Anzeige & Erstellen einer Geschwindigkeit.

// TODO for now
#![allow(unused_imports)]

use std::{collections::BTreeMap, fmt::Debug, iter};

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
use crate::anschluss::{polarity::Polarität, pwm, OutputSave, ToSave};
use crate::farbe::Farbe;
use crate::non_empty::{MaybeEmpty, NonEmpty};
pub use crate::steuerung::geschwindigkeit::{Error, Geschwindigkeit, Name};
use crate::steuerung::geschwindigkeit::{Fahrtrichtung, Mittelleiter, Zweileiter};

pub type Map<Leiter> = BTreeMap<Name, (Geschwindigkeit<Leiter>, AnzeigeStatus<Leiter>)>;

#[derive(Debug)]
pub struct AnzeigeStatus<Leiter: LeiterAnzeige> {
    aktuelle_geschwindigkeit: u8,
    pwm_slider_state: slider::State,
    fahrtrichtung_state: Leiter::Fahrtrichtung,
}

pub trait LeiterAnzeige: Sized {
    type Fahrtrichtung;
    type Message: Debug + Clone + Send;

    fn anzeige_status_neu() -> AnzeigeStatus<Self>;

    fn anzeige_neu<'t, R>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Self>,
        status: &'t mut AnzeigeStatus<Self>,
    ) -> Anzeige<'t, Self::Message, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer;

    fn anzeige_update(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_status: &mut AnzeigeStatus<Self>,
        message: Self::Message,
    ) -> Result<iced::Command<Self::Message>, Error>;
}

#[derive(Debug, Clone)]
pub enum MessageMittelleiter {
    Geschwindigkeit(u8),
    Umdrehen,
}

impl LeiterAnzeige for Mittelleiter {
    type Fahrtrichtung = button::State;
    type Message = MessageMittelleiter;

    fn anzeige_status_neu() -> AnzeigeStatus<Self> {
        AnzeigeStatus {
            aktuelle_geschwindigkeit: 0,
            pwm_slider_state: slider::State::new(),
            fahrtrichtung_state: button::State::new(),
        }
    }

    fn anzeige_neu<'t, R>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Mittelleiter>,
        status: &'t mut AnzeigeStatus<Mittelleiter>,
    ) -> Anzeige<'t, Self::Message, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer,
    {
        let ks_iter = |Geschwindigkeit { leiter }: &'t Geschwindigkeit<Mittelleiter>| match leiter {
            Mittelleiter::Pwm { .. } => None,
            Mittelleiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.iter()),
        };
        let zeige_fahrtrichtung = |button_state: &'t mut button::State| {
            Button::new(button_state, Text::new("Umdrehen"))
                .on_press(MessageMittelleiter::Umdrehen)
                .into()
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

    fn anzeige_update(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_status: &mut AnzeigeStatus<Self>,
        message: Self::Message,
    ) -> Result<iced::Command<Self::Message>, Error> {
        match message {
            MessageMittelleiter::Geschwindigkeit(wert) => {
                anzeige_status.aktuelle_geschwindigkeit = wert;
                geschwindigkeit.geschwindigkeit(wert)
            },
            MessageMittelleiter::Umdrehen => geschwindigkeit.umdrehen(),
        }
        .map(|()| iced::Command::none())
    }
}

#[derive(Debug, Clone)]
pub enum MessageZweileiter {
    Geschwindigkeit(u8),
    Fahrtrichtung(Fahrtrichtung),
}

impl LeiterAnzeige for Zweileiter {
    type Fahrtrichtung = Fahrtrichtung;
    type Message = MessageZweileiter;

    fn anzeige_status_neu() -> AnzeigeStatus<Self> {
        AnzeigeStatus {
            aktuelle_geschwindigkeit: 0,
            pwm_slider_state: slider::State::new(),
            fahrtrichtung_state: Fahrtrichtung::Vorwärts,
        }
    }

    fn anzeige_neu<'t, R>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Zweileiter>,
        status: &'t mut AnzeigeStatus<Zweileiter>,
    ) -> Anzeige<'t, Self::Message, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer,
    {
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
            Row::new()
                .push(fahrtrichtung_radio(Fahrtrichtung::Vorwärts, fahrtrichtung))
                .push(fahrtrichtung_radio(Fahrtrichtung::Rückwärts, fahrtrichtung))
                .into()
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

    fn anzeige_update(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_status: &mut AnzeigeStatus<Self>,
        message: Self::Message,
    ) -> Result<iced::Command<Self::Message>, Error> {
        match message {
            MessageZweileiter::Geschwindigkeit(wert) => {
                anzeige_status.aktuelle_geschwindigkeit = wert;
                geschwindigkeit.geschwindigkeit(wert)
            },
            MessageZweileiter::Fahrtrichtung(fahrtrichtung) => {
                anzeige_status.fahrtrichtung_state = fahrtrichtung;
                geschwindigkeit.fahrtrichtung(fahrtrichtung)
            },
        }
        .map(|()| iced::Command::none())
    }
}

pub struct Anzeige<'t, M, R> {
    row: Row<'t, M, R>,
}
impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 'static + Clone,
    R: 't + column::Renderer + row::Renderer + text::Renderer + slider::Renderer + radio::Renderer,
{
    pub fn neu_mit_leiter<Leiter: LeiterAnzeige, Iter: Iterator>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Leiter>,
        status: &'t mut AnzeigeStatus<Leiter>,
        ks_iter: impl FnOnce(&'t Geschwindigkeit<Leiter>) -> Option<Iter>,
        geschwindigkeits_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(&'t mut Leiter::Fahrtrichtung) -> Element<'t, M, R>,
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
        row = row.push(zeige_fahrtrichtung(fahrtrichtung_state));
        Anzeige { row }
    }
}

impl<'t, M, R> Widget<M, R> for Anzeige<'t, M, R>
where
    R: Renderer + row::Renderer,
{
    reexport_no_event_methods! {Row<'t, M, R>, row, M, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<M>,
    ) -> event::Status {
        self.row.on_event(event, layout, cursor_position, renderer, clipboard, messages)
    }

    fn overlay(&mut self, _layout: Layout<'_>) -> Option<overlay::Element<'_, M, R>> {
        //TODO overlay (Expander-artige Anschlüsse-Anzeige)
        None
    }
}

impl<'t, M, R> From<Anzeige<'t, M, R>> for Element<'t, M, R>
where
    M: 'static,
    R: 't + Renderer + row::Renderer,
{
    fn from(anzeige: Anzeige<'t, M, R>) -> Self {
        Element::new(anzeige)
    }
}

pub struct AuswahlStatus {
    neu_name: String,
    neu_name_state: text_input::State,
    aktueller_tab: usize,
    umdrehen_anschluss: OutputSave,
    umdrehen_state: anschluss::Status<anschluss::Output>,
    pwm_pin: pwm::Save,
    pwm_state: anschluss::PwmState,
    neuer_ks_anschluss: bool,
    ks_anschlüsse: NonEmpty<(OutputSave, anschluss::Status<anschluss::Output>, button::State)>,
    hinzufügen_button_state: button::State,
}

impl AuswahlStatus {
    pub fn neu() -> Self {
        AuswahlStatus {
            neu_name: String::new(),
            neu_name_state: text_input::State::new(),
            aktueller_tab: 0,
            umdrehen_anschluss: OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
            umdrehen_state: anschluss::Status::neu_output(),
            pwm_pin: pwm::Save(0),
            pwm_state: anschluss::PwmState::neu(),
            neuer_ks_anschluss: false,
            ks_anschlüsse: NonEmpty::singleton((
                OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
                anschluss::Status::neu_output(),
                button::State::new(),
            )),
            hinzufügen_button_state: button::State::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    WähleTab(u8),
    PwmPin(pwm::Save),
    KonstanteSpannungAnschluss(usize, OutputSave),
    NeuerKonstanteSpannungAnschluss,
    LöscheKonstanteSpannungAnschluss(usize),
    Hinzufügen,
}

#[derive(Debug, Clone)]
pub enum AuswahlNachricht<Leiter: ToSave>
where
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
{
    Schließen,
    Hinzufügen(<Geschwindigkeit<Leiter> as ToSave>::Save),
    Löschen(Name),
}

pub struct Auswahl<'t, F, G, R: card::Renderer> {
    card: Card<'t, InterneAuswahlNachricht, R>,
    aktueller_tab: &'t mut u8,
    umdrehen_anschluss: &'t mut OutputSave,
    pwm_pin: &'t mut pwm::Save,
    neuer_ks_anschluss: &'t mut bool,
    ks_anschlüsse: NonEmpty<&'t mut OutputSave>,
    pwm_nachricht: &'t F,
    ks_nachricht: &'t G,
}

enum UmdrehenAnzeige {
    KonstanteSpannung,
    Immer,
}

impl<'t, F, G, R: card::Renderer> Auswahl<'t, F, G, R> {
    fn neu<Leiter>(
        status: &'t mut AuswahlStatus,
        umdrehen_anzeige: UmdrehenAnzeige,
        pwm_nachricht: &'t impl Fn(OutputSave, pwm::Save) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
        ks_nachricht: &'t impl Fn(
            OutputSave,
            NonEmpty<OutputSave>,
        ) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    ) -> Self
    where
        Leiter: ToSave,
        F: Fn(OutputSave, pwm::Save) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
        G: Fn(OutputSave, NonEmpty<OutputSave>) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    {
        let AuswahlStatus {
            neu_name,
            neu_name_state,
            aktueller_tab,
            umdrehen_anschluss,
            umdrehen_state,
            pwm_pin,
            pwm_state,
            neuer_ks_anschluss,
            ks_anschlüsse,
            hinzufügen_button_state,
        } = status;
        if *neuer_ks_anschluss {
            *neuer_ks_anschluss = false;
            ks_anschlüsse.push((
                OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
                anschluss::Status::neu_output(),
                button::State::new(),
            ))
        }
        todo!()
    }
}

impl<'t, Leiter, F, G, R> Widget<AuswahlNachricht<Leiter>, R> for Auswahl<'t, F, G, R>
where
    Leiter: ToSave,
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
    F: Fn(OutputSave, pwm::Save) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    G: Fn(OutputSave, NonEmpty<OutputSave>) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    R: Renderer + card::Renderer,
{
    reexport_no_event_methods! {Card<'t, InterneAuswahlNachricht, R>, card, InterneAuswahlNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<AuswahlNachricht<Leiter>>,
    ) -> event::Status {
        let mut column_messages = Vec::new();
        let mut status = self.card.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut column_messages,
        );
        for message in column_messages {
            status = event::Status::Captured;
            match message {
                InterneAuswahlNachricht::WähleTab(tab) => *self.aktueller_tab = tab,
                InterneAuswahlNachricht::PwmPin(pin) => *self.pwm_pin = pin,
                InterneAuswahlNachricht::KonstanteSpannungAnschluss(ix, anschluss) => {
                    *self.ks_anschlüsse[ix] = anschluss
                },
                InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss => {
                    *self.neuer_ks_anschluss = true
                },
                InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix) => {
                    self.ks_anschlüsse.remove(ix);
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    messages.push(AuswahlNachricht::Hinzufügen(
                        if self.aktueller_tab == &0 {
                            (self.pwm_nachricht)(
                                self.umdrehen_anschluss.clone(),
                                self.pwm_pin.clone(),
                            )
                        } else {
                            (self.ks_nachricht)(
                                self.umdrehen_anschluss.clone(),
                                self.ks_anschlüsse
                                    .iter()
                                    .map(|output_save| (*output_save).clone())
                                    .collect::<MaybeEmpty<_>>()
                                    .unwrap(),
                            )
                        },
                    ))
                },
            }
        }
        status
    }
}

impl<'t, Leiter, F, G, R> From<Auswahl<'t, F, G, R>> for Element<'t, AuswahlNachricht<Leiter>, R>
where
    Leiter: ToSave,
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
    F: Fn(OutputSave, pwm::Save) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    G: Fn(OutputSave, NonEmpty<OutputSave>) -> <Geschwindigkeit<Leiter> as ToSave>::Save,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, F, G, R>) -> Self {
        Element::new(anzeige)
    }
}
