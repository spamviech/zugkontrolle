//! Anzeige & Erstellen einer Geschwindigkeit.

use std::{collections::BTreeMap, fmt::Debug, iter, num::NonZeroUsize};

use iced_aw::native::{card, number_input, tab_bar, tabs, Card, TabLabel, Tabs};
use iced_native::{
    button,
    column,
    container,
    event,
    overlay,
    radio,
    row,
    scrollable,
    slider,
    text,
    text_input,
    Button,
    Clipboard,
    Column,
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

use super::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar};
use crate::anschluss::{polarity::Polarität, pwm, OutputSave, ToSave};
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

pub trait LeiterAnzeige: ToSave + Sized {
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

    fn auswahl_neu<'t, R>(status: &'t mut AuswahlStatus) -> Auswahl<'t, Self, R>
    where
        R: 't
            + container::Renderer
            + column::Renderer
            + row::Renderer
            + scrollable::Renderer
            + text::Renderer
            + text_input::Renderer
            + button::Renderer
            + radio::Renderer
            + card::Renderer
            + tabs::Renderer
            + number_input::Renderer,
        <R as tab_bar::Renderer>::Style: From<TabBar>;
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

    fn auswahl_neu<'t, R>(status: &'t mut AuswahlStatus) -> Auswahl<'t, Self, R>
    where
        R: 't
            + container::Renderer
            + column::Renderer
            + row::Renderer
            + scrollable::Renderer
            + text::Renderer
            + text_input::Renderer
            + button::Renderer
            + radio::Renderer
            + card::Renderer
            + tabs::Renderer
            + number_input::Renderer,
        <R as tab_bar::Renderer>::Style: From<TabBar>,
    {
        Auswahl::neu(
            status,
            UmdrehenAnzeige::KonstanteSpannung,
            "Umdrehen",
            &|_umdrehen, pin, polarität| Mittelleiter::Pwm { pin, polarität },
            &|umdrehen, geschwindigkeit| Mittelleiter::KonstanteSpannung {
                geschwindigkeit,
                letzter_wert: 0,
                umdrehen,
            },
        )
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

    fn auswahl_neu<'t, R>(status: &'t mut AuswahlStatus) -> Auswahl<'t, Self, R>
    where
        R: 't
            + container::Renderer
            + column::Renderer
            + row::Renderer
            + scrollable::Renderer
            + text::Renderer
            + text_input::Renderer
            + button::Renderer
            + radio::Renderer
            + card::Renderer
            + tabs::Renderer
            + number_input::Renderer,
        <R as tab_bar::Renderer>::Style: From<TabBar>,
    {
        Auswahl::neu(
            status,
            UmdrehenAnzeige::Immer,
            "Fahrtrichtung",
            &|fahrtrichtung, geschwindigkeit, polarität| Zweileiter::Pwm {
                geschwindigkeit,
                polarität,
                fahrtrichtung,
            },
            &|fahrtrichtung, geschwindigkeit| Zweileiter::KonstanteSpannung {
                geschwindigkeit,
                letzter_wert: 0,
                fahrtrichtung,
            },
        )
    }
}

pub struct Anzeige<'t, M, R> {
    column: Column<'t, M, R>,
}
impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 'static + Clone,
    R: 't + column::Renderer + row::Renderer + text::Renderer + slider::Renderer + radio::Renderer,
{
    pub fn neu_mit_leiter<Leiter, Iter>(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Leiter>,
        status: &'t mut AnzeigeStatus<Leiter>,
        ks_iter: impl FnOnce(&'t Geschwindigkeit<Leiter>) -> Option<Iter>,
        geschwindigkeits_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(&'t mut Leiter::Fahrtrichtung) -> Element<'t, M, R>,
        // TODO overlay mit Anschlüssen?
    ) -> Self
    where
        Leiter: LeiterAnzeige,
        Iter: Iterator,
    {
        let AnzeigeStatus { aktuelle_geschwindigkeit, pwm_slider_state, fahrtrichtung_state } =
            status;
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut column = Column::new().spacing(1).push(Text::new(&name.0));
        column = if let Some(iter) = ks_iter(geschwindigkeit) {
            column.push(
                Row::with_children(
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
                            .spacing(0)
                            .into()
                        })
                        .collect(),
                )
                .spacing(0),
            )
        } else {
            column.push(
                Slider::new(
                    pwm_slider_state,
                    0 ..= u8::MAX,
                    *aktuelle_geschwindigkeit,
                    geschwindigkeits_nachricht,
                )
                .width(Length::Units(100)),
            )
        };
        column = column.push(zeige_fahrtrichtung(fahrtrichtung_state));
        Anzeige { column }
    }
}

impl<'t, M, R> Widget<M, R> for Anzeige<'t, M, R>
where
    R: Renderer + column::Renderer,
{
    reexport_no_event_methods! {Column<'t, M, R>, column, M, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<M>,
    ) -> event::Status {
        self.column.on_event(event, layout, cursor_position, renderer, clipboard, messages)
    }

    fn overlay(&mut self, _layout: Layout<'_>) -> Option<overlay::Element<'_, M, R>> {
        //TODO overlay (Expander-artige Anschlüsse-Anzeige)
        None
    }
}

impl<'t, M, R> From<Anzeige<'t, M, R>> for Element<'t, M, R>
where
    M: 'static,
    R: 't + Renderer + column::Renderer,
{
    fn from(anzeige: Anzeige<'t, M, R>) -> Self {
        Element::new(anzeige)
    }
}

#[derive(Debug)]
enum KonstanteSpannungAnpassen {
    Hinzufügen,
    Entfernen(NonZeroUsize),
}

#[derive(Debug)]
pub struct AuswahlStatus {
    neu_name: String,
    neu_name_state: text_input::State,
    aktueller_tab: usize,
    umdrehen_anschluss: OutputSave,
    umdrehen_state: anschluss::Status<anschluss::Output>,
    pwm_pin: pwm::Save,
    pwm_polarität: Polarität,
    pwm_state: anschluss::PwmState,
    ks_anschlüsse_anpassen: Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse: NonEmpty<(OutputSave, anschluss::Status<anschluss::Output>, button::State)>,
    ks_scrollable_state: scrollable::State,
    hinzufügen_button_state: button::State,
    geschwindigkeiten: BTreeMap<Name, button::State>,
    scrollable_state: scrollable::State,
}

impl AuswahlStatus {
    pub fn neu<'t>(geschwindigkeiten: impl Iterator<Item = &'t Name>) -> Self {
        AuswahlStatus {
            neu_name: String::new(),
            neu_name_state: text_input::State::new(),
            aktueller_tab: 0,
            umdrehen_anschluss: OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
            umdrehen_state: anschluss::Status::neu_output(),
            pwm_pin: pwm::Save(0),
            pwm_polarität: Polarität::Normal,
            pwm_state: anschluss::PwmState::neu(),
            ks_anschlüsse_anpassen: None,
            ks_anschlüsse: NonEmpty::singleton((
                OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
                anschluss::Status::neu_output(),
                button::State::new(),
            )),
            ks_scrollable_state: scrollable::State::new(),
            hinzufügen_button_state: button::State::new(),
            geschwindigkeiten: geschwindigkeiten
                .map(|name| (name.clone(), button::State::new()))
                .collect(),
            scrollable_state: scrollable::State::new(),
        }
    }

    pub fn hinzufügen(&mut self, name: Name) {
        self.geschwindigkeiten.insert(name, button::State::new());
    }

    pub fn entfernen(&mut self, name: &Name) {
        self.geschwindigkeiten.remove(name);
    }
}

#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    Schließen,
    WähleTab(usize),
    Name(String),
    UmdrehenAnschluss(OutputSave),
    PwmPin(pwm::Save),
    PwmPolarität(Polarität),
    KonstanteSpannungAnschluss(usize, OutputSave),
    NeuerKonstanteSpannungAnschluss,
    LöscheKonstanteSpannungAnschluss(NonZeroUsize),
    Hinzufügen,
    Löschen(Name),
}

#[derive(zugkontrolle_derive::Debug, zugkontrolle_derive::Clone)]
pub enum AuswahlNachricht<Leiter: ToSave>
where
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
{
    Schließen,
    Hinzufügen(Name, <Geschwindigkeit<Leiter> as ToSave>::Save),
    Löschen(Name),
}

pub struct Auswahl<'t, Leiter, R>
where
    Leiter: ToSave,
    <Leiter as ToSave>::Save: 't,
    R: card::Renderer,
{
    card: Card<'t, InterneAuswahlNachricht, R>,
    neu_name: &'t mut String,
    aktueller_tab: &'t mut usize,
    umdrehen_anschluss: &'t mut OutputSave,
    pwm_pin: &'t mut pwm::Save,
    pwm_polarität: &'t mut Polarität,
    ks_anschlüsse_anpassen: &'t mut Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse: NonEmpty<&'t mut OutputSave>,
    pwm_nachricht: &'t dyn Fn(OutputSave, pwm::Save, Polarität) -> <Leiter as ToSave>::Save,
    ks_nachricht: &'t dyn Fn(OutputSave, NonEmpty<OutputSave>) -> <Leiter as ToSave>::Save,
}

enum UmdrehenAnzeige {
    KonstanteSpannung,
    Immer,
}

impl<'t, Leiter, R> Auswahl<'t, Leiter, R>
where
    Leiter: ToSave,
    <Leiter as ToSave>::Save: 't,
    R: 't
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + scrollable::Renderer
        + text::Renderer
        + text_input::Renderer
        + button::Renderer
        + radio::Renderer
        + card::Renderer
        + tabs::Renderer
        + number_input::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    fn neu(
        status: &'t mut AuswahlStatus,
        umdrehen_anzeige: UmdrehenAnzeige,
        umdrehen_beschreibung: impl Into<String>,
        pwm_nachricht: &'t impl Fn(OutputSave, pwm::Save, Polarität) -> <Leiter as ToSave>::Save,
        ks_nachricht: &'t impl Fn(OutputSave, NonEmpty<OutputSave>) -> <Leiter as ToSave>::Save,
    ) -> Self {
        let AuswahlStatus {
            neu_name,
            neu_name_state,
            aktueller_tab,
            umdrehen_anschluss,
            umdrehen_state,
            pwm_pin,
            pwm_polarität,
            pwm_state,
            ks_anschlüsse_anpassen,
            ks_anschlüsse,
            ks_scrollable_state,
            hinzufügen_button_state,
            geschwindigkeiten,
            scrollable_state,
        } = status;
        if let Some(anpassen) = ks_anschlüsse_anpassen {
            match anpassen {
                KonstanteSpannungAnpassen::Hinzufügen => ks_anschlüsse.push((
                    OutputSave::Pin { pin: 0, polarität: Polarität::Normal },
                    anschluss::Status::neu_output(),
                    button::State::new(),
                )),
                KonstanteSpannungAnpassen::Entfernen(ix) => {
                    ks_anschlüsse.remove(ix.get());
                },
            }
            *ks_anschlüsse_anpassen = None;
        }
        // TODO Anzeige vorhandene Geschwindigkeiten mit Löschen-Knopf
        let (output_save_head, status_head, button_state_head) = &mut ks_anschlüsse.head;
        let anschlüsse_state_head = (status_head, button_state_head);
        let (anschlüsse_state_tail, anschlüsse_save_tail): (Vec<_>, Vec<_>) = ks_anschlüsse
            .tail
            .iter_mut()
            .map(|(output_save, status, button_state)| ((status, button_state), output_save))
            .unzip();
        let anschlüsse_state =
            NonEmpty { head: anschlüsse_state_head, tail: anschlüsse_state_tail };
        let anschlüsse_save = NonEmpty { head: output_save_head, tail: anschlüsse_save_tail };
        let mut neu = Column::new().push(TextInput::new(
            neu_name_state,
            "<Name>",
            neu_name,
            InterneAuswahlNachricht::Name,
        ));
        let umdrehen_auswahl = Column::new().push(Text::new(umdrehen_beschreibung)).push(
            Element::from(anschluss::Auswahl::neu_output(umdrehen_state))
                .map(InterneAuswahlNachricht::UmdrehenAnschluss),
        );
        let make_radio = |polarität: Polarität| {
            Radio::new(
                polarität,
                polarität.to_string(),
                Some(*pwm_polarität),
                InterneAuswahlNachricht::PwmPolarität,
            )
        };
        let pwm_auswahl = Row::new()
            .push(
                Element::from(anschluss::Pwm::neu(pwm_state)).map(InterneAuswahlNachricht::PwmPin),
            )
            .push(
                Column::new()
                    .push(make_radio(Polarität::Normal))
                    .push(make_radio(Polarität::Invertiert)),
            );
        let mut ks_auswahl = Row::new();
        match umdrehen_anzeige {
            UmdrehenAnzeige::KonstanteSpannung => ks_auswahl = ks_auswahl.push(umdrehen_auswahl),
            UmdrehenAnzeige::Immer => neu = neu.push(umdrehen_auswahl),
        }
        let mut ks_scrollable = Scrollable::new(ks_scrollable_state).height(Length::Units(150));
        for (i, (status, button_state)) in anschlüsse_state.into_iter().enumerate() {
            let ii = i;
            let mut row = Row::new().push(
                Element::from(anschluss::Auswahl::neu_output(status)).map(move |anschluss| {
                    InterneAuswahlNachricht::KonstanteSpannungAnschluss(ii, anschluss)
                }),
            );
            row =
                row.push(
                    if let Some(ix) = NonZeroUsize::new(i) {
                        Element::from(Button::new(button_state, Text::new("X")).on_press(
                            InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix),
                        ))
                    } else {
                        Element::from(
                            Button::new(button_state, Text::new("+"))
                                .on_press(InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss),
                        )
                    },
                );
            ks_scrollable = ks_scrollable.push(row)
        }
        ks_auswahl = ks_auswahl.push(ks_scrollable);
        let tabs = Tabs::new(*aktueller_tab, InterneAuswahlNachricht::WähleTab)
            .push(TabLabel::Text("Pwm".to_string()), pwm_auswahl)
            .push(TabLabel::Text("Konstante Spannung".to_string()), ks_auswahl)
            .height(Length::Shrink)
            .tab_bar_style(TabBar);
        neu = neu.push(tabs);
        let mut scrollable = Scrollable::new(scrollable_state).push(neu).push(
            Button::new(hinzufügen_button_state, Text::new("Hinzufügen"))
                .on_press(InterneAuswahlNachricht::Hinzufügen),
        );
        for (name, button_state) in geschwindigkeiten.iter_mut() {
            let button = Button::new(button_state, Text::new("X"))
                .on_press(InterneAuswahlNachricht::Löschen(name.clone()));
            scrollable = scrollable.push(Column::new().push(Text::new(&name.0)).push(button));
        }
        let card = Card::new(Text::new("Geschwindigkeit"), scrollable)
            .on_close(InterneAuswahlNachricht::Schließen);
        Auswahl {
            card,
            neu_name,
            aktueller_tab,
            umdrehen_anschluss,
            pwm_pin,
            pwm_polarität,
            ks_anschlüsse_anpassen,
            ks_anschlüsse: anschlüsse_save,
            pwm_nachricht,
            ks_nachricht,
        }
    }
}

impl<'t, Leiter, R> Widget<AuswahlNachricht<Leiter>, R> for Auswahl<'t, Leiter, R>
where
    Leiter: ToSave,
    <Leiter as ToSave>::Save: 't,
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
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
                InterneAuswahlNachricht::Schließen => messages.push(AuswahlNachricht::Schließen),
                InterneAuswahlNachricht::WähleTab(tab) => *self.aktueller_tab = tab,
                InterneAuswahlNachricht::Name(name) => *self.neu_name = name,
                InterneAuswahlNachricht::UmdrehenAnschluss(anschluss) => {
                    *self.umdrehen_anschluss = anschluss
                },
                InterneAuswahlNachricht::PwmPin(pin) => *self.pwm_pin = pin,
                InterneAuswahlNachricht::PwmPolarität(polarität) => {
                    *self.pwm_polarität = polarität
                },
                InterneAuswahlNachricht::KonstanteSpannungAnschluss(ix, anschluss) => {
                    *self.ks_anschlüsse[ix] = anschluss
                },
                InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss => {
                    *self.ks_anschlüsse_anpassen = Some(KonstanteSpannungAnpassen::Hinzufügen)
                },
                InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix) => {
                    *self.ks_anschlüsse_anpassen = Some(KonstanteSpannungAnpassen::Entfernen(ix));
                    self.ks_anschlüsse.remove(ix.get());
                },
                InterneAuswahlNachricht::Hinzufügen => messages.push(
                    AuswahlNachricht::Hinzufügen(Name(self.neu_name.clone()), Geschwindigkeit {
                        leiter: if self.aktueller_tab == &0 {
                            (self.pwm_nachricht)(
                                self.umdrehen_anschluss.clone(),
                                self.pwm_pin.clone(),
                                *self.pwm_polarität,
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
                    }),
                ),
                InterneAuswahlNachricht::Löschen(name) => {
                    messages.push(AuswahlNachricht::Löschen(name))
                },
            }
        }
        status
    }
}

impl<'t, Leiter, R> From<Auswahl<'t, Leiter, R>> for Element<'t, AuswahlNachricht<Leiter>, R>
where
    Leiter: 't + ToSave,
    <Leiter as ToSave>::Save: 't,
    <Geschwindigkeit<Leiter> as ToSave>::Save: Debug + Clone,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Leiter, R>) -> Self {
        Element::new(anzeige)
    }
}
