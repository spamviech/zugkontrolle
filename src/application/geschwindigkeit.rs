//! Anzeige und Erstellen einer [Geschwindigkeit].

use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
    num::NonZeroUsize,
    sync::mpsc::Sender,
    time::Duration,
};

use iced::Command;
use iced_aw::native::{card, number_input, tab_bar, tabs, Card, TabLabel, Tabs};
use iced_native::{
    button, column, container, event, overlay, radio, row, scrollable, slider, text, text_input,
    Button, Clipboard, Column, Element, Event, Layout, Length, Point, Radio, Renderer, Row,
    Scrollable, Slider, Text, TextInput, Widget,
};
use log::error;
use nonempty::NonEmpty;

pub use crate::steuerung::geschwindigkeit::{Geschwindigkeit, Name};
use crate::{
    anschluss::{
        de_serialisieren::Serialisiere, pin::pwm, polarität::Polarität, OutputSerialisiert,
    },
    application::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar},
    eingeschränkt::NichtNegativ,
    maybe_empty::MaybeEmpty,
    steuerung::geschwindigkeit::{
        Fahrtrichtung, Fehler, GeschwindigkeitSerialisiert, Leiter, Mittelleiter, Zweileiter,
    },
};

fn remove_from_nonempty_tail<T>(non_empty: &mut NonEmpty<T>, ix: NonZeroUsize) -> Option<T> {
    let i = ix.get();
    // no need to check head, since `i` is non-zero
    if i < non_empty.len() {
        Some(non_empty.tail.remove(i - 1))
    } else {
        None
    }
}

/// Sortierte Map aller Widget zur Anzeige der [Geschwindigkeiten](Geschwindigkeit).
pub type Map<Leiter> = BTreeMap<Name, AnzeigeZustand<Leiter>>;

/// Zustand des Widgets zur Anzeige einer [Geschwindigkeit].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct AnzeigeZustand<L: LeiterAnzeige> {
    name: Name,
    aktuelle_geschwindigkeit: u8,
    pwm_slider_zustand: slider::State,
    fahrtrichtung: <L as LeiterAnzeige>::Fahrtrichtung,
    pwm_frequenz: NichtNegativ,
    verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    stopp_zeit: Duration,
    umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
}

impl<L: LeiterAnzeige> AnzeigeZustand<L> {
    /// Erstelle einen neuen [AnzeigeZustand].
    pub fn neu(
        name: Name,
        fahrtrichtung: <L as LeiterAnzeige>::Fahrtrichtung,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    ) -> Self {
        AnzeigeZustand {
            name,
            aktuelle_geschwindigkeit: 0,
            pwm_slider_zustand: slider::State::new(),
            fahrtrichtung,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        }
    }
}

/// Ermöglicht Erstellen und Anpassen einer [Geschwindigkeit] mit dieser Leiter-Art.
pub trait LeiterAnzeige: Serialisiere + Leiter + Sized {
    /// Zustand für anpassen der [Fahrtrichtung](Leiter::Fahrtrichtung).
    type Fahrtrichtung: Debug;
    /// Nachricht einer [Anzeige].
    type Nachricht: Debug + Clone + Unpin + Send;
    /// Zurücksetzen des Widgets nach fehlerhafter async-Aktion.
    type ZustandZurücksetzen: Debug + Clone + Unpin + Send;

    /// Erstelle einen neuen [AnzeigeZustand].
    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self>;

    /// Erstelle eine neue [Anzeige].
    fn anzeige_neu<'t, R>(
        geschwindigkeit: &Geschwindigkeit<Self>,
        zustand: &'t mut AnzeigeZustand<Self>,
    ) -> Anzeige<'t, Self::Nachricht, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer;

    /// Update-Methode des [Anzeige]-Widgets.
    fn anzeige_update<Nachricht: Send + 'static>(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        message: Self::Nachricht,
        sender: Sender<Nachricht>,
        konvertiere_async_fehler: impl FnOnce(String, Fehler, Self::ZustandZurücksetzen) -> Nachricht
            + Send
            + 'static,
    ) -> Result<Command<Self::Nachricht>, Fehler>;

    /// Zurücksetzen des Widgets nach fehlerhafter async-Aktion.
    fn zustand_zurücksetzen(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        zustand_zurücksetzen: Self::ZustandZurücksetzen,
    ) -> Command<Self::Nachricht>;

    /// Erstelle eine neue [Auswahl].
    fn auswahl_neu<'t, R>(zustand: &'t mut AuswahlZustand) -> Auswahl<'t, Self, R>
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

/// Nachricht einer [Anzeige] für [Mittelleiter]-Geschwindigkeiten.
#[derive(Debug, Clone, Copy)]
pub enum NachrichtMittelleiter {
    /// Anpassen der Fahrgeschwindigkeit.
    Geschwindigkeit(u8),
    /// Umdrehen der Fahrtrichtung.
    Umdrehen,
}

/// Zurücksetzen des Zustands des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenMittelleiter {
    /// Die Geschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
}

impl LeiterAnzeige for Mittelleiter {
    type Fahrtrichtung = button::State;
    type Nachricht = NachrichtMittelleiter;
    type ZustandZurücksetzen = ZustandZurücksetzenMittelleiter;

    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self> {
        AnzeigeZustand {
            name,
            aktuelle_geschwindigkeit: 0,
            pwm_slider_zustand: slider::State::new(),
            fahrtrichtung: button::State::new(),
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        }
    }

    fn anzeige_neu<'t, R>(
        geschwindigkeit: &Geschwindigkeit<Mittelleiter>,
        zustand: &'t mut AnzeigeZustand<Mittelleiter>,
    ) -> Anzeige<'t, Self::Nachricht, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer,
    {
        let zeige_fahrtrichtung = |button_zustand: &'t mut button::State| {
            Button::new(button_zustand, Text::new("Umdrehen"))
                .on_press(NachrichtMittelleiter::Umdrehen)
                .into()
        };
        Anzeige::neu_mit_leiter(
            geschwindigkeit,
            zustand,
            Geschwindigkeit::<Mittelleiter>::ks_länge,
            NachrichtMittelleiter::Geschwindigkeit,
            zeige_fahrtrichtung,
        )
    }

    fn anzeige_update<Nachricht: Send + 'static>(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        message: Self::Nachricht,
        sender: Sender<Nachricht>,
        konvertiere_async_fehler: impl FnOnce(String, Fehler, Self::ZustandZurücksetzen) -> Nachricht
            + Send
            + 'static,
    ) -> Result<Command<Self::Nachricht>, Fehler> {
        match message {
            NachrichtMittelleiter::Geschwindigkeit(wert) => {
                geschwindigkeit.geschwindigkeit(
                    wert,
                    anzeige_zustand.pwm_frequenz,
                    anzeige_zustand.verhältnis_fahrspannung_überspannung,
                )?;
                anzeige_zustand.aktuelle_geschwindigkeit = wert;
            },
            NachrichtMittelleiter::Umdrehen => {
                let bisherige_geschwindigkeit = anzeige_zustand.aktuelle_geschwindigkeit;
                let titel = format!("Umdrehen von Geschwindigkeit {}", anzeige_zustand.name.0);
                anzeige_zustand.aktuelle_geschwindigkeit = 0;
                let _join_handle = geschwindigkeit.async_umdrehen(
                    anzeige_zustand.pwm_frequenz,
                    anzeige_zustand.verhältnis_fahrspannung_überspannung,
                    anzeige_zustand.stopp_zeit,
                    anzeige_zustand.umdrehen_zeit,
                    sender,
                    move |_clone, fehler| {
                        konvertiere_async_fehler(
                            titel,
                            fehler,
                            ZustandZurücksetzenMittelleiter { bisherige_geschwindigkeit },
                        )
                    },
                );
            },
        }
        Ok(Command::none())
    }

    fn zustand_zurücksetzen(
        _geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        zustand_zurücksetzen: Self::ZustandZurücksetzen,
    ) -> Command<Self::Nachricht> {
        anzeige_zustand.aktuelle_geschwindigkeit = zustand_zurücksetzen.bisherige_geschwindigkeit;
        Command::none()
    }

    fn auswahl_neu<'t, R>(zustand: &'t mut AuswahlZustand) -> Auswahl<'t, Self, R>
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
            zustand,
            FahrtrichtungAnschluss::KonstanteSpannung,
            "Umdrehen",
            &|_umdrehen, pin, polarität| Mittelleiter::Pwm { pin, letzter_wert: 0, polarität },
            &|umdrehen, geschwindigkeit| Mittelleiter::KonstanteSpannung {
                geschwindigkeit,
                letzter_wert: 0,
                umdrehen,
            },
        )
    }
}

/// Nachricht einer [Anzeige] für [Zweileiter]-Geschwindigkeiten.
#[derive(Debug, Clone, Copy)]
pub enum NachrichtZweileiter {
    /// Anpassen der Fahrgeschwindigkeit.
    Geschwindigkeit(u8),
    /// Anpassung der Fahrtrichtung.
    Fahrtrichtung(Fahrtrichtung),
}

/// Zurücksetzen des Zustands des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenZweileiter {
    /// Die Fahrgeschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
    /// Die Fahrtrichtung vor der async-Aktion.
    pub bisherige_fahrtrichtung: Fahrtrichtung,
}

impl LeiterAnzeige for Zweileiter {
    type Fahrtrichtung = Fahrtrichtung;
    type Nachricht = NachrichtZweileiter;
    type ZustandZurücksetzen = ZustandZurücksetzenZweileiter;

    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self> {
        AnzeigeZustand {
            name,
            aktuelle_geschwindigkeit: 0,
            pwm_slider_zustand: slider::State::new(),
            fahrtrichtung: Fahrtrichtung::Vorwärts,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        }
    }

    fn anzeige_neu<'t, R>(
        geschwindigkeit: &Geschwindigkeit<Zweileiter>,
        zustand: &'t mut AnzeigeZustand<Zweileiter>,
    ) -> Anzeige<'t, Self::Nachricht, R>
    where
        R: 't
            + column::Renderer
            + row::Renderer
            + button::Renderer
            + text::Renderer
            + slider::Renderer
            + radio::Renderer,
    {
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung, aktuell: &Fahrtrichtung| {
            Radio::new(
                fahrtrichtung,
                fahrtrichtung.to_string(),
                Some(*aktuell),
                NachrichtZweileiter::Fahrtrichtung,
            )
        };
        let zeige_fahrtrichtung = |fahrtrichtung: &'t mut Fahrtrichtung| {
            Row::new()
                .push(fahrtrichtung_radio(Fahrtrichtung::Vorwärts, fahrtrichtung))
                .push(fahrtrichtung_radio(Fahrtrichtung::Rückwärts, fahrtrichtung))
                .into()
        };
        Anzeige::neu_mit_leiter(
            geschwindigkeit,
            zustand,
            Geschwindigkeit::<Zweileiter>::ks_länge,
            NachrichtZweileiter::Geschwindigkeit,
            zeige_fahrtrichtung,
        )
    }

    fn anzeige_update<Nachricht: Send + 'static>(
        geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        message: Self::Nachricht,
        sender: Sender<Nachricht>,
        konvertiere_async_fehler: impl FnOnce(String, Fehler, Self::ZustandZurücksetzen) -> Nachricht
            + Send
            + 'static,
    ) -> Result<Command<Self::Nachricht>, Fehler> {
        match message {
            NachrichtZweileiter::Geschwindigkeit(wert) => {
                geschwindigkeit.geschwindigkeit(
                    wert,
                    anzeige_zustand.pwm_frequenz,
                    anzeige_zustand.verhältnis_fahrspannung_überspannung,
                )?;
                anzeige_zustand.aktuelle_geschwindigkeit = wert;
            },
            NachrichtZweileiter::Fahrtrichtung(fahrtrichtung) => {
                let bisherige_geschwindigkeit = anzeige_zustand.aktuelle_geschwindigkeit;
                let bisherige_fahrtrichtung = anzeige_zustand.fahrtrichtung;
                let titel = format!(
                    "Fahrtrichtung einstellen von Geschwindigkeit {} auf {}",
                    anzeige_zustand.name.0, fahrtrichtung
                );
                anzeige_zustand.aktuelle_geschwindigkeit = 0;
                anzeige_zustand.fahrtrichtung = fahrtrichtung;
                let _join_handle = geschwindigkeit.async_fahrtrichtung(
                    fahrtrichtung,
                    anzeige_zustand.pwm_frequenz,
                    anzeige_zustand.stopp_zeit,
                    sender,
                    move |_clone, fehler| {
                        konvertiere_async_fehler(
                            titel,
                            fehler,
                            ZustandZurücksetzenZweileiter {
                                bisherige_geschwindigkeit,
                                bisherige_fahrtrichtung,
                            },
                        )
                    },
                );
            },
        }
        Ok(Command::none())
    }

    fn zustand_zurücksetzen(
        _geschwindigkeit: &mut Geschwindigkeit<Self>,
        anzeige_zustand: &mut AnzeigeZustand<Self>,
        zustand_zurücksetzen: Self::ZustandZurücksetzen,
    ) -> Command<Self::Nachricht> {
        anzeige_zustand.aktuelle_geschwindigkeit = zustand_zurücksetzen.bisherige_geschwindigkeit;
        anzeige_zustand.fahrtrichtung = zustand_zurücksetzen.bisherige_fahrtrichtung;
        Command::none()
    }

    fn auswahl_neu<'t, R>(zustand: &'t mut AuswahlZustand) -> Auswahl<'t, Self, R>
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
            zustand,
            FahrtrichtungAnschluss::Immer,
            "Fahrtrichtung",
            &|fahrtrichtung, geschwindigkeit, polarität| Zweileiter::Pwm {
                geschwindigkeit,
                letzter_wert: 0,
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

/// Anzeige und Steuerung einer [Geschwindigkeit].
pub struct Anzeige<'t, M, R> {
    column: Column<'t, M, R>,
}

impl<M, R> Debug for Anzeige<'_, M, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Anzeige").field("column", &"<Column>").finish()
    }
}

impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 'static + Clone,
    R: 't + column::Renderer + row::Renderer + text::Renderer + slider::Renderer + radio::Renderer,
{
    /// Erstelle eine neue [Anzeige] für eine [LeiterAnzeige].
    pub fn neu_mit_leiter<'s, Leiter: LeiterAnzeige>(
        geschwindigkeit: &'s Geschwindigkeit<Leiter>,
        zustand: &'t mut AnzeigeZustand<Leiter>,
        ks_länge: impl FnOnce(&'s Geschwindigkeit<Leiter>) -> Option<usize>,
        geschwindigkeit_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(
            &'t mut <Leiter as LeiterAnzeige>::Fahrtrichtung,
        ) -> Element<'t, M, R>,
        // TODO overlay mit Anschlüssen?
    ) -> Self {
        let AnzeigeZustand {
            name,
            aktuelle_geschwindigkeit,
            pwm_slider_zustand,
            fahrtrichtung,
            pwm_frequenz: _,
            verhältnis_fahrspannung_überspannung: _,
            stopp_zeit: _,
            umdrehen_zeit: _,
        } = zustand;
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut column = Column::new().spacing(1).push(Text::new(&name.0));
        column = if let Some(länge) = ks_länge(geschwindigkeit) {
            column.push(
                Row::with_children(
                    (0..=länge)
                        .map(|i| {
                            let i_u8 = i as u8;
                            Radio::new(
                                i_u8,
                                i_u8.to_string(),
                                Some(*aktuelle_geschwindigkeit),
                                geschwindigkeit_nachricht.clone(),
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
                    pwm_slider_zustand,
                    0..=u8::MAX,
                    *aktuelle_geschwindigkeit,
                    geschwindigkeit_nachricht,
                )
                .width(Length::Units(100)),
            )
        };
        column = column.push(zeige_fahrtrichtung(fahrtrichtung));
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

/// Zustand für das Auswahl-Fenster zum Erstellen und Anpassen einer [Geschwindigkeit].
#[derive(Debug)]
pub struct AuswahlZustand {
    neu_name: String,
    neu_name_zustand: text_input::State,
    aktueller_tab: usize,
    umdrehen_anschluss: OutputSerialisiert,
    umdrehen_zustand: anschluss::Zustand<anschluss::Output>,
    pwm_pin: pwm::Serialisiert,
    pwm_polarität: Polarität,
    pwm_zustand: anschluss::PwmZustand,
    ks_anschlüsse_anpassen: Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse:
        NonEmpty<(OutputSerialisiert, anschluss::Zustand<anschluss::Output>, button::State)>,
    ks_scrollable_zustand: scrollable::State,
    hinzufügen_button_zustand: button::State,
    geschwindigkeiten: BTreeMap<Name, (String, button::State)>,
    scrollable_zustand: scrollable::State,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [AuswahlZustand].
    pub fn neu<'t, Leiter: 't + Display>(
        geschwindigkeiten: impl Iterator<Item = (&'t Name, &'t Geschwindigkeit<Leiter>)>,
    ) -> Self {
        AuswahlZustand {
            neu_name: String::new(),
            neu_name_zustand: text_input::State::new(),
            aktueller_tab: 0,
            umdrehen_anschluss: OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
            umdrehen_zustand: anschluss::Zustand::neu_output(),
            pwm_pin: pwm::Serialisiert(0),
            pwm_polarität: Polarität::Normal,
            pwm_zustand: anschluss::PwmZustand::neu(),
            ks_anschlüsse_anpassen: None,
            ks_anschlüsse: NonEmpty::singleton((
                OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
                anschluss::Zustand::neu_output(),
                button::State::new(),
            )),
            ks_scrollable_zustand: scrollable::State::new(),
            hinzufügen_button_zustand: button::State::new(),
            geschwindigkeiten: geschwindigkeiten.map(Self::iter_map).collect(),
            scrollable_zustand: scrollable::State::new(),
        }
    }

    fn iter_map<'t, Leiter: 't + Display>(
        (name, geschwindigkeit): (&'t Name, &'t Geschwindigkeit<Leiter>),
    ) -> (Name, (String, button::State)) {
        (name.clone(), (geschwindigkeit.to_string(), button::State::new()))
    }

    /// Füge eine neue [Geschwindigkeit] hinzu.
    pub fn hinzufügen<Leiter: Display>(
        &mut self,
        name: &Name,
        geschwindigkeit: &Geschwindigkeit<Leiter>,
    ) {
        let (key, value) = Self::iter_map((name, geschwindigkeit));
        let _ = self.geschwindigkeiten.insert(key, value);
    }

    /// Entferne eine [Geschwindigkeit].
    pub fn entfernen(&mut self, name: &Name) {
        let _ = self.geschwindigkeiten.remove(name);
    }
}

#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    Schließen,
    WähleTab(usize),
    Name(String),
    UmdrehenAnschluss(OutputSerialisiert),
    PwmPin(pwm::Serialisiert),
    PwmPolarität(Polarität),
    KonstanteSpannungAnschluss(usize, OutputSerialisiert),
    NeuerKonstanteSpannungAnschluss,
    LöscheKonstanteSpannungAnschluss(NonZeroUsize),
    Hinzufügen,
    Löschen(Name),
}

/// Nachricht eines [Auswahl]-Widgets.
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone)]
#[zugkontrolle_debug(Leiter::Serialisiert: Debug)]
#[zugkontrolle_debug( <Geschwindigkeit<Leiter> as Serialisiere>::Serialisiert: Debug)]
#[zugkontrolle_clone(Leiter: Serialisiere)]
#[zugkontrolle_clone(<Geschwindigkeit<Leiter> as Serialisiere>::Serialisiert: Clone)]
pub enum AuswahlNachricht<Leiter: Serialisiere> {
    /// Schließe das Auswahl-Fenster.
    Schließen,
    /// Füge eine neue [Geschwindigkeit] hinzu.
    Hinzufügen(Name, <Geschwindigkeit<Leiter> as Serialisiere>::Serialisiert),
    /// Lösche eine [Geschwindigkeit].
    Löschen(Name),
}

/// Hinzufügen und Anpassen einer [Geschwindigkeit].
pub struct Auswahl<'t, Leiter, R>
where
    Leiter: Serialisiere,
    <Leiter as Serialisiere>::Serialisiert: 't,
    R: card::Renderer,
{
    card: Card<'t, InterneAuswahlNachricht, R>,
    neu_name: &'t mut String,
    aktueller_tab: &'t mut usize,
    umdrehen_anschluss: &'t mut OutputSerialisiert,
    pwm_pin: &'t mut pwm::Serialisiert,
    pwm_polarität: &'t mut Polarität,
    ks_anschlüsse_anpassen: &'t mut Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse: NonEmpty<&'t mut OutputSerialisiert>,
    pwm_nachricht: &'t dyn Fn(
        OutputSerialisiert,
        pwm::Serialisiert,
        Polarität,
    ) -> <Leiter as Serialisiere>::Serialisiert,
    ks_nachricht: &'t dyn Fn(
        OutputSerialisiert,
        NonEmpty<OutputSerialisiert>,
    ) -> <Leiter as Serialisiere>::Serialisiert,
}

impl<'t, Leiter, R> Debug for Auswahl<'t, Leiter, R>
where
    Leiter: Serialisiere,
    <Leiter as Serialisiere>::Serialisiert: 't,
    R: card::Renderer,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Auswahl")
            .field("card", &"<Card>")
            .field("neu_name", &self.neu_name)
            .field("aktueller_tab", &self.aktueller_tab)
            .field("umdrehen_anschluss", &self.umdrehen_anschluss)
            .field("pwm_pin", &self.pwm_pin)
            .field("pwm_polarität", &self.pwm_polarität)
            .field("ks_anschlüsse_anpassen", &self.ks_anschlüsse_anpassen)
            .field("ks_anschlüsse", &self.ks_anschlüsse)
            .field("pwm_nachricht", &"<closure>")
            .field("ks_nachricht", &"<closure>")
            .finish()
    }
}

/// Wo soll eine Auswahl für einen Anschluss zum Einstellen der Fahrtrichtung angezeigt werden.
#[derive(Debug, Clone, Copy)]
pub enum FahrtrichtungAnschluss {
    /// Nur für [Geschwindigkeiten](Geschwindigkeit) die über ein Pwm-Signal gesteuert werden.
    Pwm,
    /// Nur für [Geschwindigkeiten](Geschwindigkeit) die über mehrere Anschlüsse
    /// mit konstanter Spannung gesteuert werden.
    KonstanteSpannung,
    /// Bei allen [Geschwindigkeiten](Geschwindigkeit), unabhängig davon wie sie gesteuert werden.
    Immer,
}

impl<'t, Leiter, R> Auswahl<'t, Leiter, R>
where
    Leiter: Serialisiere,
    <Leiter as Serialisiere>::Serialisiert: 't,
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
    /// Erstelle eine neue [Auswahl].
    pub fn neu(
        zustand: &'t mut AuswahlZustand,
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        fahrtrichtung_beschreibung: impl Into<String>,
        pwm_nachricht: &'t impl Fn(
            OutputSerialisiert,
            pwm::Serialisiert,
            Polarität,
        ) -> <Leiter as Serialisiere>::Serialisiert,
        ks_nachricht: &'t impl Fn(
            OutputSerialisiert,
            NonEmpty<OutputSerialisiert>,
        ) -> <Leiter as Serialisiere>::Serialisiert,
    ) -> Self {
        let AuswahlZustand {
            neu_name,
            neu_name_zustand,
            aktueller_tab,
            umdrehen_anschluss,
            umdrehen_zustand,
            pwm_pin,
            pwm_polarität,
            pwm_zustand,
            ks_anschlüsse_anpassen,
            ks_anschlüsse,
            ks_scrollable_zustand,
            hinzufügen_button_zustand,
            geschwindigkeiten,
            scrollable_zustand,
        } = zustand;
        if let Some(anpassen) = ks_anschlüsse_anpassen {
            match anpassen {
                KonstanteSpannungAnpassen::Hinzufügen => ks_anschlüsse.push((
                    OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
                    anschluss::Zustand::neu_output(),
                    button::State::new(),
                )),
                KonstanteSpannungAnpassen::Entfernen(ix) => {
                    let _ = remove_from_nonempty_tail(ks_anschlüsse, *ix);
                },
            }
            *ks_anschlüsse_anpassen = None;
        }
        let (output_save_head, zustand_head, button_zustand_head) = &mut ks_anschlüsse.head;
        let anschlüsse_zustand_head = (zustand_head, button_zustand_head);
        let (anschlüsse_zustand_tail, anschlüsse_save_tail): (Vec<_>, Vec<_>) = ks_anschlüsse
            .tail
            .iter_mut()
            .map(|(output_save, zustand, button_zustand)| ((zustand, button_zustand), output_save))
            .unzip();
        let anschlüsse_zustand =
            NonEmpty { head: anschlüsse_zustand_head, tail: anschlüsse_zustand_tail };
        let anschlüsse_save = NonEmpty { head: output_save_head, tail: anschlüsse_save_tail };
        let width = Length::Units(950);
        let mut neu = Column::new().push(
            TextInput::new(neu_name_zustand, "<Name>", neu_name, InterneAuswahlNachricht::Name)
                .width(width),
        );
        let umdrehen_auswahl = Column::new().push(Text::new(fahrtrichtung_beschreibung)).push(
            Element::from(anschluss::Auswahl::neu_output(umdrehen_zustand))
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
        let mut pwm_auswahl = Row::new();
        let mut ks_auswahl = Row::new();
        match fahrtrichtung_anschluss {
            FahrtrichtungAnschluss::Pwm => pwm_auswahl = pwm_auswahl.push(umdrehen_auswahl),
            FahrtrichtungAnschluss::KonstanteSpannung => {
                ks_auswahl = ks_auswahl.push(umdrehen_auswahl)
            },
            FahrtrichtungAnschluss::Immer => neu = neu.push(umdrehen_auswahl),
        }
        let pwm_auswahl = pwm_auswahl
            .push(
                Element::from(anschluss::Pwm::neu(pwm_zustand))
                    .map(InterneAuswahlNachricht::PwmPin),
            )
            .push(
                Column::new()
                    .push(make_radio(Polarität::Normal))
                    .push(make_radio(Polarität::Invertiert)),
            );
        let mut ks_scrollable = Scrollable::new(ks_scrollable_zustand).height(Length::Units(150));
        for (i, (zustand, button_zustand)) in anschlüsse_zustand.into_iter().enumerate() {
            let ii = i;
            let mut row = Row::new().push(
                Element::from(anschluss::Auswahl::neu_output(zustand)).map(move |anschluss| {
                    InterneAuswahlNachricht::KonstanteSpannungAnschluss(ii, anschluss)
                }),
            );
            row = row.push(if let Some(ix) = NonZeroUsize::new(i) {
                Element::from(
                    Button::new(button_zustand, Text::new("X"))
                        .on_press(InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix)),
                )
            } else {
                Element::from(
                    Button::new(button_zustand, Text::new("+"))
                        .on_press(InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss),
                )
            });
            ks_scrollable = ks_scrollable.push(row)
        }
        let ks_auswahl = ks_auswahl.push(ks_scrollable);
        let tabs = Tabs::new(*aktueller_tab, InterneAuswahlNachricht::WähleTab)
            .push(TabLabel::Text("Pwm".to_string()), pwm_auswahl)
            .push(TabLabel::Text("Konstante Spannung".to_string()), ks_auswahl)
            .width(width)
            .height(Length::Shrink)
            .tab_bar_style(TabBar);
        let neu = neu.push(tabs);
        let mut scrollable = Scrollable::new(scrollable_zustand).push(neu).push(
            Button::new(hinzufügen_button_zustand, Text::new("Hinzufügen"))
                .on_press(InterneAuswahlNachricht::Hinzufügen),
        );
        for (name, (anschlüsse, button_zustand)) in geschwindigkeiten.iter_mut() {
            let button = Button::new(button_zustand, Text::new("X"))
                .on_press(InterneAuswahlNachricht::Löschen(name.clone()));
            scrollable = scrollable.push(
                Column::new().push(Text::new(&name.0)).push(Text::new(&*anschlüsse)).push(button),
            );
        }
        let card = Card::new(Text::new("Geschwindigkeit"), scrollable)
            .on_close(InterneAuswahlNachricht::Schließen)
            .width(Length::Shrink);
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
    Leiter: Serialisiere,
    <Leiter as Serialisiere>::Serialisiert: 't,
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
                InterneAuswahlNachricht::KonstanteSpannungAnschluss(ix, anschluss_neu) => {
                    if let Some(anschluss) = self.ks_anschlüsse.get_mut(ix) {
                        **anschluss = anschluss_neu
                    } else {
                        error!(
                            "Update-Nachricht für Anschluss {}, es gibt aber nur {}!",
                            ix,
                            self.ks_anschlüsse.len()
                        )
                    }
                },
                InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss => {
                    *self.ks_anschlüsse_anpassen = Some(KonstanteSpannungAnpassen::Hinzufügen)
                },
                InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix) => {
                    *self.ks_anschlüsse_anpassen = Some(KonstanteSpannungAnpassen::Entfernen(ix));
                    let _ = remove_from_nonempty_tail(&mut self.ks_anschlüsse, ix);
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    messages.push(AuswahlNachricht::Hinzufügen(
                        Name(self.neu_name.clone()),
                        GeschwindigkeitSerialisiert {
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
                        },
                    ))
                },
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
    Leiter: 't + Serialisiere,
    <Leiter as Serialisiere>::Serialisiert: 't,
    R: 't + Renderer + card::Renderer,
{
    fn from(anzeige: Auswahl<'t, Leiter, R>) -> Self {
        Element::new(anzeige)
    }
}
