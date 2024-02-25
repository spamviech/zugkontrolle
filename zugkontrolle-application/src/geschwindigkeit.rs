//! Anzeige und Erstellen einer [`Geschwindigkeit`].

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Debug, Display, Formatter},
    num::NonZeroUsize,
    ops::DerefMut,
    time::Duration,
};

use iced_aw::{
    native::{
        card::{self, Card},
        TabLabel, Tabs,
    },
    style::{number_input, tab_bar},
};
use iced_core::{
    event, text as text_core,
    widget::text::{self, Text},
    Element, Font, Length, Renderer,
};
use iced_widget::{
    button::{self, Button},
    container,
    radio::{self, Radio},
    scrollable::{self, Scrollable},
    slider::{self, Slider},
    text_input::{self, TextInput},
    Column, Row, Space,
};
use log::error;
use nonempty::NonEmpty;

use zugkontrolle_anschluss::{pin::pwm, polarität::Polarität, OutputSerialisiert};
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_gleis::steuerung::{
    geschwindigkeit::{
        Fahrtrichtung, Geschwindigkeit, GeschwindigkeitSerialisiert, Leiter, Mittelleiter,
        MittelleiterSerialisiert, Name, Zweileiter, ZweileiterSerialisiert,
    },
    plan::AktionGeschwindigkeit,
};
use zugkontrolle_util::{eingeschränkt::NichtNegativ, unicase_ord::UniCaseOrd};

use crate::{
    anschluss,
    bootstrap::{Bootstrap, Icon},
    map_mit_zustand::MapMitZustand,
    style::{sammlung::Sammlung, tab_bar::TabBar},
};

/// Versuche ein Element vom [`NonEmpty::tail`] zu entfernen.
fn remove_from_nonempty_tail<T>(non_empty: &mut NonEmpty<T>, ix: NonZeroUsize) -> Option<T> {
    let i = ix.get();
    // no need to check head, since `i` is non-zero
    (i < non_empty.len()).then(|| {
        non_empty.tail.remove(
            // 1 <= i < non_empty.len()
            #[allow(clippy::arithmetic_side_effects)]
            {
                i - 1
            },
        )
    })
}

/// Sortierte Map aller Widget zur Anzeige der [`Geschwindigkeiten`](Geschwindigkeit).
pub type Set = BTreeSet<Name>;

/// Zustand des Widgets zur Anzeige einer [`Geschwindigkeit`].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct AnzeigeZustand<L: Leiter> {
    /// Der Name der Geschwindigkeit.
    name: Name,
    /// Die aktuelle Pwm-Frequenz.
    pwm_frequenz: NichtNegativ,
    /// Wie groß ist die maximale Fahrspannung im Verhältnis zur Spannung zum Umdrehen (aktuell nur für [Mittelleiter]).
    verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    /// Wie lange wird nach anhalten gewartet, bis die Fahrtrichtung umgedreht wird.
    stopp_zeit: Duration,
    /// Wie lange ist die Überspannung beim Umdrehen [`Fließend`](crate::anschluss::polarität::Fließend::Fließend).
    umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
}

impl<L: Leiter> AnzeigeZustand<L> {
    /// Erstelle einen neuen [`AnzeigeZustand`].
    pub fn neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
    ) -> Self {
        AnzeigeZustand {
            name,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        }
    }
}

/// Anzeige und Steuerung einer [`Geschwindigkeit`].
pub struct Anzeige<'t, M, R> {
    /// Das Element mit der Widget-Hierarchie.
    element: Element<'t, M, R>,
}

impl<M, R> Debug for Anzeige<'_, M, R> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 't + Clone,
    R: 't + text_core::Renderer,
    <R as Renderer>::Theme: radio::StyleSheet + slider::StyleSheet + text::StyleSheet,
{
    /// Erstelle eine neue [Anzeige] für einen [`Leiter`].
    pub fn neu<'s, L: Leiter>(
        name: &'s Name,
        geschwindigkeit: &'s Geschwindigkeit<L>,
        ks_länge: impl FnOnce(&'s Geschwindigkeit<L>) -> Option<usize>,
        geschwindigkeit_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(Option<<L as Leiter>::Fahrtrichtung>) -> Element<'t, M, R>,
        // TODO overlay mit Anschlüssen?
    ) -> Self {
        let aktuelle_geschwindigkeit = geschwindigkeit.aktuelle_geschwindigkeit();
        let aktuelle_fahrtrichtung = geschwindigkeit.aktuelle_fahrtrichtung();
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut column = Column::new().spacing(1).push(Text::new(name.0.clone()));
        column = if let Some(länge) = ks_länge(geschwindigkeit) {
            if länge > u8::MAX.into() {
                error!("Zu viele Anschlüsse mit Konstanter Spannung bei einer Geschwindigkeit: {länge}");
            }
            column.push(
                Row::with_children(
                    (0..=länge)
                        .map(|i| {
                            let i_u8 = u8::try_from(i).unwrap_or(u8::MAX);
                            Radio::new(
                                i_u8.to_string(),
                                i_u8,
                                Some(aktuelle_geschwindigkeit),
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
                Slider::new(0..=u8::MAX, aktuelle_geschwindigkeit, geschwindigkeit_nachricht)
                    .width(Length::Fixed(100.)),
            )
        };
        column = column.push(zeige_fahrtrichtung(aktuelle_fahrtrichtung));
        Anzeige { element: column.into() }
    }
}

impl<'t, L, R> From<Anzeige<'t, AktionGeschwindigkeit<L>, R>>
    for Element<'t, AktionGeschwindigkeit<L>, R>
where
    L: 'static + Leiter,
    R: 't + Renderer,
{
    fn from(anzeige: Anzeige<'t, AktionGeschwindigkeit<L>, R>) -> Self {
        anzeige.element
    }
}

/// Welche Tab-Seite wird angezeigt.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TabId {
    /// Auswahl für Steuerung über einen Pwm-Pin wird angezeigt.
    Pwm,
    /// Auswahl für Steuerung über mehrere Anschlüsse mit unterschiedlicher konstanter Spannung wird angezeigt.
    KonstanteSpannung,
}

/// Zustand für das Auswahl-Fenster zum Erstellen und Anpassen einer [`Geschwindigkeit`].
#[derive(Debug, PartialEq, Eq)]
struct AuswahlZustand<S> {
    /// Der aktuell gewählte Name.
    neu_name: String,
    /// Der aktuell angezeigt Tab.
    aktueller_tab: TabId,
    /// Der aktuelle Anschluss zum einstellen der Fahrtrichtung.
    umdrehen_anschluss: OutputSerialisiert,
    /// Der aktuell gewählte Pin zur Steuerung über ein Pwm-Signal.
    pwm_pin: pwm::Serialisiert,
    /// Die aktuell gewählte Polarität des Pwm-Signals
    pwm_polarität: Polarität,
    /// Die aktuell gewählten Anschlüsse zur Steuerung über konstante Spannungswerte.
    ks_anschlüsse: NonEmpty<OutputSerialisiert>,
    /// Bereits existierende Geschwindigkeiten.
    geschwindigkeiten: BTreeMap<UniCaseOrd<Name>, (String, GeschwindigkeitSerialisiert<S>)>,
}

/// Der Startwert für ein [`Auswahl`]-Widget.
#[derive(Debug, Clone)]
#[allow(variant_size_differences)]
pub enum AuswahlStartwert {
    /// Steuerung über ein Pwm-Signal.
    Pwm {
        /// Anschluss zur Steuerung der Fahrtrichtung.
        umdrehen_anschluss: Option<OutputSerialisiert>,
        /// Der [`Pwm-Pin`](pwm::Pin).
        pwm_pin: pwm::Serialisiert,
        /// Die Polarität des Pwm-Signals.
        polarität: Polarität,
    },
    /// Steuerung über mehrere Anschlüsse mit konstanter Spannung.
    KonstanteSpannung {
        /// Anschluss zur Steuerung der Fahrtrichtung.
        umdrehen_anschluss: Option<OutputSerialisiert>,
        /// Die Anschlüsse zum steuern der Geschwindigkeit.
        geschwindigkeit_anschlüsse: NonEmpty<OutputSerialisiert>,
    },
}

impl<LeiterSerialisiert> AuswahlZustand<LeiterSerialisiert> {
    /// Erstelle einen neuen [`AuswahlZustand`].
    fn neu<'t>(
        startwert: Option<(Name, AuswahlStartwert)>,
        geschwindigkeiten: impl Iterator<
            Item = (&'t Name, &'t GeschwindigkeitSerialisiert<LeiterSerialisiert>),
        >,
    ) -> AuswahlZustand<LeiterSerialisiert>
    where
        LeiterSerialisiert: 't + Clone + Display,
    {
        let geschwindigkeiten = geschwindigkeiten.map(Self::iter_map).collect();
        let standard_anschluss = OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal };
        let standard_pwm_pin = pwm::Serialisiert(0);
        match startwert {
            Some((name, AuswahlStartwert::Pwm { umdrehen_anschluss, pwm_pin, polarität })) => {
                AuswahlZustand {
                    neu_name: name.0,
                    aktueller_tab: TabId::Pwm,
                    umdrehen_anschluss: umdrehen_anschluss
                        .unwrap_or_else(|| standard_anschluss.clone()),
                    pwm_pin,
                    pwm_polarität: polarität,
                    ks_anschlüsse: NonEmpty::singleton(standard_anschluss),
                    geschwindigkeiten,
                }
            },
            Some((
                name,
                AuswahlStartwert::KonstanteSpannung {
                    umdrehen_anschluss,
                    geschwindigkeit_anschlüsse,
                },
            )) => AuswahlZustand {
                neu_name: name.0,
                aktueller_tab: TabId::KonstanteSpannung,
                umdrehen_anschluss: umdrehen_anschluss
                    .unwrap_or_else(|| standard_anschluss.clone()),
                pwm_pin: standard_pwm_pin,
                pwm_polarität: Polarität::Normal,
                ks_anschlüsse: geschwindigkeit_anschlüsse,
                geschwindigkeiten,
            },
            None => AuswahlZustand {
                neu_name: String::new(),
                aktueller_tab: TabId::Pwm,
                umdrehen_anschluss: standard_anschluss.clone(),
                pwm_pin: standard_pwm_pin,
                pwm_polarität: Polarität::Normal,
                ks_anschlüsse: NonEmpty::singleton(standard_anschluss),
                geschwindigkeiten,
            },
        }
    }

    /// Konvertiere Map-Einträge für Geschwindigkeiten in benötigte Informationen zur Darstellung.
    fn iter_map(
        (name, geschwindigkeit): (&Name, &GeschwindigkeitSerialisiert<LeiterSerialisiert>),
    ) -> (UniCaseOrd<Name>, (String, GeschwindigkeitSerialisiert<LeiterSerialisiert>))
    where
        LeiterSerialisiert: Clone + Display,
    {
        (UniCaseOrd::neu(name.clone()), (geschwindigkeit.to_string(), geschwindigkeit.clone()))
    }
}

/// Interne Nachrichten zur Interaktion mit einem [`Auswahl`]-Widget.
#[derive(Debug, Clone)]
enum InterneAuswahlNachricht {
    /// Schließe das Auswahl-Fenster.
    Schließen,
    /// Wechsle auf die gewünschten Tab-Seite.
    WähleTab(TabId),
    /// Neuer gewählter Name.
    Name(String),
    /// Neuer gewählter Anschluss für das einstellen der Fahrtrichtung.
    UmdrehenAnschluss(OutputSerialisiert),
    /// Neuer Pin zur Steuerung über ein Pwm-Signal.
    PwmPin(pwm::Serialisiert),
    /// Neue Polarität bei der Steuerung über ein Pwm-Signal.
    PwmPolarität(Polarität),
    /// Anpassen eines Anschlusses zu Steuerung über konstante Spannungswerte.
    KonstanteSpannungAnschluss(usize, OutputSerialisiert),
    /// Neuer Anschluss zur Steuerung über konstante Spannungswerte.
    NeuerKonstanteSpannungAnschluss,
    /// Entferne einen Anschluss zur Steuerung über konstante Spannungswerte.
    LöscheKonstanteSpannungAnschluss(NonZeroUsize),
    /// Füge die aktuell gewählte Geschwindigkeit hinzu.
    Hinzufügen,
    /// Entferne eine bekannte Geschwindigkeit.
    Löschen(Name),
    /// Bearbeite eine bekannte Geschwindigkeit.
    Bearbeiten(Name, AuswahlStartwert),
}

/// Nachricht eines [`Auswahl`]-Widgets.
#[derive(Debug, Clone)]
pub enum AuswahlNachricht<LeiterSerialisiert> {
    /// Schließe das Auswahl-Fenster.
    Schließen,
    /// Füge eine neue [`Geschwindigkeit`] hinzu.
    Hinzufügen(Name, GeschwindigkeitSerialisiert<LeiterSerialisiert>),
    /// Lösche eine [`Geschwindigkeit`].
    Löschen(Name),
}

/// Hinzufügen und Anpassen einer [`Geschwindigkeit`].
#[derive(Debug)]
pub struct Auswahl<'t, LeiterSerialisiert, R>(
    MapMitZustand<
        't,
        AuswahlZustand<LeiterSerialisiert>,
        InterneAuswahlNachricht,
        AuswahlNachricht<LeiterSerialisiert>,
        R,
    >,
);

/// Wo soll eine Auswahl für einen Anschluss zum Einstellen der Fahrtrichtung angezeigt werden.
#[derive(Debug, Clone, Copy)]
pub enum FahrtrichtungAnschluss {
    /// Nur für [`Geschwindigkeiten`](Geschwindigkeit) die über ein Pwm-Signal gesteuert werden.
    Pwm,
    /// Nur für [`Geschwindigkeiten`](Geschwindigkeit) die über mehrere Anschlüsse
    /// mit konstanter Spannung gesteuert werden.
    KonstanteSpannung,
    /// Bei allen [`Geschwindigkeiten`](Geschwindigkeit), unabhängig davon wie sie gesteuert werden.
    Immer,
}

impl<'t, LeiterSerialisiert, R> Auswahl<'t, LeiterSerialisiert, R>
where
    LeiterSerialisiert: 't + Display + Clone,
    R: 't + text_core::Renderer<Font = Font>,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + radio::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + card::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
{
    // Alle Argumente benötigt.
    #[allow(clippy::too_many_arguments)]
    /// Erstelle eine neue [`Auswahl`].
    fn neu<'l, L: LeiterAnzeige<'l, LeiterSerialisiert, R>>(
        startwert: Option<(Name, GeschwindigkeitSerialisiert<LeiterSerialisiert>)>,
        geschwindigkeiten: BTreeMap<Name, GeschwindigkeitSerialisiert<LeiterSerialisiert>>,
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        fahrtrichtung_beschreibung: impl Into<String>,
        pwm_nachricht: &'t impl Fn(
            OutputSerialisiert,
            pwm::Serialisiert,
            Polarität,
        ) -> LeiterSerialisiert,
        ks_nachricht: &'t impl Fn(
            OutputSerialisiert,
            NonEmpty<OutputSerialisiert>,
        ) -> LeiterSerialisiert,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let fahrtrichtung_beschreibung = fahrtrichtung_beschreibung.into();
        let auswahl_startwert = startwert.map(|(name, start_geschwindigkeit)| {
            (
                name,
                <L as LeiterAnzeige<'l, LeiterSerialisiert, R>>::auswahl_startwert(
                    start_geschwindigkeit,
                ),
            )
        });
        let erzeuge_zustand =
            move || AuswahlZustand::neu(auswahl_startwert.clone(), geschwindigkeiten.iter());
        let erzeuge_element = move |zustand: &AuswahlZustand<LeiterSerialisiert>| {
            Self::erzeuge_element::<L>(
                zustand,
                fahrtrichtung_anschluss,
                &fahrtrichtung_beschreibung,
                scrollable_style,
                settings,
            )
        };
        let mapper = Self::mapper(pwm_nachricht, ks_nachricht);
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    /// Konvertiere eine [`InterneAuswahlNachricht`] in eine [`AuswahlNachricht`].
    fn mapper(
        pwm_nachricht: &'t impl Fn(
            OutputSerialisiert,
            pwm::Serialisiert,
            Polarität,
        ) -> LeiterSerialisiert,
        ks_nachricht: &'t impl Fn(
            OutputSerialisiert,
            NonEmpty<OutputSerialisiert>,
        ) -> LeiterSerialisiert,
    ) -> impl 't
           + Fn(
        InterneAuswahlNachricht,
        &mut (dyn DerefMut<Target = AuswahlZustand<LeiterSerialisiert>>),
        &mut event::Status,
    ) -> Vec<AuswahlNachricht<LeiterSerialisiert>> {
        |interne_nachricht, zustand, status| {
            *status = event::Status::Captured;
            let mut nachrichten = Vec::new();
            match interne_nachricht {
                InterneAuswahlNachricht::Schließen => {
                    nachrichten.push(AuswahlNachricht::Schließen);
                },
                InterneAuswahlNachricht::WähleTab(tab) => {
                    zustand.aktueller_tab = tab;
                },
                InterneAuswahlNachricht::Name(name) => {
                    zustand.neu_name = name;
                },
                InterneAuswahlNachricht::UmdrehenAnschluss(anschluss) => {
                    zustand.umdrehen_anschluss = anschluss;
                },
                InterneAuswahlNachricht::PwmPin(pin) => {
                    zustand.pwm_pin = pin;
                },
                InterneAuswahlNachricht::PwmPolarität(polarität) => {
                    zustand.pwm_polarität = polarität;
                },
                InterneAuswahlNachricht::KonstanteSpannungAnschluss(ix, anschluss_neu) => {
                    if let Some(anschluss) = zustand.ks_anschlüsse.get_mut(ix) {
                        *anschluss = anschluss_neu;
                    } else {
                        error!(
                            "Update-Nachricht für Anschluss {ix}, es gibt aber nur {}!",
                            zustand.ks_anschlüsse.len()
                        );
                    }
                },
                InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss => {
                    zustand
                        .ks_anschlüsse
                        .push(OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal });
                },
                InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix) => {
                    let _ = remove_from_nonempty_tail(&mut zustand.ks_anschlüsse, ix);
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    let leiter = match zustand.aktueller_tab {
                        TabId::Pwm => pwm_nachricht(
                            zustand.umdrehen_anschluss.clone(),
                            zustand.pwm_pin.clone(),
                            zustand.pwm_polarität,
                        ),
                        TabId::KonstanteSpannung => {
                            let NonEmpty { head, tail } = &zustand.ks_anschlüsse;
                            ks_nachricht(
                                zustand.umdrehen_anschluss.clone(),
                                NonEmpty {
                                    head: (*head).clone(),
                                    tail: tail
                                        .iter()
                                        .map(|anschluss| (*anschluss).clone())
                                        .collect(),
                                },
                            )
                        },
                    };
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        Name(zustand.neu_name.clone()),
                        GeschwindigkeitSerialisiert { leiter },
                    );
                    nachrichten.push(nachricht);
                },
                InterneAuswahlNachricht::Löschen(name) => {
                    nachrichten.push(AuswahlNachricht::Löschen(name));
                },
                InterneAuswahlNachricht::Bearbeiten(name, startwert) => {
                    zustand.neu_name = name.0;
                    match startwert {
                        AuswahlStartwert::Pwm { umdrehen_anschluss, pwm_pin, polarität } => {
                            zustand.aktueller_tab = TabId::Pwm;
                            if let Some(umdrehen_anschluss) = umdrehen_anschluss {
                                zustand.umdrehen_anschluss = umdrehen_anschluss;
                            }
                            zustand.pwm_pin = pwm_pin;
                            zustand.pwm_polarität = polarität;
                        },
                        AuswahlStartwert::KonstanteSpannung {
                            umdrehen_anschluss,
                            geschwindigkeit_anschlüsse,
                        } => {
                            zustand.aktueller_tab = TabId::KonstanteSpannung;
                            if let Some(umdrehen_anschluss) = umdrehen_anschluss {
                                zustand.umdrehen_anschluss = umdrehen_anschluss;
                            }
                            zustand.ks_anschlüsse = geschwindigkeit_anschlüsse;
                        },
                    };
                },
            }
            nachrichten
        }
    }

    /// Erzeuge die Anschluss-Auswahl zum Einstellen der Fahrtrichtung.
    fn umdrehen_auswahl(
        fahrtrichtung_beschreibung: &str,
        scrollable_style: Sammlung,
        settings: I2cSettings,
        umdrehen_anschluss: &OutputSerialisiert,
    ) -> Element<'t, InterneAuswahlNachricht, R> {
        Column::new()
            .push(Text::new(fahrtrichtung_beschreibung.to_owned()))
            .push(
                Element::from(anschluss::Auswahl::neu_output_s(
                    Some(umdrehen_anschluss),
                    scrollable_style,
                    settings,
                ))
                .map(InterneAuswahlNachricht::UmdrehenAnschluss),
            )
            .into()
    }

    /// Erzeuge die Widgets für die Pin-Auswahl zu Steuerung über ein Pwm-Signal.
    fn pwm_auswahl(
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        umdrehen_auswahl: impl FnOnce() -> Element<'t, InterneAuswahlNachricht, R>,
        pwm_pin: &pwm::Serialisiert,
        pwm_polarität: Polarität,
    ) -> Element<'t, InterneAuswahlNachricht, R> {
        let make_radio = |polarität: Polarität| {
            Radio::new(
                polarität.to_string(),
                polarität,
                Some(pwm_polarität),
                InterneAuswahlNachricht::PwmPolarität,
            )
        };
        let mut pwm_auswahl = Row::new();
        if let FahrtrichtungAnschluss::Pwm = fahrtrichtung_anschluss {
            pwm_auswahl = pwm_auswahl.push(umdrehen_auswahl());
        }
        pwm_auswahl = pwm_auswahl
            .push(
                Element::from(anschluss::Pwm::neu_s(Some(pwm_pin.clone())))
                    .map(InterneAuswahlNachricht::PwmPin),
            )
            .push(
                Column::new()
                    .push(make_radio(Polarität::Normal))
                    .push(make_radio(Polarität::Invertiert)),
            );
        pwm_auswahl.into()
    }

    /// Erzeuge die Widgets für die Anschlüsse-Auswahl zur Steuerung über konstante Spannungswerte.
    fn ks_auswahl(
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        umdrehen_auswahl: impl FnOnce() -> Element<'t, InterneAuswahlNachricht, R>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
        ks_anschlüsse: &NonEmpty<OutputSerialisiert>,
    ) -> Element<'t, InterneAuswahlNachricht, R> {
        let mut ks_auswahl = Column::new().height(Length::Shrink);
        if let FahrtrichtungAnschluss::KonstanteSpannung = fahrtrichtung_anschluss {
            ks_auswahl = ks_auswahl.push(umdrehen_auswahl());
        }
        ks_auswahl = ks_auswahl
            .push(Space::with_height(Length::Fixed(1.)))
            .push(Text::new("Geschwindigkeit"));
        for (i, ks_anschluss) in ks_anschlüsse.iter().enumerate() {
            // FIXME neu hinzugefügte Anschlüsse werden erst nach Ändern der Fenstergröße angezeigt
            // (davor wird nur das scrollable größer)
            let mut row = Row::new().height(Length::Shrink).push(
                Element::from(anschluss::Auswahl::neu_output_s(
                    Some(ks_anschluss),
                    scrollable_style,
                    settings,
                ))
                .map(move |anschluss| {
                    InterneAuswahlNachricht::KonstanteSpannungAnschluss(i, anschluss)
                }),
            );
            row = row.push(if let Some(ix) = NonZeroUsize::new(i) {
                Element::new(
                    Button::new(Text::new("X"))
                        .on_press(InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix)),
                )
            } else {
                Element::new(
                    Button::new(Text::new("+"))
                        .on_press(InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss),
                )
            });
            row = row.push(Space::new(Length::Fixed(7.5), Length::Shrink));
            ks_auswahl = ks_auswahl.push(row);
        }
        ks_auswahl.into()
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Auswahl`]-Widget.
    fn erzeuge_element<'l, L: LeiterAnzeige<'l, LeiterSerialisiert, R>>(
        zustand: &AuswahlZustand<LeiterSerialisiert>,
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        fahrtrichtung_beschreibung: &str,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'t, InterneAuswahlNachricht, R> {
        let AuswahlZustand {
            neu_name,
            aktueller_tab,
            umdrehen_anschluss,
            pwm_pin,
            pwm_polarität,
            ks_anschlüsse,
            geschwindigkeiten,
        } = zustand;
        let width = Length::Fixed(950.);
        let mut neuer_anschluss = Column::new().push(
            TextInput::new("<Name>", neu_name).on_input(InterneAuswahlNachricht::Name).width(width),
        );
        let erzeuge_umdrehen_auswahl = || {
            Self::umdrehen_auswahl(
                fahrtrichtung_beschreibung,
                scrollable_style,
                settings,
                umdrehen_anschluss,
            )
        };
        if let FahrtrichtungAnschluss::Immer = fahrtrichtung_anschluss {
            neuer_anschluss = neuer_anschluss.push(erzeuge_umdrehen_auswahl());
        }
        let pwm_auswahl = Self::pwm_auswahl(
            fahrtrichtung_anschluss,
            erzeuge_umdrehen_auswahl,
            pwm_pin,
            *pwm_polarität,
        );
        let ks_auswahl = Self::ks_auswahl(
            fahrtrichtung_anschluss,
            erzeuge_umdrehen_auswahl,
            scrollable_style,
            settings,
            ks_anschlüsse,
        );
        let tabs = Tabs::with_tabs(
            vec![
                (TabId::Pwm, TabLabel::Text("Pwm".to_owned()), pwm_auswahl),
                (
                    TabId::KonstanteSpannung,
                    TabLabel::Text("Konstante Spannung".to_owned()),
                    Scrollable::new(ks_auswahl).height(Length::Fixed(150.)).into(),
                ),
            ],
            InterneAuswahlNachricht::WähleTab,
        )
        .set_active_tab(aktueller_tab)
        .width(width)
        .height(Length::Shrink)
        .tab_bar_style(TabBar.into());
        let neuer_anschluss = neuer_anschluss.push(tabs);
        let mut column = Column::new().push(neuer_anschluss).push(
            Button::new(Text::new("Hinzufügen")).on_press(InterneAuswahlNachricht::Hinzufügen),
        );
        for (name, (anschlüsse_str, anschlüsse)) in geschwindigkeiten {
            let bearbeiten = Button::new(Icon::neu(Bootstrap::Feather)).on_press(
                InterneAuswahlNachricht::Bearbeiten(
                    name.clone().into_inner(),
                    <L as LeiterAnzeige<'l, LeiterSerialisiert, R>>::auswahl_startwert(
                        anschlüsse.clone(),
                    ),
                ),
            );
            let löschen = Button::new(Text::new("X"))
                .on_press(InterneAuswahlNachricht::Löschen(name.clone().into_inner()));
            let geschwindigkeit = Column::new()
                .push(Text::new(name.as_ref().to_owned()))
                .push(Text::new(anschlüsse_str.clone()))
                .push(Row::new().push(bearbeiten).push(löschen));
            column = column.push(geschwindigkeit);
        }
        let card = Card::new(
            Text::new("Geschwindigkeit"),
            Scrollable::new(column).height(Length::Fixed(400.)).width(Length::Shrink),
        )
        .on_close(InterneAuswahlNachricht::Schließen)
        .width(Length::Shrink);
        card.into()
    }
}

impl<'t, LeiterSerialisiert, R> From<Auswahl<'t, LeiterSerialisiert, R>>
    for Element<'t, AuswahlNachricht<LeiterSerialisiert>, R>
where
    LeiterSerialisiert: 'static + PartialEq,
    R: 't + text_core::Renderer<Font = Font>,
{
    fn from(anzeige: Auswahl<'t, LeiterSerialisiert, R>) -> Self {
        Element::new(anzeige.0)
    }
}

/// Ermöglicht Erstellen und Anpassen einer [`Geschwindigkeit`] mit dieser Leiter-Art.
pub trait LeiterAnzeige<'t, S, R>: Leiter + Sized {
    /// Erstelle eine neue [`Anzeige`].
    fn anzeige_neu(
        name: &Name,
        geschwindigkeit: &Geschwindigkeit<Self>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R>;

    /// Erstelle eine neue [`Auswahl`].
    fn auswahl_neu(
        startwert: Option<(Name, GeschwindigkeitSerialisiert<S>)>,
        geschwindigkeiten: BTreeMap<Name, GeschwindigkeitSerialisiert<S>>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Auswahl<'t, S, R>;

    /// Erzeuge den [Startwert](AuswahlStartwert) für ein [`Auswahl`]-Widget.
    fn auswahl_startwert(serialisiert: GeschwindigkeitSerialisiert<S>) -> AuswahlStartwert;
}

/// Zurücksetzen des Zustands des [`Anzeige`]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenMittelleiter {
    /// Die Geschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
}

impl<'t, R> LeiterAnzeige<'t, MittelleiterSerialisiert, R> for Mittelleiter
where
    R: 't + text_core::Renderer<Font = Font>,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + radio::StyleSheet
        + slider::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + card::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
{
    fn anzeige_neu(
        name: &Name,
        geschwindigkeit: &Geschwindigkeit<Mittelleiter>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R> {
        let zeige_fahrtrichtung = |_none| {
            Button::new(Text::new("Umdrehen"))
                .on_press(AktionGeschwindigkeit::Umdrehen {
                    geschwindigkeit: geschwindigkeit.clone(),
                })
                .into()
        };
        let clone = geschwindigkeit.clone();
        Anzeige::neu(
            name,
            geschwindigkeit,
            Geschwindigkeit::<Mittelleiter>::ks_länge,
            move |wert| AktionGeschwindigkeit::Geschwindigkeit {
                geschwindigkeit: clone.clone(),
                wert,
            },
            zeige_fahrtrichtung,
        )
    }

    fn auswahl_neu(
        startwert: Option<(Name, GeschwindigkeitSerialisiert<MittelleiterSerialisiert>)>,
        geschwindigkeiten: BTreeMap<Name, GeschwindigkeitSerialisiert<MittelleiterSerialisiert>>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Auswahl<'t, MittelleiterSerialisiert, R> {
        Auswahl::neu::<Self>(
            startwert,
            geschwindigkeiten,
            FahrtrichtungAnschluss::KonstanteSpannung,
            "Umdrehen",
            &|_umdrehen, pin, polarität| MittelleiterSerialisiert::Pwm { pin, polarität },
            &|umdrehen, geschwindigkeit| MittelleiterSerialisiert::KonstanteSpannung {
                geschwindigkeit,
                umdrehen,
            },
            scrollable_style,
            settings,
        )
    }

    fn auswahl_startwert(
        serialisiert: GeschwindigkeitSerialisiert<MittelleiterSerialisiert>,
    ) -> AuswahlStartwert {
        match serialisiert.leiter {
            MittelleiterSerialisiert::Pwm { pin, polarität } => {
                AuswahlStartwert::Pwm { umdrehen_anschluss: None, pwm_pin: pin, polarität }
            },
            MittelleiterSerialisiert::KonstanteSpannung { geschwindigkeit, umdrehen } => {
                AuswahlStartwert::KonstanteSpannung {
                    umdrehen_anschluss: Some(umdrehen),
                    geschwindigkeit_anschlüsse: geschwindigkeit,
                }
            },
        }
    }
}

/// Zurücksetzen des Zustands des [`Anzeige`]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenZweileiter {
    /// Die Fahrgeschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
    /// Die Fahrtrichtung vor der async-Aktion.
    pub bisherige_fahrtrichtung: Fahrtrichtung,
}

impl<'t, R: 't> LeiterAnzeige<'t, ZweileiterSerialisiert, R> for Zweileiter
where
    R: text_core::Renderer<Font = Font>,
    <R as Renderer>::Theme: container::StyleSheet
        + button::StyleSheet
        + scrollable::StyleSheet
        + radio::StyleSheet
        + slider::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + card::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
{
    fn anzeige_neu(
        name: &Name,
        geschwindigkeit: &Geschwindigkeit<Zweileiter>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R> {
        let geschwindigkeit_radio_clone = geschwindigkeit.clone();
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung, aktuell: &Fahrtrichtung| {
            Radio::new(
                fahrtrichtung.to_string(),
                fahrtrichtung,
                Some(*aktuell),
                move |gewählte_fahrtrichtung| AktionGeschwindigkeit::Fahrtrichtung {
                    geschwindigkeit: geschwindigkeit_radio_clone.clone(),
                    fahrtrichtung: gewählte_fahrtrichtung,
                },
            )
        };
        let zeige_fahrtrichtung = |fahrtrichtung: Option<Fahrtrichtung>| {
            let fahrtrichtung = fahrtrichtung.unwrap_or(Fahrtrichtung::Vorwärts);
            Row::new()
                .push((fahrtrichtung_radio.clone())(Fahrtrichtung::Vorwärts, &fahrtrichtung))
                .push(fahrtrichtung_radio(Fahrtrichtung::Rückwärts, &fahrtrichtung))
                .into()
        };
        let geschwindigkeit_clone = geschwindigkeit.clone();
        Anzeige::neu(
            name,
            geschwindigkeit,
            Geschwindigkeit::<Zweileiter>::ks_länge,
            move |wert| AktionGeschwindigkeit::Geschwindigkeit {
                geschwindigkeit: geschwindigkeit_clone.clone(),
                wert,
            },
            zeige_fahrtrichtung,
        )
    }

    fn auswahl_neu(
        startwert: Option<(Name, GeschwindigkeitSerialisiert<ZweileiterSerialisiert>)>,
        geschwindigkeiten: BTreeMap<Name, GeschwindigkeitSerialisiert<ZweileiterSerialisiert>>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Auswahl<'t, ZweileiterSerialisiert, R> {
        Auswahl::neu::<Self>(
            startwert,
            geschwindigkeiten,
            FahrtrichtungAnschluss::Immer,
            "Fahrtrichtung",
            &|fahrtrichtung, geschwindigkeit, polarität| ZweileiterSerialisiert::Pwm {
                geschwindigkeit,
                polarität,
                fahrtrichtung,
            },
            &|fahrtrichtung, geschwindigkeit| ZweileiterSerialisiert::KonstanteSpannung {
                geschwindigkeit,
                fahrtrichtung,
            },
            scrollable_style,
            settings,
        )
    }

    fn auswahl_startwert(
        serialisiert: GeschwindigkeitSerialisiert<ZweileiterSerialisiert>,
    ) -> AuswahlStartwert {
        match serialisiert.leiter {
            ZweileiterSerialisiert::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                AuswahlStartwert::Pwm {
                    umdrehen_anschluss: Some(fahrtrichtung),
                    pwm_pin: geschwindigkeit,
                    polarität,
                }
            },
            ZweileiterSerialisiert::KonstanteSpannung { geschwindigkeit, fahrtrichtung } => {
                AuswahlStartwert::KonstanteSpannung {
                    umdrehen_anschluss: Some(fahrtrichtung),
                    geschwindigkeit_anschlüsse: geschwindigkeit,
                }
            },
        }
    }
}
