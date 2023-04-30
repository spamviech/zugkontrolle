//! Anzeige und Erstellen einer [Geschwindigkeit].

use std::{
    collections::BTreeMap,
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
use iced_native::{
    event,
    widget::{
        button::{self, Button},
        container,
        radio::{self, Radio},
        scrollable::{self, Scrollable},
        slider::{self, Slider},
        text::{self, Text},
        text_input::{self, TextInput},
        Column, Row,
    },
    Element, Font, Length, Renderer,
};
use log::error;
use nonempty::NonEmpty;

pub use crate::steuerung::geschwindigkeit::{Geschwindigkeit, Name};
use crate::{
    anschluss::{pin::pwm, polarität::Polarität, OutputSerialisiert},
    application::{anschluss, map_mit_zustand::MapMitZustand, style::tab_bar::TabBar},
    eingeschränkt::NichtNegativ,
    steuerung::{
        geschwindigkeit::{
            Fahrtrichtung, GeschwindigkeitSerialisiert, Leiter, Mittelleiter,
            MittelleiterSerialisiert, Zweileiter, ZweileiterSerialisiert,
        },
        plan::AktionGeschwindigkeit,
    },
    unicase_ord::UniCaseOrd,
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
pub type Map<L> = BTreeMap<Name, AnzeigeZustand<L>>;

/// Zustand des Widgets zur Anzeige einer [Geschwindigkeit].
#[derive(zugkontrolle_macros::Debug)]
#[zugkontrolle_debug(<L as Leiter>::VerhältnisFahrspannungÜberspannung: Debug)]
#[zugkontrolle_debug(<L as Leiter>::UmdrehenZeit: Debug)]
pub struct AnzeigeZustand<L: Leiter> {
    name: Name,
    pwm_frequenz: NichtNegativ,
    verhältnis_fahrspannung_überspannung: <L as Leiter>::VerhältnisFahrspannungÜberspannung,
    stopp_zeit: Duration,
    umdrehen_zeit: <L as Leiter>::UmdrehenZeit,
}

impl<L: Leiter> AnzeigeZustand<L> {
    /// Erstelle einen neuen [AnzeigeZustand].
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

/// Anzeige und Steuerung einer [Geschwindigkeit].
pub struct Anzeige<'t, M, R> {
    element: Element<'t, M, R>,
}

impl<M, R> Debug for Anzeige<'_, M, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Anzeige").field("element", &"<Element>").finish()
    }
}

impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 't + Clone,
    R: 't + iced_native::text::Renderer,
    <R as Renderer>::Theme: radio::StyleSheet + slider::StyleSheet + text::StyleSheet,
{
    /// Erstelle eine neue [Anzeige] für einen [Leiter].
    pub fn neu<'s, L: Leiter>(
        name: &'t Name,
        geschwindigkeit: &'s Geschwindigkeit<L>,
        ks_länge: impl FnOnce(&'s Geschwindigkeit<L>) -> Option<usize>,
        geschwindigkeit_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(Option<<L as Leiter>::Fahrtrichtung>) -> Element<'t, M, R>,
        // TODO overlay mit Anschlüssen?
    ) -> Self {
        let aktuelle_geschwindigkeit = geschwindigkeit.aktuelle_geschwindigkeit();
        let aktuelle_fahrtrichtung = geschwindigkeit.aktuelle_fahrtrichtung();
        // TODO Anschluss-Anzeige (Expander über Overlay?)
        let mut column = Column::new().spacing(1).push(Text::new(&name.0));
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

#[derive(Debug)]
enum KonstanteSpannungAnpassen {
    Hinzufügen,
    Entfernen(NonZeroUsize),
}

/// Zustand für das Auswahl-Fenster zum Erstellen und Anpassen einer [Geschwindigkeit].
#[derive(Debug)]
struct AuswahlZustand {
    neu_name: String,
    aktueller_tab: usize,
    umdrehen_anschluss: OutputSerialisiert,
    pwm_pin: pwm::Serialisiert,
    pwm_polarität: Polarität,
    ks_anschlüsse: NonEmpty<OutputSerialisiert>,
    geschwindigkeiten: BTreeMap<UniCaseOrd<Name>, String>,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [AuswahlZustand].
    fn neu<'t, Leiter: 't + Display>(
        geschwindigkeiten: impl Iterator<Item = (&'t Name, &'t Geschwindigkeit<Leiter>)>,
    ) -> Self {
        AuswahlZustand {
            neu_name: String::new(),
            aktueller_tab: 0,
            umdrehen_anschluss: OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal },
            pwm_pin: pwm::Serialisiert(0),
            pwm_polarität: Polarität::Normal,
            ks_anschlüsse: NonEmpty::singleton(OutputSerialisiert::Pin {
                pin: 0,
                polarität: Polarität::Normal,
            }),
            geschwindigkeiten: geschwindigkeiten.map(Self::iter_map).collect(),
        }
    }

    fn iter_map<'t, Leiter: 't + Display>(
        (name, geschwindigkeit): (&'t Name, &'t Geschwindigkeit<Leiter>),
    ) -> (UniCaseOrd<Name>, String) {
        (UniCaseOrd::neu(name.clone()), geschwindigkeit.to_string())
    }

    /// Füge eine neue [Geschwindigkeit] hinzu.
    fn hinzufügen<Leiter: Display>(
        &mut self,
        name: &Name,
        geschwindigkeit: &Geschwindigkeit<Leiter>,
    ) {
        let (key, value) = Self::iter_map((name, geschwindigkeit));
        let _ = self.geschwindigkeiten.insert(key, value);
    }

    /// Entferne eine [Geschwindigkeit].
    fn entfernen(&mut self, name: &Name) {
        let _ = self.geschwindigkeiten.remove(&UniCaseOrd::neu(name.clone()));
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
#[derive(Debug, Clone)]
pub enum AuswahlNachricht<LeiterSerialisiert> {
    /// Schließe das Auswahl-Fenster.
    Schließen,
    /// Füge eine neue [Geschwindigkeit] hinzu.
    Hinzufügen(Name, GeschwindigkeitSerialisiert<LeiterSerialisiert>),
    /// Lösche eine [Geschwindigkeit].
    Löschen(Name),
}

/// Hinzufügen und Anpassen einer [Geschwindigkeit].
#[derive(Debug)]
pub struct Auswahl<'t, LeiterSerialisiert, R>(
    MapMitZustand<
        't,
        AuswahlZustand,
        InterneAuswahlNachricht,
        AuswahlNachricht<LeiterSerialisiert>,
        R,
    >,
);

/// Wo soll eine Auswahl für einen Anschluss zum Einstellen der Fahrtrichtung angezeigt werden.
#[derive(Debug, Clone, Copy)]
enum FahrtrichtungAnschluss {
    /// Nur für [Geschwindigkeiten](Geschwindigkeit) die über ein Pwm-Signal gesteuert werden.
    Pwm,
    /// Nur für [Geschwindigkeiten](Geschwindigkeit) die über mehrere Anschlüsse
    /// mit konstanter Spannung gesteuert werden.
    KonstanteSpannung,
    /// Bei allen [Geschwindigkeiten](Geschwindigkeit), unabhängig davon wie sie gesteuert werden.
    Immer,
}

impl<'t, LeiterSerialisiert, R> Auswahl<'t, LeiterSerialisiert, R>
where
    R: 't + iced_native::text::Renderer<Font = Font>,
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
{
    /// Erstelle eine neue [Auswahl].
    pub fn neu<Leiter: Display>(
        geschwindigkeiten: &'t BTreeMap<Name, Geschwindigkeit<Leiter>>,
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
    ) -> Self {
        let fahrtrichtung_beschreibung = fahrtrichtung_beschreibung.into();
        let erzeuge_zustand = || AuswahlZustand::neu(geschwindigkeiten.iter());
        let erzeuge_element = move |zustand: &AuswahlZustand| {
            Self::erzeuge_element(zustand, fahrtrichtung_anschluss, &fahrtrichtung_beschreibung)
        };
        let mapper = |interne_nachricht,
                      zustand: &mut dyn DerefMut<Target = AuswahlZustand>,
                      status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneAuswahlNachricht::Schließen => vec![AuswahlNachricht::Schließen],
                InterneAuswahlNachricht::WähleTab(tab) => {
                    zustand.aktueller_tab = tab;
                    Vec::new()
                },
                InterneAuswahlNachricht::Name(name) => {
                    zustand.neu_name = name;
                    Vec::new()
                },
                InterneAuswahlNachricht::UmdrehenAnschluss(anschluss) => {
                    zustand.umdrehen_anschluss = anschluss;
                    Vec::new()
                },
                InterneAuswahlNachricht::PwmPin(pin) => {
                    zustand.pwm_pin = pin;
                    Vec::new()
                },
                InterneAuswahlNachricht::PwmPolarität(polarität) => {
                    zustand.pwm_polarität = polarität;
                    Vec::new()
                },
                InterneAuswahlNachricht::KonstanteSpannungAnschluss(ix, anschluss_neu) => {
                    if let Some(anschluss) = zustand.ks_anschlüsse.get_mut(ix) {
                        *anschluss = anschluss_neu;
                    } else {
                        error!(
                            "Update-Nachricht für Anschluss {}, es gibt aber nur {}!",
                            ix,
                            zustand.ks_anschlüsse.len()
                        )
                    }
                    Vec::new()
                },
                InterneAuswahlNachricht::NeuerKonstanteSpannungAnschluss => {
                    zustand
                        .ks_anschlüsse
                        .push(OutputSerialisiert::Pin { pin: 0, polarität: Polarität::Normal });
                    Vec::new()
                },
                InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix) => {
                    let _ = remove_from_nonempty_tail(&mut zustand.ks_anschlüsse, ix);
                    Vec::new()
                },
                InterneAuswahlNachricht::Hinzufügen => {
                    let leiter = if zustand.aktueller_tab == 0 {
                        pwm_nachricht(
                            zustand.umdrehen_anschluss.clone(),
                            zustand.pwm_pin.clone(),
                            zustand.pwm_polarität,
                        )
                    } else {
                        let NonEmpty { head, tail } = &zustand.ks_anschlüsse;
                        ks_nachricht(
                            zustand.umdrehen_anschluss.clone(),
                            NonEmpty {
                                head: (*head).clone(),
                                tail: tail.iter().map(|anschluss| (*anschluss).clone()).collect(),
                            },
                        )
                    };
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        Name(zustand.neu_name.clone()),
                        GeschwindigkeitSerialisiert { leiter },
                    );
                    vec![nachricht]
                },
                InterneAuswahlNachricht::Löschen(name) => {
                    vec![AuswahlNachricht::Löschen(name)]
                },
            }
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }

    fn erzeuge_element(
        zustand: &AuswahlZustand,
        fahrtrichtung_anschluss: FahrtrichtungAnschluss,
        fahrtrichtung_beschreibung: &str,
    ) -> Element<'t, InterneAuswahlNachricht, R> {
        let AuswahlZustand {
            neu_name,
            aktueller_tab,
            umdrehen_anschluss: _,
            pwm_pin: _,
            pwm_polarität,
            ks_anschlüsse,
            geschwindigkeiten,
        } = zustand;
        let output_save_head = &ks_anschlüsse.head;
        let anschlüsse_save_tail: Vec<_> = ks_anschlüsse.tail.iter().collect();
        let anschlüsse_save = NonEmpty { head: output_save_head, tail: anschlüsse_save_tail };
        let width = Length::Fixed(950.);
        let mut neuer_anschluss = Column::new().push(
            TextInput::new("<Name>", neu_name).on_input(InterneAuswahlNachricht::Name).width(width),
        );
        let umdrehen_auswahl =
            Column::new().push(Text::new(fahrtrichtung_beschreibung.to_owned())).push(
                Element::from(anschluss::Auswahl::neu_output(None))
                    .map(InterneAuswahlNachricht::UmdrehenAnschluss),
            );
        let make_radio = |polarität: Polarität| {
            Radio::new(
                polarität.to_string(),
                polarität,
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
            FahrtrichtungAnschluss::Immer => {
                neuer_anschluss = neuer_anschluss.push(umdrehen_auswahl)
            },
        }
        let pwm_auswahl = pwm_auswahl
            .push(Element::from(anschluss::Pwm::neu(None)).map(InterneAuswahlNachricht::PwmPin))
            .push(
                Column::new()
                    .push(make_radio(Polarität::Normal))
                    .push(make_radio(Polarität::Invertiert)),
            );
        let mut ks_column = Column::new().height(Length::Fixed(150.));
        for i in 0..anschlüsse_save.len() {
            let ii = i;
            let mut row = Row::new().push(Element::from(anschluss::Auswahl::neu_output(None)).map(
                move |anschluss| InterneAuswahlNachricht::KonstanteSpannungAnschluss(ii, anschluss),
            ));
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
            ks_column = ks_column.push(row)
        }
        let ks_auswahl = ks_auswahl.push(Scrollable::new(ks_column));
        let tabs = Tabs::new(*aktueller_tab, InterneAuswahlNachricht::WähleTab)
            .push(TabLabel::Text("Pwm".to_owned()), pwm_auswahl)
            .push(TabLabel::Text("Konstante Spannung".to_owned()), ks_auswahl)
            .width(width)
            .height(Length::Shrink)
            .tab_bar_style(TabBar.into());
        let neuer_anschluss = neuer_anschluss.push(tabs);
        let mut column = Column::new().push(neuer_anschluss).push(
            Button::new(Text::new("Hinzufügen")).on_press(InterneAuswahlNachricht::Hinzufügen),
        );
        for (name, anschlüsse) in geschwindigkeiten.iter() {
            let button = Button::new(Text::new("X"))
                .on_press(InterneAuswahlNachricht::Löschen(name.clone().into_inner()));
            let geschwindigkeit = Column::new()
                .push(Text::new(name.as_ref().to_owned()))
                .push(Text::new(anschlüsse.clone()))
                .push(button);
            column = column.push(geschwindigkeit);
        }
        let card = Card::new(Text::new("Geschwindigkeit"), Scrollable::new(column))
            .on_close(InterneAuswahlNachricht::Schließen)
            .width(Length::Shrink);
        card.into()
    }
}

impl<'t, LeiterSerialisiert, R> From<Auswahl<'t, LeiterSerialisiert, R>>
    for Element<'t, AuswahlNachricht<LeiterSerialisiert>, R>
where
    LeiterSerialisiert: 't,
    R: 't + iced_native::text::Renderer<Font = Font>,
{
    fn from(anzeige: Auswahl<'t, LeiterSerialisiert, R>) -> Self {
        Element::new(anzeige.0)
    }
}

/// Ermöglicht Erstellen und Anpassen einer [Geschwindigkeit] mit dieser Leiter-Art.
pub trait LeiterAnzeige<'t, S, R>: Leiter + Sized {
    /// Erstelle eine neue [Anzeige].
    fn anzeige_neu(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Self>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R>;

    /// Erstelle eine neue [Auswahl].
    fn auswahl_neu(
        geschwindigkeiten: &'t BTreeMap<Name, Geschwindigkeit<Self>>,
    ) -> Auswahl<'t, S, R>;
}

/// Zurücksetzen des Zustands des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenMittelleiter {
    /// Die Geschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
}

impl<'t, R> LeiterAnzeige<'t, MittelleiterSerialisiert, R> for Mittelleiter
where
    R: 't + iced_native::text::Renderer<Font = Font>,
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
{
    fn anzeige_neu(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Mittelleiter>,
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

    #[inline(always)]
    fn auswahl_neu(
        geschwindigkeiten: &'t BTreeMap<Name, Geschwindigkeit<Mittelleiter>>,
    ) -> Auswahl<'t, MittelleiterSerialisiert, R> {
        Auswahl::neu(
            geschwindigkeiten,
            FahrtrichtungAnschluss::KonstanteSpannung,
            "Umdrehen",
            &|_umdrehen, pin, polarität| MittelleiterSerialisiert::Pwm { pin, polarität },
            &|umdrehen, geschwindigkeit| MittelleiterSerialisiert::KonstanteSpannung {
                geschwindigkeit,
                umdrehen,
            },
        )
    }
}

/// Zurücksetzen des Zustands des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenZweileiter {
    /// Die Fahrgeschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
    /// Die Fahrtrichtung vor der async-Aktion.
    pub bisherige_fahrtrichtung: Fahrtrichtung,
}

impl<'t, R: 't> LeiterAnzeige<'t, ZweileiterSerialisiert, R> for Zweileiter
where
    R: iced_native::text::Renderer<Font = Font>,
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
{
    fn anzeige_neu(
        name: &'t Name,
        geschwindigkeit: &'t Geschwindigkeit<Zweileiter>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R> {
        let clone = geschwindigkeit.clone();
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung, aktuell: &Fahrtrichtung| {
            Radio::new(
                fahrtrichtung.to_string(),
                fahrtrichtung,
                Some(*aktuell),
                move |fahrtrichtung| AktionGeschwindigkeit::Fahrtrichtung {
                    geschwindigkeit: clone.clone(),
                    fahrtrichtung,
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
        let clone = geschwindigkeit.clone();
        Anzeige::neu(
            name,
            geschwindigkeit,
            Geschwindigkeit::<Zweileiter>::ks_länge,
            move |wert| AktionGeschwindigkeit::Geschwindigkeit {
                geschwindigkeit: clone.clone(),
                wert,
            },
            zeige_fahrtrichtung,
        )
    }

    #[inline(always)]
    fn auswahl_neu(
        geschwindigkeiten: &'t BTreeMap<Name, Geschwindigkeit<Zweileiter>>,
    ) -> Auswahl<'t, ZweileiterSerialisiert, R> {
        Auswahl::neu(
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
        )
    }
}
