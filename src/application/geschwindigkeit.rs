//! Anzeige und Erstellen einer [Geschwindigkeit].

use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Display, Formatter},
    num::NonZeroUsize,
    time::Duration,
};

use iced_aw::pure::{Card, TabLabel, Tabs};
use iced_native::{event, text, Clipboard, Event, Font, Layout, Length, Point, Renderer, Shell};
use iced_pure::{
    overlay,
    widget::{
        button::{self, Button},
        scrollable::{self, Scrollable},
        slider::{self, Slider},
        text_input::{self, TextInput},
        Column, Radio, Row, Text,
    },
    Element, Widget,
};
use log::error;
use nonempty::NonEmpty;

pub use crate::steuerung::geschwindigkeit::{Geschwindigkeit, Name};
use crate::{
    anschluss::{pin::pwm, polarität::Polarität, OutputSerialisiert},
    application::{anschluss, macros::reexport_no_event_methods, style::tab_bar::TabBar},
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

/// Ermöglicht Erstellen und Anpassen einer [Geschwindigkeit] mit dieser Leiter-Art.
pub trait LeiterAnzeige<S>: Leiter + Sized {
    /// Erstelle einen neuen [AnzeigeZustand].
    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self>;

    /// Erstelle eine neue [Anzeige].
    fn anzeige_neu<'t, R: 't + text::Renderer>(
        geschwindigkeit: &Geschwindigkeit<Self>,
        zustand: &'t mut AnzeigeZustand<Self>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R>;

    /// Erstelle eine neue [Auswahl].
    fn auswahl_neu<'t, R: 't + text::Renderer<Font = Font>>(
        zustand: &'t mut AuswahlZustand,
    ) -> Auswahl<'t, S, R>;
}

/// Zurücksetzen des Zustands des [Anzeige]-Widgets.
#[derive(Debug, Clone, Copy)]
pub struct ZustandZurücksetzenMittelleiter {
    /// Die Geschwindigkeit vor der async-Aktion.
    pub bisherige_geschwindigkeit: u8,
}

impl LeiterAnzeige<MittelleiterSerialisiert> for Mittelleiter {
    #[inline(always)]
    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self> {
        AnzeigeZustand::neu(
            name,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    fn anzeige_neu<'t, R: 't + text::Renderer>(
        geschwindigkeit: &Geschwindigkeit<Mittelleiter>,
        zustand: &'t mut AnzeigeZustand<Self>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R> {
        let zeige_fahrtrichtung = |_none| {
            Button::new(Text::new("Umdrehen"))
                .on_press(AktionGeschwindigkeit::Umdrehen {
                    geschwindigkeit: geschwindigkeit.clone(),
                })
                .into()
        };
        let clone = geschwindigkeit.clone();
        Anzeige::neu_mit_leiter(
            geschwindigkeit,
            zustand,
            Geschwindigkeit::<Mittelleiter>::ks_länge,
            move |wert| AktionGeschwindigkeit::Geschwindigkeit {
                geschwindigkeit: clone.clone(),
                wert,
            },
            zeige_fahrtrichtung,
        )
    }

    #[inline(always)]
    fn auswahl_neu<'t, R: 't + text::Renderer<Font = Font>>(
        zustand: &'t mut AuswahlZustand,
    ) -> Auswahl<'t, MittelleiterSerialisiert, R> {
        Auswahl::neu(
            zustand,
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

impl LeiterAnzeige<ZweileiterSerialisiert> for Zweileiter {
    #[inline(always)]
    fn anzeige_zustand_neu(
        name: Name,
        pwm_frequenz: NichtNegativ,
        verhältnis_fahrspannung_überspannung: <Self as Leiter>::VerhältnisFahrspannungÜberspannung,
        stopp_zeit: Duration,
        umdrehen_zeit: <Self as Leiter>::UmdrehenZeit,
    ) -> AnzeigeZustand<Self> {
        AnzeigeZustand::neu(
            name,
            pwm_frequenz,
            verhältnis_fahrspannung_überspannung,
            stopp_zeit,
            umdrehen_zeit,
        )
    }

    fn anzeige_neu<'t, R: 't + text::Renderer>(
        geschwindigkeit: &Geschwindigkeit<Zweileiter>,
        zustand: &'t mut AnzeigeZustand<Self>,
    ) -> Anzeige<'t, AktionGeschwindigkeit<Self>, R> {
        let clone = geschwindigkeit.clone();
        let fahrtrichtung_radio = |fahrtrichtung: Fahrtrichtung, aktuell: &Fahrtrichtung| {
            Radio::new(
                fahrtrichtung,
                fahrtrichtung.to_string(),
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
        Anzeige::neu_mit_leiter(
            geschwindigkeit,
            zustand,
            Geschwindigkeit::<Zweileiter>::ks_länge,
            move |wert| AktionGeschwindigkeit::Geschwindigkeit {
                geschwindigkeit: clone.clone(),
                wert,
            },
            zeige_fahrtrichtung,
        )
    }

    #[inline(always)]
    fn auswahl_neu<'t, R: 't + text::Renderer<Font = Font>>(
        zustand: &'t mut AuswahlZustand,
    ) -> Auswahl<'t, ZweileiterSerialisiert, R> {
        Auswahl::neu(
            zustand,
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

/// Anzeige und Steuerung einer [Geschwindigkeit].
pub struct Anzeige<'t, M, R> {
    column: Column<'t, M, R>,
}

impl<M, R> Debug for Anzeige<'_, M, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Anzeige").field("column", &"<Column>").finish()
    }
}

impl<'t, M, R> Anzeige<'t, M, R>
where
    M: 't + Clone,
    R: 't + text::Renderer,
{
    /// Erstelle eine neue [Anzeige] für eine [LeiterAnzeige].
    pub fn neu_mit_leiter<'s, L: LeiterAnzeige<S>, S>(
        geschwindigkeit: &'s Geschwindigkeit<L>,
        zustand: &'t mut AnzeigeZustand<L>,
        ks_länge: impl FnOnce(&'s Geschwindigkeit<L>) -> Option<usize>,
        geschwindigkeit_nachricht: impl Fn(u8) -> M + Clone + 'static,
        zeige_fahrtrichtung: impl FnOnce(Option<<L as Leiter>::Fahrtrichtung>) -> Element<'t, M, R>,
        // TODO overlay mit Anschlüssen?
    ) -> Self {
        let AnzeigeZustand {
            name,
            pwm_slider_zustand,
            fahrtrichtung,
            pwm_frequenz: _,
            verhältnis_fahrspannung_überspannung: _,
            stopp_zeit: _,
            umdrehen_zeit: _,
        } = zustand;
        let aktuelle_geschwindigkeit = geschwindigkeit.aktuelle_geschwindigkeit();
        let aktuelle_fahrtrichtung = geschwindigkeit.aktuelle_fahrtrichtung();
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
                    .width(Length::Units(100)),
            )
        };
        column = column.push(zeige_fahrtrichtung(fahrtrichtung, aktuelle_fahrtrichtung));
        Anzeige { column }
    }
}

impl<'t, M, R: Renderer> Widget<M, R> for Anzeige<'t, M, R> {
    reexport_no_event_methods! {Column<'t, M, R>, column, M, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, M>,
    ) -> event::Status {
        self.column.on_event(
            todo!("state: Tree"),
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            shell,
        )
    }

    fn overlay(
        &mut self,
        _layout: Layout<'_>,
        _renderer: &R,
    ) -> Option<overlay::Element<'_, M, R>> {
        //TODO overlay (Expander-artige Anschlüsse-Anzeige)
        None
    }
}

impl<'t, M: 't, R: 't + Renderer> From<Anzeige<'t, M, R>> for Element<'t, M, R> {
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
    aktueller_tab: usize,
    umdrehen_anschluss: OutputSerialisiert,
    umdrehen_zustand: anschluss::Zustand<anschluss::Output>,
    pwm_pin: pwm::Serialisiert,
    pwm_polarität: Polarität,
    pwm_zustand: anschluss::PwmZustand,
    ks_anschlüsse_anpassen: Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse: NonEmpty<(OutputSerialisiert, anschluss::Zustand<anschluss::Output>)>,
    geschwindigkeiten: BTreeMap<UniCaseOrd<Name>, String>,
}

impl AuswahlZustand {
    /// Erstelle einen neuen [AuswahlZustand].
    pub fn neu<'t, Leiter: 't + Display>(
        geschwindigkeiten: impl Iterator<Item = (&'t Name, &'t Geschwindigkeit<Leiter>)>,
    ) -> Self {
        AuswahlZustand {
            neu_name: String::new(),
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
            )),
            geschwindigkeiten: geschwindigkeiten.map(Self::iter_map).collect(),
        }
    }

    fn iter_map<'t, Leiter: 't + Display>(
        (name, geschwindigkeit): (&'t Name, &'t Geschwindigkeit<Leiter>),
    ) -> (UniCaseOrd<Name>, String) {
        (UniCaseOrd::neu(name.clone()), geschwindigkeit.to_string())
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
pub struct Auswahl<'t, LeiterSerialisiert, R> {
    card: Card<'t, InterneAuswahlNachricht, R>,
    neu_name: &'t mut String,
    aktueller_tab: &'t mut usize,
    umdrehen_anschluss: &'t mut OutputSerialisiert,
    pwm_pin: &'t mut pwm::Serialisiert,
    pwm_polarität: &'t mut Polarität,
    ks_anschlüsse_anpassen: &'t mut Option<KonstanteSpannungAnpassen>,
    ks_anschlüsse: NonEmpty<&'t mut OutputSerialisiert>,
    pwm_nachricht:
        &'t dyn Fn(OutputSerialisiert, pwm::Serialisiert, Polarität) -> LeiterSerialisiert,
    ks_nachricht:
        &'t dyn Fn(OutputSerialisiert, NonEmpty<OutputSerialisiert>) -> LeiterSerialisiert,
}

impl<LeiterSerialisiert, R> Debug for Auswahl<'_, LeiterSerialisiert, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl<'t, LeiterSerialisiert, R> Auswahl<'t, LeiterSerialisiert, R>
where
    R: 't + text::Renderer<Font = Font>,
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
        ) -> LeiterSerialisiert,
        ks_nachricht: &'t impl Fn(
            OutputSerialisiert,
            NonEmpty<OutputSerialisiert>,
        ) -> LeiterSerialisiert,
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
        let mut neu = Column::new()
            .push(TextInput::new("<Name>", neu_name, InterneAuswahlNachricht::Name).width(width));
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
                    Button::new(Text::new("X"))
                        .on_press(InterneAuswahlNachricht::LöscheKonstanteSpannungAnschluss(ix)),
                )
            } else {
                Element::from(
                    Button::new(Text::new("+"))
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
            Button::new(Text::new("Hinzufügen")).on_press(InterneAuswahlNachricht::Hinzufügen),
        );
        for (name, (anschlüsse, button_zustand)) in geschwindigkeiten.iter_mut() {
            let button = Button::new(Text::new("X"))
                .on_press(InterneAuswahlNachricht::Löschen(name.clone().into_inner()));
            scrollable = scrollable.push(
                Column::new()
                    .push(Text::new(name.to_string()))
                    .push(Text::new(&*anschlüsse))
                    .push(button),
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

impl<'t, LeiterSerialisiert, R> Widget<AuswahlNachricht<LeiterSerialisiert>, R>
    for Auswahl<'t, LeiterSerialisiert, R>
where
    R: text::Renderer<Font = Font>,
{
    reexport_no_event_methods! {Card<'t, InterneAuswahlNachricht, R>, card, InterneAuswahlNachricht, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, AuswahlNachricht<LeiterSerialisiert>>,
    ) -> event::Status {
        let mut column_messages = Vec::new();
        let mut column_shell = Shell::new(&mut column_messages);
        let mut status = self.card.on_event(
            todo!("state: Tree"),
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut column_shell,
        );
        if column_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            column_shell.revalidate_layout(|| shell.invalidate_layout())
        }
        for message in column_messages {
            status = event::Status::Captured;
            match message {
                InterneAuswahlNachricht::Schließen => shell.publish(AuswahlNachricht::Schließen),
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
                    let leiter = if self.aktueller_tab == &0 {
                        (self.pwm_nachricht)(
                            self.umdrehen_anschluss.clone(),
                            self.pwm_pin.clone(),
                            *self.pwm_polarität,
                        )
                    } else {
                        let NonEmpty { head, tail } = &self.ks_anschlüsse;
                        (self.ks_nachricht)(
                            self.umdrehen_anschluss.clone(),
                            NonEmpty {
                                head: (*head).clone(),
                                tail: tail.iter().map(|anschluss| (*anschluss).clone()).collect(),
                            },
                        )
                    };
                    let nachricht = AuswahlNachricht::Hinzufügen(
                        Name(self.neu_name.clone()),
                        GeschwindigkeitSerialisiert { leiter },
                    );
                    shell.publish(nachricht)
                },
                InterneAuswahlNachricht::Löschen(name) => {
                    shell.publish(AuswahlNachricht::Löschen(name))
                },
            }
        }
        status
    }
}

impl<'t, LeiterSerialisiert, R> From<Auswahl<'t, LeiterSerialisiert, R>>
    for Element<'t, AuswahlNachricht<LeiterSerialisiert>, R>
where
    R: 't + text::Renderer<Font = Font>,
{
    fn from(anzeige: Auswahl<'t, LeiterSerialisiert, R>) -> Self {
        Element::new(anzeige)
    }
}
