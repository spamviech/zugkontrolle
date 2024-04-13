//! Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).

use iced_aw::{
    number_input, style, tab_bar,
    widgets::{NumberInput, TabLabel, Tabs},
};
use iced_core::{event, text as text_core, widget::Text, Element, Font, Length, Renderer};
use iced_widget::{
    container, radio,
    scrollable::{self, Scrollable},
    text, text_input, Column, Container, Radio, Row, Space,
};
use log::error;

use zugkontrolle_anschluss::{
    level::Level,
    pcf8574::{self, Beschreibung, I2cBus, Lager, Variante},
    pin::pwm,
    polarität::Polarität,
    InputAnschluss, InputSerialisiert, OutputAnschluss, OutputSerialisiert,
};
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_util::eingeschränkt::{kleiner_8, InvaliderWert};

use crate::{
    bootstrap::{Bootstrap, Icon},
    map_mit_zustand::MapMitZustand,
    style::{sammlung::Sammlung, tab_bar::TabBar},
};

/// Welche Tab-Seite wird angezeigt.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
enum TabId {
    /// Auswahl eines Pins.
    #[default]
    Pin,
    /// Auswahl eines Pcf8574-Ports.
    Pcf8574,
}

/// Zustand eines Widgets zur Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug, Default, PartialEq, Eq)]
struct Zustand<T> {
    /// Welche Tab-Seite wird angezeigt.
    active_tab: TabId,
    /// Der aktuell ausgewählte Pin.
    pin: u8,
    /// Der aktuell ausgewählte Pcf8574.
    beschreibung: Beschreibung,
    /// Der aktuell ausgewählte Port.
    port: kleiner_8,
    /// Zusätzlicher Zustand für die Auswahl eines [Input-](InputAnschluss) oder [`OutputAnschlusses`](OutputAnschluss).
    modus: T,
}

/// Eine interne Nachricht für die Auswahl eines Anschlusses.
#[derive(Debug, Clone)]
enum InterneNachricht<T> {
    /// Wechsel auf eine Tab-Seite.
    TabSelected(TabId),
    /// Neuer aktuell gewählter Pin.
    Pin(u8),
    /// Neuer aktuell gewählter I2cBus für einen Pcf8574.
    I2cBus(I2cBus),
    /// Neues aktuell gewähltes A0-Level für einen Pcf8574.
    A0(Level),
    /// Neues aktuell gewähltes A1-Level für einen Pcf8574.
    A1(Level),
    /// Neues aktuell gewähltes A2-Level für einen Pcf8574.
    A2(Level),
    /// Neue aktuell gewählte Variante für einen Pcf8574.
    Variante(Variante),
    /// Neuer aktuell gewählter Port für einen Pcf8574.
    Port(u8),
    /// Neuer Zustand für die Auswahl eines [Input-](InputAnschluss) oder [`OutputAnschlusses`](OutputAnschluss).
    Modus(T),
}

/// Interne Nachricht zur [`Auswahl`] eines [`InputAnschluss`](crate::anschluss::InputAnschluss).
#[derive(Debug, Clone, Copy)]
pub struct InputNachricht {
    /// Neuer aktuell gewählter Interrupt-Pin.
    interrupt: u8,
}

impl InputNachricht {
    /// Erstelle eine neue Nachricht mit dem gewählten Interrupt-Pin.
    fn interrupt(interrupt: u8) -> Self {
        InputNachricht { interrupt }
    }
}

/// Interne Nachricht zur [`Auswahl`] eines [`OutputAnschluss`](crate::anschluss::OutputAnschluss).
#[derive(Debug, Clone, Copy)]
pub struct OutputNachricht {
    /// Neue aktuell gewählte Polarität.
    polarität: Polarität,
}

impl OutputNachricht {
    /// Erstelle eine neue Nachricht mit der gewählten Polarität.
    fn polarität(polarität: Polarität) -> Self {
        OutputNachricht { polarität }
    }
}

/// Widget zur Auswahl eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Debug)]
pub struct Auswahl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>(
    MapMitZustand<'a, Zustand<Modus>, InterneNachricht<ModusNachricht>, Serialisiert, Thema, R>,
);

impl<'a, Thema, R> Auswahl<'a, u8, InputNachricht, InputSerialisiert, Thema, R>
where
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + container::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <Thema as scrollable::StyleSheet>::Style: From<Sammlung>,
    <Thema as style::tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [`InputAnschluss`](crate::anschluss::InputAnschluss).
    #[must_use]
    pub fn neu_input(
        start_wert: Option<&'a InputAnschluss>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputAnschluss::Pin(pin)) => (TabId::Pin, Some(pin.pin()), None, None, None),
            Some(InputAnschluss::Pcf8574Port(port)) => (
                TabId::Pcf8574,
                None,
                Some(*port.beschreibung()),
                Some(port.port()),
                port.interrupt_pin(),
            ),
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_input_aux(
            active_tab,
            pin,
            beschreibung,
            port,
            modus,
            lager,
            scrollable_style,
            settings,
        )
    }

    /// Erstelle ein Widget zur Auswahl eines [`InputAnschluss`](crate::anschluss::InputAnschluss).
    #[must_use]
    pub fn neu_input_s(
        start_wert: Option<&InputSerialisiert>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputSerialisiert::Pin { pin }) => (TabId::Pin, Some(pin), None, None, None),
            Some(InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt }) => {
                (TabId::Pcf8574, None, Some(beschreibung), Some(port), interrupt.as_ref())
            },
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_input_aux(
            active_tab,
            pin.copied(),
            beschreibung.copied(),
            port.copied(),
            modus.copied(),
            lager,
            scrollable_style,
            settings,
        )
    }

    // Alle Argumente benötigt, evtl. Zusammenfassen aller Startwerte in Hilfs-Struct?
    #[allow(clippy::too_many_arguments)]
    /// Erstelle ein Widget zur Auswahl eines [`InputAnschluss`](crate::anschluss::InputAnschluss).
    fn neu_input_aux(
        active_tab: TabId,
        start_pin: Option<u8>,
        start_beschreibung: Option<Beschreibung>,
        start_port: Option<kleiner_8>,
        start_modus: Option<u8>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        Auswahl::neu_mit_modus_view(
            ZeigeModus::Pcf8574,
            |pin, beschreibung| {
                let interrupt_pin = lager.interrupt_pin(&beschreibung).map_or(
                    Element::from(
                        NumberInput::new(*pin, 32, InputNachricht::interrupt).width(Length::Fill),
                    ),
                    |interrupt_pin| {
                        let text: Text<'_, Thema, R> = Text::new(interrupt_pin.to_string());
                        Element::from(text)
                    },
                );
                Element::from(
                    Column::new()
                        .push(
                            Container::new(Text::new("Interrupt-Pin"))
                                .width(Length::Fill)
                                .center_x(),
                        )
                        .push(interrupt_pin)
                        .width(Length::Fixed(100.)),
                )
            },
            &|modus: &mut u8, InputNachricht { interrupt: pin }| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            |beschreibung, port, pin| InputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                interrupt: if lager.interrupt_pin(&beschreibung).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
            move || Zustand {
                active_tab,
                pin: start_pin.unwrap_or(0),
                beschreibung: start_beschreibung.unwrap_or(Beschreibung {
                    i2c_bus: pcf8574::I2cBus::I2c0_1,
                    a0: Level::Low,
                    a1: Level::Low,
                    a2: Level::Low,
                    variante: Variante::Normal,
                }),
                port: start_port.unwrap_or(kleiner_8::MIN),
                modus: start_modus.unwrap_or(0),
            },
            scrollable_style,
            settings,
        )
    }
}

impl<'a, Thema, R> Auswahl<'a, Polarität, OutputNachricht, OutputSerialisiert, Thema, R>
where
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + container::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <Thema as scrollable::StyleSheet>::Style: From<Sammlung>,
    <Thema as style::tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [`OutputAnschluss`](crate::anschluss::OutputAnschluss).
    #[must_use]
    pub fn neu_output(
        start_wert: Option<&'a OutputAnschluss>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputAnschluss::Pin { pin, polarität }) => {
                (TabId::Pin, Some(pin.pin()), None, None, Some(*polarität))
            },
            Some(OutputAnschluss::Pcf8574Port { port, polarität }) => (
                TabId::Pcf8574,
                None,
                Some(*port.beschreibung()),
                Some(port.port()),
                Some(*polarität),
            ),
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_output_aux(active_tab, pin, beschreibung, port, modus, scrollable_style, settings)
    }

    /// Erstelle ein Widget zur Auswahl eines [`OutputAnschluss`](crate::anschluss::OutputAnschluss).
    #[must_use]
    pub fn neu_output_s(
        start_wert: Option<&OutputSerialisiert>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputSerialisiert::Pin { pin, polarität }) => {
                (TabId::Pin, Some(pin), None, None, Some(polarität))
            },
            Some(OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität }) => {
                (TabId::Pcf8574, None, Some(beschreibung), Some(port), Some(polarität))
            },
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_output_aux(
            active_tab,
            pin.copied(),
            beschreibung.copied(),
            port.copied(),
            modus.copied(),
            scrollable_style,
            settings,
        )
    }

    /// Erstelle ein Widget zur Auswahl eines [`OutputAnschluss`](crate::anschluss::OutputAnschluss).
    fn neu_output_aux(
        active_tab: TabId,
        start_pin: Option<u8>,
        start_beschreibung: Option<Beschreibung>,
        start_port: Option<kleiner_8>,
        start_polarität: Option<Polarität>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        Auswahl::neu_mit_modus_view(
            ZeigeModus::Beide,
            |polarität, _beschreibung| {
                Column::new()
                    .push(Radio::new(
                        "Normal",
                        Polarität::Normal,
                        Some(*polarität),
                        OutputNachricht::polarität,
                    ))
                    .push(Radio::new(
                        "Invertiert",
                        Polarität::Invertiert,
                        Some(*polarität),
                        OutputNachricht::polarität,
                    ))
                    .into()
            },
            &|modus, OutputNachricht { polarität }| *modus = polarität,
            &|pin, polarität| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |beschreibung, port, polarität| OutputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                polarität: *polarität,
            },
            move || Zustand {
                active_tab,
                pin: start_pin.unwrap_or(0),
                beschreibung: start_beschreibung.unwrap_or(Beschreibung {
                    i2c_bus: pcf8574::I2cBus::I2c0_1,
                    a0: Level::Low,
                    a1: Level::Low,
                    a2: Level::Low,
                    variante: Variante::Normal,
                }),
                port: start_port.unwrap_or(kleiner_8::MIN),
                modus: start_polarität.unwrap_or(Polarität::Normal),
            },
            scrollable_style,
            settings,
        )
    }
}

/// Wann soll die Auswahl für die Modus-spezifischen Einstellungen gezeigt werden?
#[derive(Debug, Clone, Copy)]
enum ZeigeModus {
    /// Die Auswahl wird bei beiden Tabs gezeigt.
    Beide,
    /// Die Auswahl wird nur bei dem Tab für einen Pcf8574 gezeigt.
    Pcf8574,
}

// anonymous lifetimes in `impl Trait` are unstable
#[allow(single_use_lifetimes)]
/// Erstelle einen [`Radio`] für alle `elemente` und füge sie zu einem [`Column`] hinzu.
pub(crate) fn make_radios<'a, 'b, T, M, Thema, R>(
    aktuell: &T,
    elemente: impl IntoIterator<Item = (&'b str, T)>,
    als_nachricht: impl Fn(T) -> M + Clone + 'static,
) -> Column<'a, M, Thema, R>
where
    T: Eq + Copy,
    M: 'a + Clone,
    R: 'a + text_core::Renderer,
    Thema: 'a + radio::StyleSheet + text::StyleSheet,
    <R as text_core::Renderer>::Font: From<Font>,
{
    let mut column = Column::new();
    let mut leerer_iterator = true;
    for (label, value) in elemente {
        leerer_iterator = false;
        column =
            column.push(Radio::new(label, value, Some(*aktuell), als_nachricht.clone()).spacing(0));
    }
    if leerer_iterator {
        column = column.push(Icon::neu(Bootstrap::ExclamationTriangle));
    }
    column
}

impl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>
    Auswahl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>
where
    Modus: Copy,
    ModusNachricht: 'static + Clone,
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + container::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <Thema as scrollable::StyleSheet>::Style: From<Sammlung>,
    <Thema as style::tab_bar::StyleSheet>::Style: From<TabBar>,
{
    // Alle Argumente werden benötigt.
    #[allow(clippy::too_many_arguments)]
    /// Erzeuge ein neues [`Auswahl`]-Widget.
    fn neu_mit_modus_view(
        zeige_modus: ZeigeModus,
        view_modus: impl 'a + Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, Thema, R>,
        update_modus: &'a impl Fn(&mut Modus, ModusNachricht),
        make_pin: &'a impl Fn(u8, &Modus) -> Serialisiert,
        make_port: impl 'a + Fn(Beschreibung, kleiner_8, &Modus) -> Serialisiert,
        erzeuge_zustand: impl 'a + Fn() -> Zustand<Modus>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_element = move |zustand: &Zustand<Modus>| {
            Self::erzeuge_element(zustand, &view_modus, zeige_modus, scrollable_style, settings)
        };
        let mapper = move |interne_nachricht,
                           zustand: &mut Zustand<Modus>,
                           status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::TabSelected(tab) => zustand.active_tab = tab,
                InterneNachricht::Pin(pin) => zustand.pin = pin,
                InterneNachricht::I2cBus(i2c_bus) => zustand.beschreibung.i2c_bus = i2c_bus,
                InterneNachricht::A0(a0) => zustand.beschreibung.a0 = a0,
                InterneNachricht::A1(a1) => zustand.beschreibung.a1 = a1,
                InterneNachricht::A2(a2) => zustand.beschreibung.a2 = a2,
                InterneNachricht::Variante(variante) => zustand.beschreibung.variante = variante,
                InterneNachricht::Port(port) => {
                    zustand.port = match kleiner_8::try_from(port) {
                        Ok(port) => port,
                        Err(InvaliderWert(port)) => {
                            error!("Port {} > kleiner_8::MAX {}", port, kleiner_8::MAX);
                            kleiner_8::MAX
                        },
                    }
                },
                InterneNachricht::Modus(msg) => update_modus(&mut zustand.modus, msg),
            }
            let nachricht = match zustand.active_tab {
                TabId::Pin => make_pin(zustand.pin, &zustand.modus),
                TabId::Pcf8574 => make_port(zustand.beschreibung, zustand.port, &zustand.modus),
            };
            vec![nachricht]
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand(), erzeuge_element, mapper))
    }
}

/// Wie viel [`Platz`](Space) soll zwischen Widgets eingefügt werden?
const PADDING: f32 = 2.5;

impl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>
    Auswahl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>
where
    ModusNachricht: 'static + Clone,
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + number_input::StyleSheet
        + tab_bar::StyleSheet
        + container::StyleSheet
        + radio::StyleSheet
        + scrollable::StyleSheet
        + text::StyleSheet
        + text_input::StyleSheet,
    <Thema as scrollable::StyleSheet>::Style: From<Sammlung>,
    <Thema as style::tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erzeuge die interne Widget-Hierarchie für ein [`Auswahl`]-Widget.
    fn erzeuge_element(
        zustand: &Zustand<Modus>,
        view_modus: &impl Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, Thema, R>,
        zeige_modus: ZeigeModus,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'a, InterneNachricht<ModusNachricht>, Thema, R> {
        let Zustand { active_tab, pin, beschreibung, port, modus } = zustand;
        let element_modus = view_modus(modus, *beschreibung);
        let Beschreibung { i2c_bus, a0, a1, a2, variante } = beschreibung;
        let view_modus_mapped = element_modus.map(InterneNachricht::Modus);
        let high_low_column =
            |level: &Level, als_nachricht: fn(Level) -> InterneNachricht<ModusNachricht>| {
                make_radios(level, [("H", Level::High), ("L", Level::Low)], als_nachricht)
            };
        let pcf8574_row = Row::new()
            .push(
                Scrollable::new(
                    Row::new()
                        .push(Text::new("I2C"))
                        .push(Space::with_width(Length::Fixed(PADDING)))
                        .push(make_radios(
                            i2c_bus,
                            [
                                ("0/1", I2cBus::I2c0_1),
                                ("3", I2cBus::I2c3),
                                ("4", I2cBus::I2c4),
                                ("5", I2cBus::I2c5),
                                ("6", I2cBus::I2c6),
                            ]
                            .into_iter()
                            .filter(|(_label, kandidat)| kandidat.aktiviert(settings)),
                            InterneNachricht::I2cBus,
                        ))
                        .push(Space::with_width(Length::Fixed(scrollable_style.breite()))),
                )
                .height(Length::Fixed(55.))
                .style(<Thema as scrollable::StyleSheet>::Style::from(scrollable_style)),
            )
            .push(high_low_column(a0, InterneNachricht::A0))
            .push(high_low_column(a1, InterneNachricht::A1))
            .push(high_low_column(a2, InterneNachricht::A2))
            .push(make_radios(
                variante,
                [("Normal", Variante::Normal), ("A", Variante::A)],
                InterneNachricht::Variante,
            ))
            .push(
                Column::new()
                    .push(Container::new(Text::new("Port")).width(Length::Fill).center_x())
                    .push(NumberInput::new(
                        u8::from(*port),
                        u8::from(kleiner_8::MAX),
                        InterneNachricht::Port,
                    ))
                    .width(Length::Fixed(75.)),
            );
        let width = Length::Fixed(600.);
        let row = match zeige_modus {
            ZeigeModus::Pcf8574 => {
                let tabs = vec![
                    (
                        TabId::Pin,
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabId::Pcf8574, TabLabel::Text("Pcf8574-Port".to_owned()), {
                        pcf8574_row.push(view_modus_mapped).into()
                    }),
                ];
                let tabs = Tabs::new_with_tabs(tabs, InterneNachricht::TabSelected)
                    .set_active_tab(active_tab)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs)
            },
            ZeigeModus::Beide => {
                let tabs = vec![
                    (
                        TabId::Pin,
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabId::Pcf8574, TabLabel::Text("Pcf8574-Port".to_owned()), {
                        pcf8574_row.into()
                    }),
                ];
                let tabs = Tabs::new_with_tabs(tabs, InterneNachricht::TabSelected)
                    .set_active_tab(active_tab)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            },
        };
        row.into()
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>
    From<Auswahl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>>
    for Element<'a, Serialisiert, Thema, R>
where
    Modus: 'static + Default,
    ModusNachricht: 'a,
    Serialisiert: 'a,
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(auswahl: Auswahl<'a, Modus, ModusNachricht, Serialisiert, Thema, R>) -> Self {
        Element::from(auswahl.0)
    }
}

/// Zustand eines Widgets zur Auswahl eines [`Pwm-Pins`](pwm::Pin).
#[derive(Debug, Default, PartialEq, Eq)]
struct PwmZustand {
    /// Der aktuell gewählte Pin.
    pin: u8,
}

impl PwmZustand {
    /// Erstelle einen neuen [`PwmZustand`].
    fn neu(pin: Option<u8>) -> Self {
        PwmZustand { pin: pin.unwrap_or(0) }
    }
}

/// Widget zur Auswahl eines [`Pwm-Pins`](pwm::Pin).
#[derive(Debug)]
pub struct Pwm<'a, Thema, R>(
    MapMitZustand<'a, PwmZustand, pwm::Serialisiert, pwm::Serialisiert, Thema, R>,
);

impl<'a, Thema, R> Pwm<'a, Thema, R>
where
    R: 'a + text_core::Renderer<Font = Font>,
    Thema: 'a
        + number_input::StyleSheet
        + text::StyleSheet
        + container::StyleSheet
        + text_input::StyleSheet,
{
    /// Erstelle ein Widget zur Auswahl eines [`Pwm-Pins`](pwm::Pin).
    pub fn neu(pin: Option<&'a pwm::Pin>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(pin.map(pwm::Pin::pin));
        Pwm(MapMitZustand::neu(erzeuge_zustand(), Self::erzeuge_element, Self::mapper))
    }

    /// Erstelle ein Widget zur Auswahl eines [`Pwm-Pins`](pwm::Pin).
    pub fn neu_s(start_pin: Option<pwm::Serialisiert>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(start_pin.as_ref().map(|pin| pin.0));
        Pwm(MapMitZustand::neu(erzeuge_zustand(), Self::erzeuge_element, Self::mapper))
    }

    /// Erzeuge die Widget-Hierarchie für ein [`Pwm`]-Widget.
    fn erzeuge_element(zustand: &PwmZustand) -> Element<'a, pwm::Serialisiert, Thema, R> {
        NumberInput::new(zustand.pin, 32, pwm::Serialisiert).into()
    }

    /// Konvertiere die interne Nachricht für ein [`Pwm`]-Widget.
    fn mapper(
        nachricht: pwm::Serialisiert,
        zustand: &mut PwmZustand,
        status: &mut event::Status,
    ) -> Vec<pwm::Serialisiert> {
        *status = event::Status::Captured;
        let pwm::Serialisiert(pin) = nachricht;
        zustand.pin = pin;
        vec![nachricht]
    }
}

impl<'a, Thema, R> From<Pwm<'a, Thema, R>> for Element<'a, pwm::Serialisiert, Thema, R>
where
    Thema: 'a,
    R: 'a + Renderer,
{
    fn from(auswahl: Pwm<'a, Thema, R>) -> Self {
        Element::from(auswahl.0)
    }
}
