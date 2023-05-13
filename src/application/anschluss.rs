//! Auswahl eines [Anschlusses](crate::anschluss::Anschluss).

use std::{collections::HashMap, ops::DerefMut};

use iced_aw::{
    native::{NumberInput, TabLabel, Tabs},
    style::tab_bar,
};
use iced_native::{
    event, text,
    widget::{Column, Radio, Row, Text},
    Element, Font, Length, Renderer,
};
use log::error;

use crate::{
    anschluss::{
        level::Level,
        pcf8574::{self, Beschreibung, Variante},
        pin::pwm,
        polarität::Polarität,
        InputAnschluss, InputSerialisiert, OutputAnschluss, OutputSerialisiert,
    },
    application::{map_mit_zustand::MapMitZustand, style::tab_bar::TabBar},
    eingeschränkt::{kleiner_8, InvaliderWert},
};

/// Zustand eines Widgets zur Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug, PartialEq, Eq)]
struct Zustand<T> {
    active_tab: usize,
    pin: u8,
    beschreibung: Beschreibung,
    port: kleiner_8,
    modus: T,
}

/// Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
#[derive(Debug, Clone)]
struct Input<'t> {
    pin: u8,
    interrupt_pins: &'t HashMap<Beschreibung, u8>,
}

impl<'t> Zustand<Input<'t>> {
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    #[inline(always)]
    fn neu_input(interrupt_pins: &'t HashMap<Beschreibung, u8>) -> Self {
        Self::neu_mit_interrupt(Input { pin: 0, interrupt_pins })
    }

    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss)
    /// mit gegebenen Start-Einstellungen.
    #[inline(always)]
    fn von_input_serialisiert(
        initial: InputSerialisiert,
        interrupt_pins: &'t HashMap<Beschreibung, u8>,
    ) -> Self {
        let make_modus = |pin: u8| Input { pin, interrupt_pins };
        match initial {
            InputSerialisiert::Pin { pin } => Self::neu_mit_initial_pin(pin, make_modus(0)),
            InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt } => {
                Zustand::neu_mit_initial_port(
                    beschreibung,
                    port,
                    make_modus(interrupt.unwrap_or(0)),
                )
            },
        }
    }

    /// Der aktuell gewählte [InputAnschluss](crate::anschluss::InputAnschluss).
    #[inline(always)]
    fn input_anschluss(&self) -> InputSerialisiert {
        self.anschluss(
            |pin, _input| InputSerialisiert::Pin { pin },
            |beschreibung, port, Input { pin, interrupt_pins, .. }| {
                InputSerialisiert::Pcf8574Port {
                    beschreibung,
                    port,
                    interrupt: if interrupt_pins.get(&beschreibung).is_some() {
                        None
                    } else {
                        Some(*pin)
                    },
                }
            },
        )
    }
}

/// Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
#[derive(Debug, Clone, Copy)]
pub struct Output {
    polarität: Polarität,
}

impl Zustand<Output> {
    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    #[inline(always)]
    fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output { polarität: Polarität::Normal })
    }

    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss)
    /// mit gegebenen Start-Einstellungen.
    #[inline(always)]
    fn von_output_serialisiert(initial: OutputSerialisiert) -> Self {
        match initial {
            OutputSerialisiert::Pin { pin, polarität } => {
                Self::neu_mit_initial_pin(pin, Output { polarität })
            },
            OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                Self::neu_mit_initial_port(beschreibung, port, Output { polarität })
            },
        }
    }

    /// Der aktuell gewählte [OutputAnschluss](crate::anschluss::OutputAnschluss).
    #[inline(always)]
    fn output_anschluss(&self) -> OutputSerialisiert {
        self.anschluss(
            |pin, Output { polarität }| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |beschreibung, port, Output { polarität }| OutputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                polarität: *polarität,
            },
        )
    }
}

impl<T> Zustand<T> {
    fn anschluss<M>(
        &self,
        make_pin: impl Fn(u8, &T) -> M,
        make_port: impl Fn(Beschreibung, kleiner_8, &T) -> M,
    ) -> M {
        if self.active_tab == 0 {
            make_pin(self.pin, &self.modus)
        } else {
            make_port(self.beschreibung, self.port, &self.modus)
        }
    }

    fn neu(
        pin: Option<u8>,
        beschreibung_port: Option<(Beschreibung, kleiner_8)>,
        modus: T,
    ) -> Self {
        let active_tab = if beschreibung_port.is_some() { 1 } else { 0 };
        let pin = pin.unwrap_or(0);
        let (beschreibung, port) = beschreibung_port.unwrap_or((
            Beschreibung {
                i2c_bus: pcf8574::I2cBus::I2c0_1,
                a0: Level::Low,
                a1: Level::Low,
                a2: Level::Low,
                variante: Variante::Normal,
            },
            kleiner_8::MIN,
        ));
        Zustand { active_tab, pin, beschreibung, port, modus }
    }

    #[inline(always)]
    fn neu_mit_interrupt(modus: T) -> Self {
        Zustand::neu(None, None, modus)
    }

    #[inline(always)]
    fn neu_mit_initial_pin(pin: u8, modus: T) -> Self {
        Zustand::neu(Some(pin), None, modus)
    }

    #[inline(always)]
    fn neu_mit_initial_port(beschreibung: Beschreibung, port: kleiner_8, modus: T) -> Self {
        Zustand::neu(None, Some((beschreibung, port)), modus)
    }
}

const TAB_PIN: usize = 0;
const TAB_PCF8574: usize = 1;

#[derive(Debug, Clone)]
enum InterneNachricht<T> {
    TabSelected(usize),
    Pin(u8),
    A0(Level),
    A1(Level),
    A2(Level),
    Variante(Variante),
    Port(u8),
    Modus(T),
}

/// Interne Nachricht zur [Auswahl] eines [InputAnschluss](crate::anschluss::InputAnschluss).
#[derive(Debug, Clone, Copy)]
pub struct InputNachricht {
    interrupt: u8,
}

impl InputNachricht {
    fn interrupt(interrupt: u8) -> Self {
        InputNachricht { interrupt }
    }
}

/// Interne Nachricht zur [Auswahl] eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
#[derive(Debug, Clone, Copy)]
pub struct OutputNachricht {
    polarität: Polarität,
}

impl OutputNachricht {
    fn polarität(polarität: Polarität) -> Self {
        OutputNachricht { polarität }
    }
}

/// Widget zur Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug)]
pub struct Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>(
    MapMitZustand<'a, Zustand<Modus>, InterneNachricht<ModusNachricht>, Serialisiert, R>,
);

impl<'a, R> Auswahl<'a, u8, InputNachricht, InputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    <R as iced_native::Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_native::widget::container::StyleSheet
        + iced_native::widget::text_input::StyleSheet
        + iced_native::widget::radio::StyleSheet
        + iced_native::widget::text::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input(
        start_wert: Option<&'a InputAnschluss>,
        interrupt_pins: &'a HashMap<Beschreibung, u8>,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputAnschluss::Pin(pin)) => (TAB_PIN, Some(pin.pin()), None, None, None),
            Some(InputAnschluss::Pcf8574Port(port)) => (
                TAB_PCF8574,
                None,
                Some(*port.beschreibung()),
                Some(port.port()),
                port.interrupt_pin(),
            ),
            None => (TAB_PIN, None, None, None, None),
        };
        Self::neu_input_aux(active_tab, pin, beschreibung, port, modus, interrupt_pins)
    }

    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input_s(
        start_wert: Option<&'a InputSerialisiert>,
        interrupt_pins: &'a HashMap<Beschreibung, u8>,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputSerialisiert::Pin { pin }) => (TAB_PIN, Some(*pin), None, None, None),
            Some(InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt }) => {
                (TAB_PCF8574, None, Some(*beschreibung), Some(*port), *interrupt)
            },
            None => (TAB_PIN, None, None, None, None),
        };
        Self::neu_input_aux(active_tab, pin, beschreibung, port, modus, interrupt_pins)
    }

    fn neu_input_aux(
        active_tab: usize,
        pin: Option<u8>,
        beschreibung: Option<Beschreibung>,
        port: Option<kleiner_8>,
        modus: Option<u8>,
        interrupt_pins: &'a HashMap<Beschreibung, u8>,
    ) -> Self {
        Auswahl::neu_mit_interrupt_view(
            ZeigeModus::Pcf8574,
            |pin, beschreibung| {
                interrupt_pins.get(&beschreibung).map_or(
                    NumberInput::new(*pin, 32, InputNachricht::interrupt).into(),
                    |pin| {
                        let text: Text<'_, R> = Text::new(pin.to_string());
                        Element::from(text)
                    },
                )
            },
            &|modus: &mut u8, InputNachricht { interrupt: pin }| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            |beschreibung, port, pin| InputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                interrupt: if interrupt_pins.get(&beschreibung).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
            move || Zustand {
                active_tab,
                pin: pin.unwrap_or(0),
                beschreibung: beschreibung.unwrap_or(Beschreibung {
                    i2c_bus: pcf8574::I2cBus::I2c0_1,
                    a0: Level::Low,
                    a1: Level::Low,
                    a2: Level::Low,
                    variante: Variante::Normal,
                }),
                port: port.unwrap_or(kleiner_8::MIN),
                modus: modus.unwrap_or(0),
            },
        )
    }
}

impl<'a, R> Auswahl<'a, Polarität, OutputNachricht, OutputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    <R as iced_native::Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_native::widget::container::StyleSheet
        + iced_native::widget::text_input::StyleSheet
        + iced_native::widget::radio::StyleSheet
        + iced_native::widget::text::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output(start_wert: Option<&'a OutputAnschluss>) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputAnschluss::Pin { pin, polarität }) => {
                (TAB_PIN, Some(pin.pin()), None, None, Some(*polarität))
            },
            Some(OutputAnschluss::Pcf8574Port { port, polarität }) => {
                (TAB_PCF8574, None, Some(*port.beschreibung()), Some(port.port()), Some(*polarität))
            },
            None => (TAB_PIN, None, None, None, None),
        };
        Self::neu_output_aux(active_tab, pin, beschreibung, port, modus)
    }

    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output_s(start_wert: Option<OutputSerialisiert>) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputSerialisiert::Pin { pin, polarität }) => {
                (TAB_PIN, Some(pin), None, None, Some(polarität))
            },
            Some(OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität }) => {
                (TAB_PCF8574, None, Some(beschreibung), Some(port), Some(polarität))
            },
            None => (TAB_PIN, None, None, None, None),
        };
        Self::neu_output_aux(active_tab, pin, beschreibung, port, modus)
    }

    fn neu_output_aux(
        active_tab: usize,
        pin: Option<u8>,
        beschreibung: Option<Beschreibung>,
        port: Option<kleiner_8>,
        modus: Option<Polarität>,
    ) -> Self {
        Auswahl::neu_mit_interrupt_view(
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
                pin: pin.unwrap_or(0),
                beschreibung: beschreibung.unwrap_or(Beschreibung {
                    i2c_bus: pcf8574::I2cBus::I2c0_1,
                    a0: Level::Low,
                    a1: Level::Low,
                    a2: Level::Low,
                    variante: Variante::Normal,
                }),
                port: port.unwrap_or(kleiner_8::MIN),
                modus: modus.unwrap_or(Polarität::Normal),
            },
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum ZeigeModus {
    Beide,
    Pcf8574,
}

fn make_radios<'a, T, M, R>(
    current: &T,
    fst: T,
    fst_s: &str,
    snd: T,
    snd_s: &str,
    to_message: impl Fn(T) -> M + Clone + 'static,
) -> Column<'a, M, R>
where
    T: Eq + Copy,
    M: 'a + Clone,
    R: 'a + text::Renderer,
    <R as iced_native::Renderer>::Theme:
        iced_native::widget::radio::StyleSheet + iced_native::widget::text::StyleSheet,
{
    Column::new()
        .push(Radio::new(fst_s, fst, Some(current.clone()), to_message.clone()).spacing(0))
        .push(Radio::new(snd_s, snd, Some(current.clone()), to_message).spacing(0))
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    Modus: Copy,
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
    <R as iced_native::Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_native::widget::container::StyleSheet
        + iced_native::widget::text_input::StyleSheet
        + iced_native::widget::radio::StyleSheet
        + iced_native::widget::text::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    fn neu_mit_interrupt_view(
        zeige_modus: ZeigeModus,
        view_modus: impl 'a + Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
        update_modus: &'a impl Fn(&mut Modus, ModusNachricht),
        make_pin: &'a impl Fn(u8, &Modus) -> Serialisiert,
        make_port: impl 'a + Fn(Beschreibung, kleiner_8, &Modus) -> Serialisiert,
        erzeuge_zustand: impl 'a + Fn() -> Zustand<Modus>,
    ) -> Self {
        let erzeuge_element = move |zustand: &Zustand<Modus>| {
            Self::erzeuge_element(zustand, &view_modus, zeige_modus)
        };
        let mapper = move |interne_nachricht,
                           zustand: &mut dyn DerefMut<Target = Zustand<Modus>>,
                           status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::TabSelected(tab) => zustand.active_tab = tab,
                InterneNachricht::Pin(pin) => zustand.pin = pin,
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
            let nachricht = if zustand.active_tab == 0 {
                make_pin(zustand.pin, &zustand.modus)
            } else {
                make_port(zustand.beschreibung, zustand.port, &zustand.modus)
            };
            vec![nachricht]
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
    <R as iced_native::Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_native::widget::container::StyleSheet
        + iced_native::widget::text_input::StyleSheet
        + iced_native::widget::radio::StyleSheet
        + iced_native::widget::text::StyleSheet,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    fn erzeuge_element(
        zustand: &Zustand<Modus>,
        view_modus: &impl Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
        zeige_modus: ZeigeModus,
    ) -> Element<'a, InterneNachricht<ModusNachricht>, R> {
        let Zustand { active_tab, pin, beschreibung, port, modus } = zustand;
        let element_modus = view_modus(&modus, *beschreibung);
        // TODO anzeige des verwendeten I2cBus
        let Beschreibung { i2c_bus: _, a0, a1, a2, variante } = beschreibung;
        let view_modus_mapped = element_modus.map(InterneNachricht::Modus);
        let high_low_column =
            |level: &Level, to_message: fn(Level) -> InterneNachricht<ModusNachricht>| {
                make_radios(level, Level::High, "H", Level::Low, "L", to_message)
            };
        let pcf8574_row = Row::new()
            .push(high_low_column(a0, InterneNachricht::A0))
            .push(high_low_column(a1, InterneNachricht::A1))
            .push(high_low_column(a2, InterneNachricht::A2))
            .push(make_radios(
                variante,
                Variante::Normal,
                "Normal",
                Variante::A,
                "A",
                InterneNachricht::Variante,
            ))
            .push(NumberInput::new(
                u8::from(*port),
                u8::from(kleiner_8::MAX),
                InterneNachricht::Port,
            ));
        // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
        let width = Length::Fixed(350.);
        let row = match zeige_modus {
            ZeigeModus::Pcf8574 => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_owned()), {
                        pcf8574_row.push(view_modus_mapped).into()
                    }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InterneNachricht::TabSelected)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs)
            },
            ZeigeModus::Beide => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_owned()), { pcf8574_row.into() }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InterneNachricht::TabSelected)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            },
        };
        row.into()
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R>
    From<Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>> for Element<'a, Serialisiert, R>
where
    Modus: 'static + PartialEq,
    ModusNachricht: 'a,
    Serialisiert: 'a,
    R: 'a + Renderer,
{
    fn from(auswahl: Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>) -> Self {
        Element::from(auswahl.0)
    }
}

/// Zustand eines Widgets zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug, PartialEq, Eq)]
struct PwmZustand {
    pin: u8,
}

impl PwmZustand {
    /// Erstelle einen neuen [PwmZustand].
    fn neu(pin: Option<u8>) -> Self {
        PwmZustand { pin: pin.unwrap_or(0) }
    }
}

/// Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug)]
pub struct Pwm<'a, R>(MapMitZustand<'a, PwmZustand, pwm::Serialisiert, pwm::Serialisiert, R>);

impl<'a, R> Pwm<'a, R>
where
    R: 'a + iced_native::text::Renderer<Font = Font>,
    <R as iced_native::Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_native::widget::text::StyleSheet
        + iced_native::widget::text_input::StyleSheet
        + iced_native::widget::container::StyleSheet,
{
    /// Erstelle ein Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
    pub fn neu(pin: Option<&'a pwm::Pin>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(pin.map(pwm::Pin::pin));
        Pwm(MapMitZustand::neu(erzeuge_zustand, Self::erzeuge_element, Self::mapper))
    }

    /// Erstelle ein Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
    pub fn neu_s(pin: Option<pwm::Serialisiert>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(pin.as_ref().map(|pin| pin.0));
        Pwm(MapMitZustand::neu(erzeuge_zustand, Self::erzeuge_element, Self::mapper))
    }

    fn erzeuge_element(zustand: &PwmZustand) -> Element<'a, pwm::Serialisiert, R> {
        NumberInput::new(zustand.pin, 32, pwm::Serialisiert).into()
    }

    fn mapper(
        nachricht: pwm::Serialisiert,
        zustand: &mut (dyn DerefMut<Target = PwmZustand>),
        status: &mut event::Status,
    ) -> Vec<pwm::Serialisiert> {
        *status = event::Status::Captured;
        let pwm::Serialisiert(pin) = nachricht;
        zustand.pin = pin;
        vec![nachricht]
    }
}

impl<'a, R> From<Pwm<'a, R>> for Element<'a, pwm::Serialisiert, R>
where
    R: 'a + Renderer,
{
    fn from(auswahl: Pwm<'a, R>) -> Self {
        Element::from(auswahl.0)
    }
}
