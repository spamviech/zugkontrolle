//! Auswahl eines [Anschlusses](crate::anschluss::Anschluss).

use std::{
    collections::HashMap,
    convert::identity,
    fmt::{self, Debug, Formatter},
};

use iced_aw::native::{
    number_input::{self, NumberInput},
    TabLabel, Tabs,
};
use iced_native::{
    event, text,
    widget::{Column, Radio, Row, Text},
    Clipboard, Element, Event, Font, Layout, Length, Point, Renderer, Shell, Widget,
};
use log::error;

use crate::{
    anschluss::{
        level::Level,
        pcf8574::{self, Beschreibung, Variante},
        pin::pwm,
        polarität::Polarität,
        InputSerialisiert, OutputSerialisiert,
    },
    application::{macros::reexport_no_event_methods, style::tab_bar::TabBar},
    eingeschränkt::{kleiner_8, InvaliderWert},
};

/// Zustand eines Widgets zur Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug)]
pub struct Zustand<T> {
    active_tab: usize,
    pin_zustand: number_input::State,
    pin: u8,
    beschreibung: Beschreibung,
    port_zustand: number_input::State,
    port: kleiner_8,
    modus: T,
}

/// Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
#[derive(Debug, Clone)]
pub struct Input<'t> {
    number_input_zustand: number_input::State,
    pin: u8,
    interrupt_pins: &'t HashMap<Beschreibung, u8>,
}

impl<'t> Zustand<Input<'t>> {
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    #[inline(always)]
    pub fn neu_input(interrupt_pins: &'t HashMap<Beschreibung, u8>) -> Self {
        Self::neu_mit_interrupt(Input {
            number_input_zustand: number_input::State::new(),
            pin: 0,
            interrupt_pins,
        })
    }

    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss)
    /// mit gegebenen Start-Einstellungen.
    #[inline(always)]
    pub fn von_input_serialisiert(
        initial: InputSerialisiert,
        interrupt_pins: &'t HashMap<Beschreibung, u8>,
    ) -> Self {
        let make_modus = |pin: u8| Input {
            number_input_zustand: number_input::State::new(),
            pin,
            interrupt_pins,
        };
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
    pub fn input_anschluss(&self) -> InputSerialisiert {
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
    pub fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output { polarität: Polarität::Normal })
    }

    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss)
    /// mit gegebenen Start-Einstellungen.
    #[inline(always)]
    pub fn von_output_serialisiert(initial: OutputSerialisiert) -> Self {
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
    pub fn output_anschluss(&self) -> OutputSerialisiert {
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
        Zustand {
            active_tab,
            pin_zustand: number_input::State::new(),
            pin,
            beschreibung,
            port_zustand: number_input::State::new(),
            port,
            modus,
        }
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
pub struct Auswahl<'a, T, I, M, R> {
    row: Row<'a, InterneNachricht<I>, R>,
    active_tab: &'a mut usize,
    pin: &'a mut u8,
    beschreibung: &'a mut Beschreibung,
    port: &'a mut kleiner_8,
    modus: &'a mut T,
    update_modus: &'a dyn Fn(&mut T, I),
    make_pin: &'a dyn Fn(u8, &T) -> M,
    make_port: Box<dyn Fn(Beschreibung, kleiner_8, &T) -> M>,
}

impl<T: Debug, I, M, R> Debug for Auswahl<'_, T, I, M, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("row", &"<Row>")
            .field("active_tab", &self.active_tab)
            .field("pin", &self.pin)
            .field("beschreibung", &self.beschreibung)
            .field("port", &self.port)
            .field("modus", &self.modus)
            .field("update_modus", &"<closure>")
            .field("make_pin", &"<closure>")
            .field("make_port", &"<closure>")
            .finish()
    }
}

impl<'a, R> Auswahl<'a, u8, InputNachricht, InputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    Element<'a, InputNachricht, R>: From<NumberInput<'a, u8, InputNachricht, R>>,
    Element<'a, InterneNachricht<InputNachricht>, R>:
        From<NumberInput<'a, u8, InterneNachricht<InputNachricht>, R>>,
{
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input(zustand: &'a mut Zustand<Input<'a>>) -> Self {
        let interrupt_pins = zustand.modus.interrupt_pins.clone();
        Auswahl::neu_mit_interrupt_view(
            zustand,
            ZeigeModus::Pcf8574,
            |Input { number_input_zustand, pin, interrupt_pins }, beschreibung| {
                (
                    interrupt_pins.get(&beschreibung).map_or(
                        NumberInput::new(number_input_zustand, *pin, 32, InputNachricht::interrupt)
                            .into(),
                        |pin| Text::new(pin.to_string()).into(),
                    ),
                    pin,
                )
            },
            &|modus: &mut u8, InputNachricht { interrupt: pin }| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            move |beschreibung, port, pin| InputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                interrupt: if interrupt_pins.get(&beschreibung).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
        )
    }
}

impl<'a, R> Auswahl<'a, Polarität, OutputNachricht, OutputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    Element<'a, InterneNachricht<OutputNachricht>, R>:
        From<NumberInput<'a, u8, InterneNachricht<OutputNachricht>, R>>,
{
    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output(zustand: &'a mut Zustand<Output>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            zustand,
            ZeigeModus::Beide,
            |Output { polarität }, _beschreibung| {
                (
                    Column::new()
                        .push(Radio::new(
                            Polarität::Normal,
                            "Normal",
                            Some(*polarität),
                            OutputNachricht::polarität,
                        ))
                        .push(Radio::new(
                            Polarität::Invertiert,
                            "Invertiert",
                            Some(*polarität),
                            OutputNachricht::polarität,
                        ))
                        .into(),
                    polarität,
                )
            },
            &|modus, OutputNachricht { polarität }| *modus = polarität,
            &|pin, polarität| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |beschreibung, port, polarität| OutputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                polarität: *polarität,
            },
        )
    }
}

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
{
    Column::new()
        .push(Radio::new(fst, fst_s, Some(current.clone()), to_message.clone()).spacing(0))
        .push(Radio::new(snd, snd_s, Some(current.clone()), to_message).spacing(0))
}

impl<'a, T, I: 'static + Clone, M, R> Auswahl<'a, T, I, M, R>
where
    T: Eq + Copy,
    M: 'a + Clone,
    R: 'a + text::Renderer<Font = Font>,
    Element<'a, InterneNachricht<I>, R>: From<NumberInput<'a, u8, InterneNachricht<I>, R>>,
    Element<'a, InterneNachricht<I>, R>: From<Row<'a, InterneNachricht<I>, R>>,
    Element<'a, InterneNachricht<I>, R>: From<Tabs<'a, InterneNachricht<I>, R>>,
{
    fn neu_mit_interrupt_view<IO>(
        zustand: &'a mut Zustand<IO>,
        zeige_modus: ZeigeModus,
        view_modus: impl FnOnce(&'a mut IO, Beschreibung) -> (Element<'a, I, R>, &'a mut T),
        update_modus: &'a impl Fn(&mut T, I),
        make_pin: &'a impl Fn(u8, &T) -> M,
        make_port: impl 'static + Fn(Beschreibung, kleiner_8, &T) -> M,
    ) -> Self {
        let Zustand { active_tab, pin_zustand, pin, beschreibung, port_zustand, port, modus } =
            zustand;
        let (view_modus, modus) = view_modus(modus, *beschreibung);
        // TODO anzeige des verwendeten I2cBus
        let Beschreibung { i2c_bus: _, a0, a1, a2, variante } = beschreibung;
        let view_modus_mapped = view_modus.map(InterneNachricht::Modus);
        let high_low_column = |level: &Level, to_message: fn(Level) -> InterneNachricht<I>| {
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
                port_zustand,
                u8::from(*port),
                u8::from(kleiner_8::MAX),
                InterneNachricht::Port,
            ));
        // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
        let width = Length::Units(350);
        let row = match zeige_modus {
            ZeigeModus::Pcf8574 => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_string()),
                        NumberInput::new(pin_zustand, *pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_string()), {
                        pcf8574_row.push(view_modus_mapped).into()
                    }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InterneNachricht::TabSelected)
                    .tab_bar_style(TabBar)
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs)
            },
            ZeigeModus::Beide => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_string()),
                        NumberInput::new(pin_zustand, *pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_string()), { pcf8574_row.into() }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InterneNachricht::TabSelected)
                    .tab_bar_style(TabBar)
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            },
        };
        Auswahl {
            row,
            active_tab,
            pin,
            beschreibung,
            port,
            modus,
            update_modus,
            make_pin,
            make_port: Box::new(make_port),
        }
    }
}

impl<'a, T, I, M, R: Renderer> Widget<M, R> for Auswahl<'a, T, I, M, R> {
    reexport_no_event_methods! {Row<'a, InterneNachricht<I>, R>, row, InterneNachricht<I>, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, M>,
    ) -> event::Status {
        let mut internal_messages = Vec::new();
        let mut internal_shell = Shell::new(&mut internal_messages);
        let mut status = self.row.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut internal_shell,
        );
        if internal_shell.are_widgets_invalid() {
            shell.invalidate_widgets()
        } else {
            internal_shell.revalidate_layout(|| shell.invalidate_layout())
        }
        let mut changed = false;
        for message in internal_messages {
            changed = true;
            match message {
                InterneNachricht::TabSelected(tab) => *self.active_tab = tab,
                InterneNachricht::Pin(pin) => *self.pin = pin,
                InterneNachricht::A0(a0) => self.beschreibung.a0 = a0,
                InterneNachricht::A1(a1) => self.beschreibung.a1 = a1,
                InterneNachricht::A2(a2) => self.beschreibung.a2 = a2,
                InterneNachricht::Variante(variante) => self.beschreibung.variante = variante,
                InterneNachricht::Port(port) => {
                    // kleiner_8: TryFrom<u8> nicht implementiert
                    // NumCast::from ebenfalls nicht möglich (auch bei aktiviertem "num"-feature)
                    // daher `kleiner_8::new` mit potentiellem panic notwendig (ausgeschlossen durch if)
                    *self.port = match kleiner_8::try_from(port) {
                        Ok(port) => port,
                        Err(InvaliderWert(port)) => {
                            error!("Port {} > kleiner_8::MAX {}", port, kleiner_8::MAX);
                            kleiner_8::MAX
                        },
                    }
                },
                InterneNachricht::Modus(msg) => (self.update_modus)(self.modus, msg),
            }
            status = event::Status::Captured;
        }
        if changed {
            let message = if *self.active_tab == 0 {
                (self.make_pin)(*self.pin, &self.modus)
            } else {
                (self.make_port)(*self.beschreibung, *self.port, self.modus)
            };
            shell.publish(message);
        }
        status
    }
}

impl<'a, T, I, M, R: 'a + Renderer> From<Auswahl<'a, T, I, M, R>> for Element<'a, M, R> {
    fn from(auswahl: Auswahl<'a, T, I, M, R>) -> Self {
        Element::new(auswahl)
    }
}

/// Zustand eines Widgets zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug)]
pub struct PwmZustand {
    pin: u8,
    number_input_zustand: number_input::State,
}

impl PwmZustand {
    /// Erstelle einen neuen [PwmZustand].
    pub fn neu() -> Self {
        PwmZustand { pin: 0, number_input_zustand: number_input::State::new() }
    }
}

/// Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
pub struct Pwm<'a, R: 'a + text::Renderer<Font = Font>> {
    number_input: NumberInput<'a, u8, pwm::Serialisiert, R>,
    pin: &'a mut u8,
}

impl<'a, R: 'a + text::Renderer<Font = Font>> Debug for Pwm<'a, R> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Pwm")
            .field("number_input", &"<NumberInput>")
            .field("pin", &self.pin)
            .finish()
    }
}

impl<'a, R: text::Renderer<Font = Font>> Pwm<'a, R> {
    /// Erstelle ein Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
    pub fn neu(PwmZustand { pin, number_input_zustand }: &'a mut PwmZustand) -> Self {
        Pwm {
            number_input: NumberInput::new(number_input_zustand, *pin, 32, pwm::Serialisiert),
            pin,
        }
    }
}

impl<'a, R: text::Renderer<Font = Font>> Widget<pwm::Serialisiert, R> for Pwm<'a, R> {
    reexport_no_event_methods! {
        NumberInput<'a, u8, pwm::Serialisiert, R>,
        number_input,
        pwm::Serialisiert,
        R
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, pwm::Serialisiert>,
    ) -> event::Status {
        let mut internal_messages = Vec::new();
        let mut internal_shell = Shell::new(&mut internal_messages);
        let mut status = self.number_input.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut internal_shell,
        );
        shell.merge(internal_shell, identity);
        if let Some(pwm::Serialisiert(pin)) = internal_messages.last() {
            *self.pin = *pin;
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, R: text::Renderer<Font = Font>> From<Pwm<'a, R>> for Element<'a, pwm::Serialisiert, R> {
    fn from(auswahl: Pwm<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
