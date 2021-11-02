//! Auswahl eines Anschlusses.

use std::{collections::HashMap, fmt::Debug};

use iced_aw::native::{number_input, tab_bar, tabs, NumberInput, TabLabel, Tabs};
use iced_native::{
    button, column, container, event, radio, row, text, Clipboard, Column, Element, Event, Layout,
    Length, Point, Radio, Renderer, Row, Text, Widget,
};

use crate::{
    anschluss::{
        level::Level, pcf8574::Variante, pin::pwm, polarität::Polarität, InputSerialisiert,
        OutputSerialisiert,
    },
    application::{macros::reexport_no_event_methods, style::tab_bar::TabBar},
};

/// Status eines Widgets zur Auswahl eines Anschlusses.
#[derive(Debug)]
pub struct Status<T> {
    active_tab: usize,
    pin_state: number_input::State,
    pin: u8,
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    port_state: number_input::State,
    port: u8,
    modus: T,
}

#[derive(Debug, Clone)]
pub struct Input<'t> {
    number_input_state: number_input::State,
    pin: u8,
    interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>,
}
impl<'t> Status<Input<'t>> {
    #[inline(always)]
    pub fn neu_input(interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>) -> Self {
        Self::neu_mit_interrupt(Input {
            number_input_state: number_input::State::new(),
            pin: 0,
            interrupt_pins,
        })
    }

    #[inline(always)]
    pub fn von_input_save(
        initial: InputSerialisiert,
        interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>,
    ) -> Self {
        let make_modus =
            |pin: u8| Input { number_input_state: number_input::State::new(), pin, interrupt_pins };
        match initial {
            InputSerialisiert::Pin { pin } => Self::neu_mit_initial_pin(pin, make_modus(0)),
            InputSerialisiert::Pcf8574Port { a0, a1, a2, variante, port, interrupt } => {
                Self::neu_mit_initial_port(
                    a0,
                    a1,
                    a2,
                    variante,
                    port,
                    make_modus(interrupt.unwrap_or(0)),
                )
            }
        }
    }

    #[inline(always)]
    pub fn input_anschluss(&self) -> InputSerialisiert {
        self.anschluss(
            |pin, _input| InputSerialisiert::Pin { pin },
            |a0, a1, a2, variante, port, Input { pin, interrupt_pins, .. }| {
                InputSerialisiert::Pcf8574Port {
                    a0,
                    a1,
                    a2,
                    variante,
                    port,
                    interrupt: if interrupt_pins.get(&(a0, a1, a2, variante)).is_some() {
                        None
                    } else {
                        Some(*pin)
                    },
                }
            },
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Output {
    polarität: Polarität,
}
impl Status<Output> {
    #[inline(always)]
    pub fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output { polarität: Polarität::Normal })
    }

    #[inline(always)]
    pub fn von_output_save(initial: OutputSerialisiert) -> Self {
        match initial {
            OutputSerialisiert::Pin { pin, polarität } => {
                Self::neu_mit_initial_pin(pin, Output { polarität })
            }
            OutputSerialisiert::Pcf8574Port { a0, a1, a2, variante, port, polarität } => {
                Self::neu_mit_initial_port(a0, a1, a2, variante, port, Output { polarität })
            }
        }
    }

    #[inline(always)]
    pub fn output_anschluss(&self) -> OutputSerialisiert {
        self.anschluss(
            |pin, Output { polarität }| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |a0, a1, a2, variante, port, Output { polarität }| OutputSerialisiert::Pcf8574Port {
                a0,
                a1,
                a2,
                variante,
                port,
                polarität: *polarität,
            },
        )
    }
}

impl<T> Status<T> {
    fn anschluss<M>(
        &self,
        make_pin: impl Fn(u8, &T) -> M,
        make_port: impl Fn(Level, Level, Level, Variante, u8, &T) -> M,
    ) -> M {
        if self.active_tab == 0 {
            make_pin(self.pin, &self.modus)
        } else {
            make_port(self.a0, self.a1, self.a2, self.variante, self.port, &self.modus)
        }
    }

    fn neu_mit_interrupt(modus: T) -> Self {
        Status {
            active_tab: 0,
            pin_state: number_input::State::new(),
            pin: 0,
            a0: Level::Low,
            a1: Level::Low,
            a2: Level::Low,
            variante: Variante::Normal,
            port_state: number_input::State::new(),
            port: 0,
            modus,
        }
    }

    fn neu_mit_initial_pin(pin: u8, modus: T) -> Self {
        Status {
            active_tab: 0,
            pin_state: number_input::State::new(),
            pin,
            a0: Level::Low,
            a1: Level::Low,
            a2: Level::Low,
            variante: Variante::Normal,
            port_state: number_input::State::new(),
            port: 0,
            modus,
        }
    }

    fn neu_mit_initial_port(
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        port: u8,
        modus: T,
    ) -> Self {
        Status {
            active_tab: 1,
            pin_state: number_input::State::new(),
            pin: 0,
            a0,
            a1,
            a2,
            variante,
            port_state: number_input::State::new(),
            port,
            modus,
        }
    }
}

#[derive(Debug, Clone)]
enum InternalMessage<T> {
    TabSelected(usize),
    Pin(u8),
    A0(Level),
    A1(Level),
    A2(Level),
    Variante(Variante),
    Port(u8),
    Modus(T),
}
#[derive(Debug, Clone, Copy)]
pub enum InputMessage {
    Interrupt(u8),
}
#[derive(Debug, Clone, Copy)]
pub enum OutputMessage {
    Polarität(Polarität),
}

pub struct Auswahl<'a, T, I, M, R: row::Renderer> {
    row: Row<'a, InternalMessage<I>, R>,
    active_tab: &'a mut usize,
    pin: &'a mut u8,
    a0: &'a mut Level,
    a1: &'a mut Level,
    a2: &'a mut Level,
    variante: &'a mut Variante,
    port: &'a mut u8,
    modus: &'a mut T,
    update_modus: &'a dyn Fn(&mut T, I),
    make_pin: &'a dyn Fn(u8, &T) -> M,
    make_port: Box<dyn Fn(Level, Level, Level, Variante, u8, &T) -> M>,
}

impl<T: Debug, I, M, R: row::Renderer> Debug for Auswahl<'_, T, I, M, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Auswahl")
            .field("row", &"<Row>")
            .field("active_tab", &self.active_tab)
            .field("pin", &self.pin)
            .field("a0", &self.a0)
            .field("a1", &self.a1)
            .field("a2", &self.a2)
            .field("variante", &self.variante)
            .field("port", &self.port)
            .field("modus", &self.modus)
            .field("update_modus", &"<closure>")
            .field("make_pin", &"<closure>")
            .field("make_port", &"<closure>")
            .finish()
    }
}

impl<'a, R> Auswahl<'a, u8, InputMessage, InputSerialisiert, R>
where
    R: 'a
        + Renderer
        + text::Renderer
        + radio::Renderer
        + column::Renderer
        + row::Renderer
        + container::Renderer
        + button::Renderer
        + number_input::Renderer
        + tabs::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    pub fn neu_input(status: &'a mut Status<Input<'a>>) -> Self {
        let interrupt_pins = status.modus.interrupt_pins.clone();
        Auswahl::neu_mit_interrupt_view(
            status,
            ZeigeModus::Pcf8574,
            |Input { number_input_state, pin, interrupt_pins }, a0, a1, a2, variante| {
                (
                    interrupt_pins.get(&(a0, a1, a2, variante)).map_or(
                        NumberInput::new(number_input_state, *pin, 32, InputMessage::Interrupt)
                            .into(),
                        |pin| Text::new(pin.to_string()).into(),
                    ),
                    pin,
                )
            },
            &|modus: &mut u8, InputMessage::Interrupt(pin)| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            move |a0, a1, a2, variante, port, pin| InputSerialisiert::Pcf8574Port {
                a0,
                a1,
                a2,
                variante,
                port,
                interrupt: if interrupt_pins.get(&(a0, a1, a2, variante)).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
        )
    }
}

impl<'a, R> Auswahl<'a, Polarität, OutputMessage, OutputSerialisiert, R>
where
    R: 'a
        + Renderer
        + text::Renderer
        + radio::Renderer
        + column::Renderer
        + row::Renderer
        + container::Renderer
        + button::Renderer
        + number_input::Renderer
        + tabs::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    pub fn neu_output(status: &'a mut Status<Output>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            status,
            ZeigeModus::Beide,
            |Output { polarität }, _a0, _a1, _a2, _variante| {
                (
                    Column::new()
                        .push(Radio::new(
                            Polarität::Normal,
                            "Normal",
                            Some(*polarität),
                            OutputMessage::Polarität,
                        ))
                        .push(Radio::new(
                            Polarität::Invertiert,
                            "Invertiert",
                            Some(*polarität),
                            OutputMessage::Polarität,
                        ))
                        .into(),
                    polarität,
                )
            },
            &|modus, OutputMessage::Polarität(polarität)| *modus = polarität,
            &|pin, polarität| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |a0, a1, a2, variante, port, polarität| OutputSerialisiert::Pcf8574Port {
                a0,
                a1,
                a2,
                variante,
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
    T: Copy + Eq,
    M: 'a + Clone,
    R: 'a + column::Renderer + row::Renderer + text::Renderer + radio::Renderer,
{
    Column::new()
        .push(Radio::new(fst, fst_s, Some(current.clone()), to_message.clone()).spacing(0))
        .push(Radio::new(snd, snd_s, Some(current.clone()), to_message).spacing(0))
}

impl<'a, T, I: 'static + Clone, M, R> Auswahl<'a, T, I, M, R>
where
    R: 'a
        + Renderer
        + text::Renderer
        + radio::Renderer
        + column::Renderer
        + row::Renderer
        + container::Renderer
        + button::Renderer
        + number_input::Renderer
        + tabs::Renderer,
    <R as tab_bar::Renderer>::Style: From<TabBar>,
{
    fn neu_mit_interrupt_view<IO>(
        Status {
            active_tab,
            pin_state,
            pin,
            a0,
            a1,
            a2,
            variante,
            port_state,
            port,
            modus,
        }: &'a mut Status<IO>,
        zeige_modus: ZeigeModus,
        view_modus: impl FnOnce(
            &'a mut IO,
            Level,
            Level,
            Level,
            Variante,
        ) -> (Element<'a, I, R>, &'a mut T),
        update_modus: &'a impl Fn(&mut T, I),
        make_pin: &'a impl Fn(u8, &T) -> M,
        make_port: impl 'static + Fn(Level, Level, Level, Variante, u8, &T) -> M,
    ) -> Self {
        let (view_modus, modus) = view_modus(modus, *a0, *a1, *a2, *variante);
        let view_modus_mapped = view_modus.map(InternalMessage::Modus);
        let high_low_column = |level: &Level, to_message: fn(Level) -> InternalMessage<I>| {
            make_radios(level, Level::High, "H", Level::Low, "L", to_message)
        };
        let pcf8574_row = Row::new()
            .push(high_low_column(a0, InternalMessage::A0))
            .push(high_low_column(a1, InternalMessage::A1))
            .push(high_low_column(a2, InternalMessage::A2))
            .push(make_radios(
                variante,
                Variante::Normal,
                "Normal",
                Variante::A,
                "A",
                InternalMessage::Variante,
            ))
            .push(NumberInput::new(port_state, *port, 7, InternalMessage::Port));
        // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
        let width = Length::Units(350);
        let row = match zeige_modus {
            ZeigeModus::Pcf8574 => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_string()),
                        NumberInput::new(pin_state, *pin, 32, InternalMessage::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_string()), {
                        pcf8574_row.push(view_modus_mapped).into()
                    }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InternalMessage::TabSelected)
                    .tab_bar_style(TabBar)
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs)
            }
            ZeigeModus::Beide => {
                let tabs = vec![
                    (
                        TabLabel::Text("Pin".to_string()),
                        NumberInput::new(pin_state, *pin, 32, InternalMessage::Pin).into(),
                    ),
                    (TabLabel::Text("Pcf8574-Port".to_string()), { pcf8574_row.into() }),
                ];
                let tabs = Tabs::with_tabs(*active_tab, tabs, InternalMessage::TabSelected)
                    .tab_bar_style(TabBar)
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            }
        };
        Auswahl {
            row,
            active_tab,
            pin,
            a0,
            a1,
            a2,
            variante,
            port,
            modus,
            update_modus,
            make_pin,
            make_port: Box::new(make_port),
        }
    }
}

impl<'a, T, I, M, R> Widget<M, R> for Auswahl<'a, T, I, M, R>
where
    T: Copy,
    R: 'a + Renderer + text::Renderer + column::Renderer + row::Renderer + tabs::Renderer,
{
    reexport_no_event_methods! {Row<'a, InternalMessage<I>, R>, row, InternalMessage<I>, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<M>,
    ) -> event::Status {
        let mut internal_messages = Vec::new();
        let mut status = self.row.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut internal_messages,
        );
        let mut changed = false;
        for message in internal_messages {
            changed = true;
            match message {
                InternalMessage::TabSelected(tab) => *self.active_tab = tab,
                InternalMessage::Pin(pin) => *self.pin = pin,
                InternalMessage::A0(a0) => *self.a0 = a0,
                InternalMessage::A1(a1) => *self.a1 = a1,
                InternalMessage::A2(a2) => *self.a2 = a2,
                InternalMessage::Variante(variante) => *self.variante = variante,
                InternalMessage::Port(port) => *self.port = port,
                InternalMessage::Modus(msg) => (self.update_modus)(self.modus, msg),
            }
            status = event::Status::Captured;
        }
        if changed {
            messages.push(if *self.active_tab == 0 {
                (self.make_pin)(*self.pin, &self.modus)
            } else {
                (self.make_port)(
                    *self.a0,
                    *self.a1,
                    *self.a2,
                    *self.variante,
                    *self.port,
                    self.modus,
                )
            })
        }
        status
    }
}

impl<'a, T, I, M, R> From<Auswahl<'a, T, I, M, R>> for Element<'a, M, R>
where
    T: Copy,
    R: 'a + Renderer + text::Renderer + column::Renderer + row::Renderer + tabs::Renderer,
{
    fn from(auswahl: Auswahl<'a, T, I, M, R>) -> Self {
        Element::new(auswahl)
    }
}

#[derive(Debug)]
pub struct PwmState {
    pin: u8,
    number_input_state: number_input::State,
}
impl PwmState {
    pub fn neu() -> Self {
        PwmState { pin: 0, number_input_state: number_input::State::new() }
    }
}

pub struct Pwm<'a, R: 'a + Renderer + number_input::Renderer> {
    number_input: NumberInput<'a, u8, pwm::Serialisiert, R>,
    pin: &'a mut u8,
}

impl<'a, R: 'a + Renderer + number_input::Renderer> Debug for Pwm<'a, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Pwm")
            .field("number_input", &"<NumberInput>")
            .field("pin", &self.pin)
            .finish()
    }
}

impl<'a, R> Pwm<'a, R>
where
    R: 'a
        + Renderer
        + button::Renderer
        + text::Renderer
        + column::Renderer
        + row::Renderer
        + container::Renderer
        + number_input::Renderer,
{
    pub fn neu(PwmState { pin, number_input_state }: &'a mut PwmState) -> Self {
        Pwm { number_input: NumberInput::new(number_input_state, *pin, 32, pwm::Serialisiert), pin }
    }
}

impl<'a, R> Widget<pwm::Serialisiert, R> for Pwm<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + number_input::Renderer,
{
    reexport_no_event_methods! {NumberInput<'a, u8, pwm::Serialisiert, R>, number_input, pwm::Serialisiert, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<pwm::Serialisiert>,
    ) -> event::Status {
        let mut status = self.number_input.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            messages,
        );
        if let Some(pwm::Serialisiert(pin)) = messages.last() {
            *self.pin = *pin;
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, R> From<Pwm<'a, R>> for Element<'a, pwm::Serialisiert, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + number_input::Renderer,
{
    fn from(auswahl: Pwm<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
