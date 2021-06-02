//! Auswahl eines Anschlusses.

use std::collections::HashMap;

use iced_aw::native::{number_input, tab_bar, tabs, NumberInput, TabLabel, Tabs};
use iced_native::{
    button,
    column,
    container,
    event,
    radio,
    row,
    text,
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
    Text,
    Widget,
};
use num_x::u3;

use super::macros::reexport_no_event_methods;
use crate::anschluss::{level::Level, pcf8574::Variante, polarity::Polarität};

pub mod style;

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
    #[inline]
    pub fn neu_input(interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>) -> Self {
        Self::neu_mit_interrupt(Input {
            number_input_state: number_input::State::new(),
            pin: 0,
            interrupt_pins,
        })
    }

    #[inline]
    pub fn input_anschluss(&self) -> InputAnschluss {
        self.anschluss(
            |pin, _input| InputAnschluss::Pin { pin },
            |a0, a1, a2, variante, port, Input { pin, interrupt_pins, .. }| {
                InputAnschluss::Pcf8574Port {
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

#[derive(Debug, Clone)]
pub struct Output {
    polarität: Polarität,
}
impl Status<Output> {
    #[inline]
    pub fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output { polarität: Polarität::Normal })
    }

    #[inline]
    pub fn output_anschluss(&self) -> OutputAnschluss {
        self.anschluss(
            |pin, Output { polarität }| OutputAnschluss::Pin { pin, polarität: *polarität },
            |a0, a1, a2, variante, port, Output { polarität }| OutputAnschluss::Pcf8574Port {
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
        make_port: impl Fn(Level, Level, Level, Variante, u3, &T) -> M,
    ) -> M {
        if self.active_tab == 0 {
            make_pin(self.pin, &self.modus)
        } else {
            make_port(self.a0, self.a1, self.a2, self.variante, u3::new(self.port), &self.modus)
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
#[derive(Debug, Clone)]
pub enum InputMessage {
    Interrupt(u8),
}
#[derive(Debug, Clone)]
pub enum OutputMessage {
    Polarität(Polarität),
}

pub struct Auswahl<'a, T, I, M, R: tabs::Renderer> {
    tabs: Tabs<'a, InternalMessage<I>, R>,
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
    make_port: Box<dyn Fn(Level, Level, Level, Variante, u3, &T) -> M>,
}

impl<'a, R> Auswahl<'a, u8, InputMessage, InputAnschluss, R>
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
    <R as tab_bar::Renderer>::Style: From<style::TabBar>,
{
    pub fn neu_input(status: &'a mut Status<Input<'a>>) -> Self {
        let interrupt_pins = status.modus.interrupt_pins.clone();
        Auswahl::neu_mit_interrupt_view(
            status,
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
            &|pin, _input| InputAnschluss::Pin { pin },
            move |a0, a1, a2, variante, port, pin| InputAnschluss::Pcf8574Port {
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

impl<'a, R> Auswahl<'a, Polarität, OutputMessage, OutputAnschluss, R>
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
    <R as tab_bar::Renderer>::Style: From<style::TabBar>,
{
    pub fn neu_output(status: &'a mut Status<Output>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            status,
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
            &|pin, polarität| OutputAnschluss::Pin { pin, polarität: *polarität },
            |a0, a1, a2, variante, port, polarität| OutputAnschluss::Pcf8574Port {
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
    <R as tab_bar::Renderer>::Style: From<style::TabBar>,
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
        view_interrupt: impl FnOnce(
            &'a mut IO,
            Level,
            Level,
            Level,
            Variante,
        ) -> (Element<'a, I, R>, &'a mut T),
        update_modus: &'a impl Fn(&mut T, I),
        make_pin: &'a impl Fn(u8, &T) -> M,
        make_port: impl 'static + Fn(Level, Level, Level, Variante, u3, &T) -> M,
    ) -> Self {
        let (view_interrupt, modus) = view_interrupt(modus, *a0, *a1, *a2, *variante);
        let tabs = vec![
            (
                TabLabel::Text("Pin".to_string()),
                NumberInput::new(pin_state, *pin, 32, InternalMessage::Pin).into(),
            ),
            (TabLabel::Text("Pcf8574-Port".to_string()), {
                Row::new()
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a0), InternalMessage::A0))
                            .push(Radio::new(Level::Low, "L", Some(*a0), InternalMessage::A0)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a1), InternalMessage::A1))
                            .push(Radio::new(Level::Low, "L", Some(*a1), InternalMessage::A1)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a2), InternalMessage::A2))
                            .push(Radio::new(Level::Low, "L", Some(*a2), InternalMessage::A2)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(
                                Variante::Normal,
                                "Normal",
                                Some(*variante),
                                InternalMessage::Variante,
                            ))
                            .push(Radio::new(
                                Variante::A,
                                "A",
                                Some(*variante),
                                InternalMessage::Variante,
                            )),
                    )
                    .push(NumberInput::new(port_state, *port, 8, InternalMessage::Port))
                    .push(view_interrupt.map(InternalMessage::Modus))
                    .into()
            }),
        ];
        let tabs = Tabs::with_tabs(*active_tab, tabs, InternalMessage::TabSelected)
                    .tab_bar_style(style::TabBar)
                    .height(Length::Shrink)
                    // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
                    .width(Length::Units(500));
        Auswahl {
            tabs,
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

#[derive(Debug, Clone)]
pub enum InputAnschluss {
    Pin {
        pin: u8,
    },
    Pcf8574Port {
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        port: u3,
        interrupt: Option<u8>,
    },
}

#[derive(Debug, Clone)]
pub enum OutputAnschluss {
    Pin {
        pin: u8,
        polarität: Polarität,
    },
    Pcf8574Port {
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        port: u3,
        polarität: Polarität,
    },
}

impl<'a, T, I, M, R> Widget<M, R> for Auswahl<'a, T, I, M, R>
where
    T: Copy,
    R: 'a + Renderer + text::Renderer + column::Renderer + row::Renderer + tabs::Renderer,
{
    reexport_no_event_methods! {Tabs<'a, InternalMessage<I>, R>, tabs, InternalMessage<I>, R}

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
        let mut status = self.tabs.on_event(
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
            messages.push(
                if *self.active_tab == 0 {
                    (self.make_pin)(*self.pin, &self.modus)
                } else {
                    (self.make_port)(
                        *self.a0,
                        *self.a1,
                        *self.a2,
                        *self.variante,
                        u3::new(*self.port),
                        self.modus,
                    )
                },
            )
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
    number_input: NumberInput<'a, u8, PwmPin, R>,
    pin: &'a mut u8,
}

#[derive(Debug, Clone)]
pub struct PwmPin(pub u8);

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
        Pwm { number_input: NumberInput::new(number_input_state, 0, 32, PwmPin), pin }
    }
}

impl<'a, R> Widget<PwmPin, R> for Pwm<'a, R>
where
    R: 'a
        + Renderer
        + container::Renderer
        + column::Renderer
        + row::Renderer
        + number_input::Renderer,
{
    reexport_no_event_methods! {NumberInput<'a, u8, PwmPin, R>, number_input, PwmPin, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<PwmPin>,
    ) -> event::Status {
        let mut status = self.number_input.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            messages,
        );
        if let Some(PwmPin(pin)) = messages.last() {
            *self.pin = *pin;
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, R> From<Pwm<'a, R>> for Element<'a, PwmPin, R>
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
