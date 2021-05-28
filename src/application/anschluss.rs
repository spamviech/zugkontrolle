//! Auswahl eines Anschlusses.

use std::collections::HashMap;

use iced_aw::native::{number_input, NumberInput, TabLabel, Tabs};
use iced_graphics::{backend, Backend, Renderer};
use iced_native::{
    button,
    event,
    Button,
    Clipboard,
    Column,
    Element,
    Event,
    Layout,
    Length,
    Point,
    Radio,
    Row,
    Text,
    Widget,
};
use num_x::u3;

use super::macros::reexport_no_event_methods;
use crate::anschluss::{level::Level, pcf8574::Variante, polarity::Polarity};

mod style;

/// Status eines Widgets zur Auswahl eines Anschlusses.
#[derive(Debug)]
pub struct Status<T> {
    active_tab: usize,
    confirm_state: button::State,
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
}

#[derive(Debug, Clone)]
pub struct Output {
    polarität: Polarity,
}
impl Status<Output> {
    #[inline]
    pub fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output { polarität: Polarity::Normal })
    }
}

impl<T> Status<T> {
    fn neu_mit_interrupt(modus: T) -> Self {
        Status {
            active_tab: 0,
            confirm_state: button::State::new(),
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
    Hinzufügen,
}
#[derive(Debug, Clone)]
enum InputMessage {
    Interrupt(u8),
}
#[derive(Debug, Clone)]
enum OutputMessage {
    Polarity(Polarity),
}

pub struct Auswahl<'a, T, M, B: Backend> {
    column: Column<'a, M, Renderer<B>>,
    active_tab: usize,
    pin: u8,
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    port: u8,
    modus: T,
}

impl<'a, B: 'a + Backend + backend::Text> Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, B> {
    pub fn neu_input(status: &'a mut Status<Input<'a>>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            status,
            |Input { number_input_state, pin, interrupt_pins }, a0, a1, a2, variante| {
                interrupt_pins.get(&(a0, a1, a2, variante)).map_or(
                    NumberInput::new(number_input_state, *pin, 32, InputMessage::Interrupt).into(),
                    |pin| Text::new(pin.to_string()).into(),
                )
            },
        )
    }
}

impl<'a, B: 'a + Backend + backend::Text> Auswahl<'a, Output, InternalMessage<OutputMessage>, B> {
    pub fn neu_output(status: &'a mut Status<Output>) -> Self {
        Auswahl::neu_mit_interrupt_view(status, |Output { polarität }, _a0, _a1, _a2, _variante| {
            Column::new()
                .push(Radio::new(
                    Polarity::Normal,
                    "Normal",
                    Some(*polarität),
                    OutputMessage::Polarity,
                ))
                .push(Radio::new(
                    Polarity::Inverse,
                    "Invertiert",
                    Some(*polarität),
                    OutputMessage::Polarity,
                ))
                .into()
        })
    }
}

impl<'a, T: 'a + Clone, M: 'static + Clone, B: 'a + Backend + backend::Text>
    Auswahl<'a, T, InternalMessage<M>, B>
{
    fn neu_mit_interrupt_view(
        Status {
            active_tab,
            confirm_state,
            pin_state,
            pin,
            a0,
            a1,
            a2,
            variante,
            port_state,
            port,
            modus,
        }: &'a mut Status<T>,
        view_interrupt: impl FnOnce(
            &'a mut T,
            Level,
            Level,
            Level,
            Variante,
        ) -> Element<'a, M, Renderer<B>>,
    ) -> Self {
        let modus_clone = modus.clone();
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
                    .push(
                        view_interrupt(modus, *a0, *a1, *a2, *variante).map(InternalMessage::Modus),
                    )
                    .into()
            }),
        ];
        let column = Column::new()
            .push(
                Tabs::with_tabs(*active_tab, tabs, InternalMessage::TabSelected)
                    .tab_bar_style(style::TabBar),
            )
            .push(
                Button::new(confirm_state, Text::new("Hinzufügen"))
                    .on_press(InternalMessage::Hinzufügen),
            )
            .width(Length::Fill)
            .height(Length::Fill);
        Auswahl {
            column,
            active_tab: *active_tab,
            pin: *pin,
            a0: *a0,
            a1: *a1,
            a2: *a2,
            variante: *variante,
            port: *port,
            modus: modus_clone,
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

impl<'a, B: Backend> Widget<InputAnschluss, Renderer<B>>
    for Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, B>
{
    reexport_no_event_methods! {Column<'a, InternalMessage<InputMessage>, Renderer<B>>, column, InternalMessage<InputMessage>, Renderer<B>}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<InputAnschluss>,
    ) -> event::Status {
        let mut column_messages = Vec::new();
        let mut status = self.column.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut column_messages,
        );
        for message in column_messages {
            match message {
                InternalMessage::TabSelected(tab) => self.active_tab = tab,
                InternalMessage::Pin(pin) => self.pin = pin,
                InternalMessage::A0(a0) => self.a0 = a0,
                InternalMessage::A1(a1) => self.a1 = a1,
                InternalMessage::A2(a2) => self.a2 = a2,
                InternalMessage::Variante(variante) => self.variante = variante,
                InternalMessage::Port(port) => self.port = port,
                InternalMessage::Modus(InputMessage::Interrupt(interrupt)) => {
                    self.modus.pin = interrupt
                },
                InternalMessage::Hinzufügen => messages.push(
                    if self.active_tab == 0 {
                        InputAnschluss::Pin { pin: self.pin }
                    } else {
                        InputAnschluss::Pcf8574Port {
                            a0: self.a0,
                            a1: self.a1,
                            a2: self.a2,
                            variante: self.variante,
                            port: u3::new(self.port),
                            interrupt: if self
                                .modus
                                .interrupt_pins
                                .get(&(self.a0, self.a1, self.a2, self.variante))
                                .is_some()
                            {
                                None
                            } else {
                                Some(self.modus.pin)
                            },
                        }
                    },
                ),
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: 'a + Backend> From<Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, B>>
    for Element<'a, InputAnschluss, Renderer<B>>
{
    fn from(auswahl: Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, B>) -> Self {
        Element::new(auswahl)
    }
}

#[derive(Debug, Clone)]
pub enum OutputAnschluss {
    Pin {
        pin: u8,
        polarität: Polarity,
    },
    Pcf8574Port {
        a0: Level,
        a1: Level,
        a2: Level,
        variante: Variante,
        port: u3,
        polarität: Polarity,
    },
}

impl<'a, B: Backend> Widget<OutputAnschluss, Renderer<B>>
    for Auswahl<'a, Output, InternalMessage<OutputMessage>, B>
{
    reexport_no_event_methods! {Column<'a, InternalMessage<OutputMessage>, Renderer<B>>, column, InternalMessage<OutputMessage>, Renderer<B>}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<OutputAnschluss>,
    ) -> event::Status {
        let mut column_messages = Vec::new();
        let mut status = self.column.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut column_messages,
        );
        for message in column_messages {
            match message {
                InternalMessage::TabSelected(tab) => self.active_tab = tab,
                InternalMessage::Pin(pin) => self.pin = pin,
                InternalMessage::A0(a0) => self.a0 = a0,
                InternalMessage::A1(a1) => self.a1 = a1,
                InternalMessage::A2(a2) => self.a2 = a2,
                InternalMessage::Variante(variante) => self.variante = variante,
                InternalMessage::Port(port) => self.port = port,
                InternalMessage::Modus(OutputMessage::Polarity(polarität)) => {
                    self.modus.polarität = polarität
                },
                InternalMessage::Hinzufügen => messages.push(
                    if self.active_tab == 0 {
                        OutputAnschluss::Pin { pin: self.pin, polarität: self.modus.polarität }
                    } else {
                        OutputAnschluss::Pcf8574Port {
                            a0: self.a0,
                            a1: self.a1,
                            a2: self.a2,
                            variante: self.variante,
                            port: u3::new(self.port),
                            polarität: self.modus.polarität,
                        }
                    },
                ),
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: 'a + Backend> From<Auswahl<'a, Output, InternalMessage<OutputMessage>, B>>
    for Element<'a, OutputAnschluss, Renderer<B>>
{
    fn from(auswahl: Auswahl<'a, Output, InternalMessage<OutputMessage>, B>) -> Self {
        Element::new(auswahl)
    }
}

pub struct PwmState(number_input::State, button::State);
impl PwmState {
    pub fn neu() -> Self {
        PwmState(number_input::State::new(), button::State::new())
    }
}

pub struct Pwm<'a, B: Backend + backend::Text> {
    column: Column<'a, PwmMessage, Renderer<B>>,
    pin: u8,
}

#[derive(Debug, Clone)]
enum PwmMessage {
    Pin(u8),
    Hinzufügen,
}

pub struct PwmPin {
    pub pin: u8,
}

impl<'a, B: 'a + Backend + backend::Text> Pwm<'a, B> {
    pub fn neu(PwmState(number_input_state, button_state): &'a mut PwmState) -> Self {
        Pwm {
            column: Column::new()
                .push(NumberInput::new(number_input_state, 0, 32, PwmMessage::Pin))
                .push(
                    Button::new(button_state, Text::new("Hinzufügen"))
                        .on_press(PwmMessage::Hinzufügen),
                ),
            pin: 0,
        }
    }
}

impl<'a, B: Backend + backend::Text> Widget<PwmPin, Renderer<B>> for Pwm<'a, B> {
    reexport_no_event_methods! {Column<'a, PwmMessage, Renderer<B>>, column, PwmMessage, Renderer<B>}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<PwmPin>,
    ) -> event::Status {
        let mut column_messages = Vec::new();
        let mut status = self.column.on_event(
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            &mut column_messages,
        );
        for message in column_messages {
            match message {
                PwmMessage::Pin(pin) => self.pin = pin,
                PwmMessage::Hinzufügen => {
                    messages.push(PwmPin { pin: self.pin });
                },
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: 'a + Backend + backend::Text> From<Pwm<'a, B>> for Element<'a, PwmPin, Renderer<B>> {
    fn from(auswahl: Pwm<'a, B>) -> Self {
        Element::new(auswahl)
    }
}
