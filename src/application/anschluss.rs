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
    Button,
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

pub struct Auswahl<'a, T, M, R> {
    column: Column<'a, M, R>,
    active_tab: usize,
    pin: u8,
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    port: u8,
    modus: T,
}

impl<'a, R> Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, R>
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

impl<'a, R> Auswahl<'a, Output, InternalMessage<OutputMessage>, R>
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

impl<'a, T: 'a + Clone, M: 'static + Clone, R> Auswahl<'a, T, InternalMessage<M>, R>
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
        view_interrupt: impl FnOnce(&'a mut T, Level, Level, Level, Variante) -> Element<'a, M, R>,
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

impl<'a, R: 'a + Renderer + column::Renderer> Widget<InputAnschluss, R>
    for Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, R>
{
    reexport_no_event_methods! {Column<'a, InternalMessage<InputMessage>, R>, column, InternalMessage<InputMessage>, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
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

impl<'a, R: 'a + Renderer + column::Renderer>
    From<Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, R>>
    for Element<'a, InputAnschluss, R>
{
    fn from(auswahl: Auswahl<'a, Input<'a>, InternalMessage<InputMessage>, R>) -> Self {
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

impl<'a, R: 'a + Renderer + column::Renderer> Widget<OutputAnschluss, R>
    for Auswahl<'a, Output, InternalMessage<OutputMessage>, R>
{
    reexport_no_event_methods! {Column<'a, InternalMessage<OutputMessage>, R>, column, InternalMessage<OutputMessage>, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
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

impl<'a, R: 'a + Renderer + column::Renderer>
    From<Auswahl<'a, Output, InternalMessage<OutputMessage>, R>>
    for Element<'a, OutputAnschluss, R>
{
    fn from(auswahl: Auswahl<'a, Output, InternalMessage<OutputMessage>, R>) -> Self {
        Element::new(auswahl)
    }
}

pub struct PwmState(number_input::State, button::State);
impl PwmState {
    pub fn neu() -> Self {
        PwmState(number_input::State::new(), button::State::new())
    }
}

pub struct Pwm<'a, R: 'a + Renderer> {
    column: Column<'a, PwmMessage, R>,
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

impl<'a, R: 'a + Renderer + column::Renderer> Widget<PwmPin, R> for Pwm<'a, R> {
    reexport_no_event_methods! {Column<'a, PwmMessage, R>, column, PwmMessage, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
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

impl<'a, R: 'a + Renderer + column::Renderer> From<Pwm<'a, R>> for Element<'a, PwmPin, R> {
    fn from(auswahl: Pwm<'a, R>) -> Self {
        Element::new(auswahl)
    }
}
