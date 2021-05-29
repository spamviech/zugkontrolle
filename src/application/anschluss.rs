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

pub mod style;

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
pub enum InputMessage {
    Interrupt(u8),
}
#[derive(Debug, Clone)]
pub enum OutputMessage {
    Polarity(Polarity),
}

pub struct Auswahl<'a, T, I, M, R> {
    column: Column<'a, InternalMessage<I>, R>,
    active_tab: &'a mut usize,
    pin: &'a mut u8,
    a0: &'a mut Level,
    a1: &'a mut Level,
    a2: &'a mut Level,
    variante: &'a mut Variante,
    port: &'a mut u8,
    modus: &'a mut T,
    update_modus: &'a dyn Fn(&mut T, I),
    new_pin: &'a dyn Fn(u8, T) -> M,
    new_port: &'a dyn Fn(Level, Level, Level, Variante, u3, T) -> M,
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
            &|pin, _interrupt| InputAnschluss::Pin { pin },
            &|a0, a1, a2, variante, port, interrupt| InputAnschluss::Pcf8574Port {
                a0,
                a1,
                a2,
                variante,
                port,
                interrupt: Some(interrupt),
                /*
                // TODO
                interrupt = if interrupt_pins.get(&(a0, a1, a2, variante)).is_some() {
                    None
                } else {
                    Some(interrupt)
                },
                */
            },
        )
    }
}

impl<'a, R> Auswahl<'a, Polarity, OutputMessage, OutputAnschluss, R>
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
                        .into(),
                    polarität,
                )
            },
            &|modus, OutputMessage::Polarity(polarität)| *modus = polarität,
            &|pin, polarität| OutputAnschluss::Pin { pin, polarität },
            &|a0, a1, a2, variante, port, polarität| OutputAnschluss::Pcf8574Port {
                a0,
                a1,
                a2,
                variante,
                port,
                polarität,
            },
        )
    }
}

impl<'a, T: Copy, I: 'static + Clone, M, R> Auswahl<'a, T, I, M, R>
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
        }: &'a mut Status<IO>,
        view_interrupt: impl FnOnce(
            &'a mut IO,
            Level,
            Level,
            Level,
            Variante,
        ) -> (Element<'a, I, R>, &'a mut T),
        update_modus: &'a impl Fn(&mut T, I),
        new_pin: &'a impl Fn(u8, T) -> M,
        new_port: &'a impl Fn(Level, Level, Level, Variante, u3, T) -> M,
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
        let column = Column::new()
            .push(
                Tabs::with_tabs(*active_tab, tabs, InternalMessage::TabSelected)
                    .tab_bar_style(style::TabBar)
                    .height(Length::Shrink)
                    // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
                    .width(Length::Units(500)),
            )
            .push(
                Button::new(confirm_state, Text::new("Hinzufügen"))
                    .on_press(InternalMessage::Hinzufügen),
            );
        Auswahl {
            column,
            active_tab,
            pin,
            a0,
            a1,
            a2,
            variante,
            port,
            modus,
            update_modus,
            new_pin,
            new_port,
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

impl<'a, T: Copy, I, M, R: 'a + Renderer + column::Renderer> Widget<M, R>
    for Auswahl<'a, T, I, M, R>
{
    reexport_no_event_methods! {Column<'a, InternalMessage<I>, R>, column, InternalMessage<I>, R}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<M>,
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
                InternalMessage::TabSelected(tab) => *self.active_tab = tab,
                InternalMessage::Pin(pin) => *self.pin = pin,
                InternalMessage::A0(a0) => *self.a0 = a0,
                InternalMessage::A1(a1) => *self.a1 = a1,
                InternalMessage::A2(a2) => *self.a2 = a2,
                InternalMessage::Variante(variante) => *self.variante = variante,
                InternalMessage::Port(port) => *self.port = port,
                InternalMessage::Modus(msg) => (self.update_modus)(self.modus, msg),
                InternalMessage::Hinzufügen => messages.push(
                    if *self.active_tab == 0 {
                        (self.new_pin)(*self.pin, *self.modus)
                    } else {
                        (self.new_port)(
                            *self.a0,
                            *self.a1,
                            *self.a2,
                            *self.variante,
                            u3::new(*self.port),
                            *self.modus,
                        )
                    },
                ),
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, T: Copy, I, M, R: 'a + Renderer + column::Renderer> From<Auswahl<'a, T, I, M, R>>
    for Element<'a, M, R>
{
    fn from(auswahl: Auswahl<'a, T, I, M, R>) -> Self {
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
