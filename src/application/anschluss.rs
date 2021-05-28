//! Auswahl eines Anschlusses.

use std::collections::HashMap;

use iced_aw::native::{number_input, NumberInput, TabLabel, Tabs};
use iced_graphics::{backend, Backend, Renderer};
use iced_native::{
    button,
    event,
    layout,
    Button,
    Clipboard,
    Column,
    Element,
    Event,
    Hasher,
    Layout,
    Length,
    Point,
    Radio,
    Rectangle,
    Row,
    Text,
    Widget,
};
use log::error;
use num_x::u3;

use crate::anschluss::{
    level::Level,
    pcf8574::Variante,
    polarity::Polarity,
    pwm,
    Anschlüsse,
    Error,
    InputAnschluss,
    OutputAnschluss,
    Pin,
};
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
pub struct Input<'t>(number_input::State, u8, &'t HashMap<(Level, Level, Level, Variante), u8>);
impl<'t> Status<Input<'t>> {
    #[inline]
    pub fn neu_input(interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>) -> Self {
        Self::neu_mit_interrupt(Input(number_input::State::new(), 0, interrupt_pins))
    }
}

#[derive(Debug, Clone)]
pub struct Output(Polarity);
impl Status<Output> {
    #[inline]
    pub fn neu_output() -> Self {
        Self::neu_mit_interrupt(Output(Polarity::Normal))
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
enum Message<T> {
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

impl<'a, B: 'a + Backend + backend::Text> Auswahl<'a, Input<'a>, Message<InputMessage>, B> {
    pub fn neu_input(status: &'a mut Status<Input<'a>>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            status,
            |Input(number_input_state, pin, map), a0, a1, a2, variante| {
                map.get(&(a0, a1, a2, variante)).map_or(
                    NumberInput::new(number_input_state, *pin, 32, InputMessage::Interrupt).into(),
                    |pin| Text::new(pin.to_string()).into(),
                )
            },
        )
    }
}

impl<'a, B: 'a + Backend + backend::Text> Auswahl<'a, Output, Message<OutputMessage>, B> {
    pub fn neu_output(status: &'a mut Status<Output>) -> Self {
        Auswahl::neu_mit_interrupt_view(status, |Output(polarity), _a0, _a1, _a2, _variante| {
            Column::new()
                .push(Radio::new(
                    Polarity::Normal,
                    "Normal",
                    Some(*polarity),
                    OutputMessage::Polarity,
                ))
                .push(Radio::new(
                    Polarity::Inverse,
                    "Invertiert",
                    Some(*polarity),
                    OutputMessage::Polarity,
                ))
                .into()
        })
    }
}

impl<'a, T: 'a + Clone, M: 'static + Clone, B: 'a + Backend + backend::Text>
    Auswahl<'a, T, Message<M>, B>
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
                NumberInput::new(pin_state, *pin, 32, Message::Pin).into(),
            ),
            (TabLabel::Text("Pcf8574-Port".to_string()), {
                Row::new()
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a0), Message::A0))
                            .push(Radio::new(Level::Low, "L", Some(*a0), Message::A0)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a1), Message::A1))
                            .push(Radio::new(Level::Low, "L", Some(*a1), Message::A1)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(Level::High, "H", Some(*a2), Message::A2))
                            .push(Radio::new(Level::Low, "L", Some(*a2), Message::A2)),
                    )
                    .push(
                        Column::new()
                            .push(Radio::new(
                                Variante::Normal,
                                "Normal",
                                Some(*variante),
                                Message::Variante,
                            ))
                            .push(Radio::new(Variante::A, "A", Some(*variante), Message::Variante)),
                    )
                    .push(NumberInput::new(port_state, *port, 8, Message::Port))
                    .push(view_interrupt(modus, *a0, *a1, *a2, *variante).map(Message::Modus))
                    .into()
            }),
        ];
        let column = Column::new()
            .push(
                Tabs::with_tabs(*active_tab, tabs, Message::TabSelected)
                    .tab_bar_style(style::TabBar),
            )
            .push(Button::new(confirm_state, Text::new("Hinzufügen")).on_press(Message::Hinzufügen))
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

macro_rules! reexport_methods {
    ($type:ty, $record:ident, $message:ty) => {
        fn width(&self) -> Length {
            <$type as Widget<$message, Renderer<B>>>::width(&self.$record)
        }

        fn height(&self) -> Length {
            <$type as Widget<$message, Renderer<B>>>::height(&self.$record)
        }

        fn layout(&self, renderer: &Renderer<B>, limits: &layout::Limits) -> layout::Node {
            <$type as Widget<$message, Renderer<B>>>::layout(&self.$record, renderer, limits)
        }

        fn draw(
            &self,
            renderer: &mut Renderer<B>,
            defaults: &<Renderer<B> as iced_native::Renderer>::Defaults,
            layout: Layout<'_>,
            cursor_position: Point,
            viewport: &Rectangle,
        ) -> <Renderer<B> as iced_native::Renderer>::Output {
            <$type as Widget<$message, Renderer<B>>>::draw(
                &self.$record,
                renderer,
                defaults,
                layout,
                cursor_position,
                viewport,
            )
        }

        fn hash_layout(&self, state: &mut Hasher) {
            <$type as Widget<$message, Renderer<B>>>::hash_layout(&self.$record, state)
        }
    };
}

impl<'a, B: Backend> Widget<Result<InputAnschluss, Error>, Renderer<B>>
    for Auswahl<'a, Input<'a>, Message<InputMessage>, B>
{
    reexport_methods! {Column<'a, Message<InputMessage>, Renderer<B>>, column, Message<InputMessage>}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Result<InputAnschluss, Error>>,
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
                Message::TabSelected(tab) => self.active_tab = tab,
                Message::Pin(pin) => self.pin = pin,
                Message::A0(a0) => self.a0 = a0,
                Message::A1(a1) => self.a1 = a1,
                Message::A2(a2) => self.a2 = a2,
                Message::Variante(variante) => self.variante = variante,
                Message::Port(port) => self.port = port,
                Message::Modus(InputMessage::Interrupt(interrupt)) => self.modus.1 = interrupt,
                Message::Hinzufügen => messages.push(
                    Anschlüsse::neu().map_err(Into::into).and_then(|mut anschlüsse| {
                        if self.active_tab == 0 {
                            anschlüsse
                                .reserviere_pin(self.pin)
                                .map(Pin::into_input)
                                .map(InputAnschluss::Pin)
                                .map_err(Into::into)
                        } else {
                            anschlüsse
                                .reserviere_pcf8574_port(
                                    self.a0,
                                    self.a1,
                                    self.a2,
                                    self.variante,
                                    u3::new(self.port),
                                )
                                .map_err(Into::into)
                                .and_then(|port| port.into_input().map_err(Into::into))
                                .and_then(|mut port| {
                                    anschlüsse
                                        .reserviere_pin(self.modus.1)
                                        .map(Pin::into_input)
                                        .map_err(Into::into)
                                        .and_then(|pin| {
                                            port.set_interrupt_pin(pin).map_err(Into::into)
                                        })
                                        .map(|old_interrupt| {
                                            if let Some(interrupt) = old_interrupt {
                                                error!(
                                                    "Vorhandenen Interrupt Pin ersetzt: {:?}",
                                                    interrupt
                                                )
                                            }
                                            port
                                        })
                                })
                                .map(InputAnschluss::Pcf8574Port)
                        }
                    }),
                ),
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: Backend> Widget<Result<OutputAnschluss, Error>, Renderer<B>>
    for Auswahl<'a, Output, Message<OutputMessage>, B>
{
    reexport_methods! {Column<'a, Message<OutputMessage>, Renderer<B>>, column, Message<OutputMessage>}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Result<OutputAnschluss, Error>>,
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
                Message::TabSelected(tab) => self.active_tab = tab,
                Message::Pin(pin) => self.pin = pin,
                Message::A0(a0) => self.a0 = a0,
                Message::A1(a1) => self.a1 = a1,
                Message::A2(a2) => self.a2 = a2,
                Message::Variante(variante) => self.variante = variante,
                Message::Port(port) => self.port = port,
                Message::Modus(OutputMessage::Polarity(polarity)) => self.modus.0 = polarity,
                Message::Hinzufügen => messages.push(
                    Anschlüsse::neu().map_err(Into::into).and_then(|mut anschlüsse| {
                        if self.active_tab == 0 {
                            anschlüsse
                                .reserviere_pin(self.pin)
                                .map(Pin::into_output)
                                .map(|pin| OutputAnschluss::Pin { pin, polarität: self.modus.0 })
                                .map_err(Into::into)
                        } else {
                            anschlüsse
                                .reserviere_pcf8574_port(
                                    self.a0,
                                    self.a1,
                                    self.a2,
                                    self.variante,
                                    u3::new(self.port),
                                )
                                .map_err(Into::into)
                                .and_then(|port| port.into_output().map_err(Into::into))
                                .map(|port| OutputAnschluss::Pcf8574Port {
                                    port,
                                    polarität: self.modus.0,
                                })
                        }
                    }),
                ),
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: 'a + Backend> From<Auswahl<'a, Input<'a>, Message<InputMessage>, B>>
    for Element<'a, Result<InputAnschluss, Error>, Renderer<B>>
{
    fn from(
        auswahl: Auswahl<'a, Input<'a>, Message<InputMessage>, B>,
    ) -> Element<'a, Result<InputAnschluss, Error>, Renderer<B>> {
        Element::new(auswahl)
    }
}

impl<'a, B: 'a + Backend> From<Auswahl<'a, Output, Message<OutputMessage>, B>>
    for Element<'a, Result<OutputAnschluss, Error>, Renderer<B>>
{
    fn from(
        auswahl: Auswahl<'a, Output, Message<OutputMessage>, B>,
    ) -> Element<'a, Result<OutputAnschluss, Error>, Renderer<B>> {
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

impl<'a, B: Backend + backend::Text> Widget<Result<pwm::Pin, Error>, Renderer<B>> for Pwm<'a, B> {
    reexport_methods! {Column<'a, PwmMessage, Renderer<B>>, column, PwmMessage}

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Result<pwm::Pin, Error>>,
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
                    messages.push(Anschlüsse::neu().map_err(Into::into).and_then(
                        |mut anschlüsse| {
                            anschlüsse
                                .reserviere_pin(self.pin)
                                .map(Pin::into_pwm)
                                .map_err(Into::into)
                        },
                    ));
                },
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, B: 'a + Backend + backend::Text> From<Pwm<'a, B>>
    for Element<'a, Result<pwm::Pin, Error>, Renderer<B>>
{
    fn from(auswahl: Pwm<'a, B>) -> Element<'a, Result<pwm::Pin, Error>, Renderer<B>> {
        Element::new(auswahl)
    }
}
