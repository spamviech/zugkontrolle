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
};
use num_x::u3;

use crate::anschluss::{
    anschlüsse::Error, level::Level, pcf8574::Variante, Anschluss, Anschlüsse
};
mod style;

// TODO direkt Input/Output/Pwm-Varianten
#[derive(Debug)]
pub struct Auswahl<T> {
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
    interrupt: T,
}

#[derive(Debug, Clone)]
enum Message {
    TabSelected(usize),
    Pin(u8),
    A0(Level),
    A1(Level),
    A2(Level),
    Variante(Variante),
    Port(u8),
    Interrupt(u8),
    Hinzufügen,
}

#[derive(Debug, Clone)]
pub struct Input<'t>(number_input::State, u8, &'t HashMap<(Level, Level, Level, Variante), u8>);
impl<'t> Auswahl<Input<'t>> {
    #[inline]
    pub fn neu(interrupt_pins: &'t HashMap<(Level, Level, Level, Variante), u8>) -> Self {
        Self::neu_mit_interrupt(Input(number_input::State::new(), 0, interrupt_pins))
    }
}

#[derive(Debug, Clone)]
pub struct Output;
impl Auswahl<Output> {
    #[inline]
    pub fn neu() -> Self {
        Self::neu_mit_interrupt(Output)
    }
}

impl<T> Auswahl<T> {
    fn neu_mit_interrupt(interrupt: T) -> Self {
        Auswahl {
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
            interrupt,
        }
    }
}

pub struct Widget<'a, T, B: Backend> {
    column: Column<'a, Message, Renderer<B>>,
    active_tab: usize,
    pin: u8,
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Variante,
    port: u8,
    interrupt: T,
}

impl<'a, B: 'a + Backend + backend::Text> Widget<'a, Input<'a>, B> {
    pub fn neu(auswahl: &'a mut Auswahl<Input<'a>>) -> Self {
        Widget::neu_mit_interrupt_view(
            auswahl,
            |Input(number_input_state, pin, map), a0, a1, a2, variante| {
                Some(map.get(&(a0, a1, a2, variante)).map_or(
                    NumberInput::new(number_input_state, *pin, 32, Message::Interrupt).into(),
                    |pin| Text::new(pin.to_string()).into(),
                ))
            },
        )
    }
}

impl<'a, B: 'a + Backend + backend::Text> Widget<'a, Output, B> {
    pub fn neu(auswahl: &'a mut Auswahl<Output>) -> Self {
        Widget::neu_mit_interrupt_view(auswahl, |Output, _a0, _a1, _a2, _variante| None)
    }
}

impl<'a, T: 'a + Clone, B: 'a + Backend + backend::Text> Widget<'a, T, B> {
    fn neu_mit_interrupt_view(
        Auswahl {
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
            interrupt,
        }: &'a mut Auswahl<T>,
        view_interrupt: impl FnOnce(
            &'a mut T,
            Level,
            Level,
            Level,
            Variante,
        ) -> Option<Element<'a, Message, Renderer<B>>>,
    ) -> Self {
        let interrupt_clone = interrupt.clone();
        let tabs = vec![
            (
                TabLabel::Text("Pin".to_string()),
                NumberInput::new(pin_state, *pin, 32, Message::Pin).into(),
            ),
            (TabLabel::Text("Pcf8574-Port".to_string()), {
                let row = Row::new()
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
                    .push(NumberInput::new(port_state, *port, 8, Message::Port));
                match view_interrupt(interrupt, *a0, *a1, *a2, *variante) {
                    None => row,
                    Some(element) => row.push(element),
                }
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
        Widget {
            column,
            active_tab: *active_tab,
            pin: *pin,
            a0: *a0,
            a1: *a1,
            a2: *a2,
            variante: *variante,
            port: *port,
            interrupt: interrupt_clone,
        }
    }
}

impl<'a, T, B: Backend> iced_native::Widget<Result<Anschluss, Error>, Renderer<B>>
    for Widget<'a, T, B>
{
    fn width(&self) -> Length {
        <Column<'a, Message, Renderer<B>> as iced_native::Widget<Message, Renderer<B>>>::width(
            &self.column,
        )
    }

    fn height(&self) -> Length {
        <Column<'a, Message, Renderer<B>> as iced_native::Widget<Message, Renderer<B>>>::height(
            &self.column,
        )
    }

    fn layout(&self, renderer: &Renderer<B>, limits: &layout::Limits) -> layout::Node {
        <Column<'a, Message, Renderer<B>> as iced_native::Widget<Message, Renderer<B>>>::layout(
            &self.column,
            renderer,
            limits,
        )
    }

    fn draw(
        &self,
        renderer: &mut Renderer<B>,
        defaults: &<Renderer<B> as iced_native::Renderer>::Defaults,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) -> <Renderer<B> as iced_native::Renderer>::Output {
        <Column<'a, Message, Renderer<B>> as iced_native::Widget<Message, Renderer<B>>>::draw(
            &self.column,
            renderer,
            defaults,
            layout,
            cursor_position,
            viewport,
        )
    }

    fn hash_layout(&self, state: &mut Hasher) {
        <Column<'a, Message, Renderer<B>> as iced_native::Widget<Message, Renderer<B>>>::hash_layout(
            &self.column,
            state,
        )
    }

    fn on_event(
        &mut self,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer<B>,
        clipboard: &mut dyn Clipboard,
        messages: &mut Vec<Result<Anschluss, Error>>,
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
                Message::Interrupt(interrupt) => todo!("interrupt signal: {}", interrupt),
                Message::Hinzufügen => {
                    messages.push(Anschlüsse::neu().and_then(|mut anschlüsse| {
                        if self.active_tab == 0 {
                            anschlüsse.reserviere_pin(self.pin).map(Into::into)
                        } else {
                            anschlüsse
                                .reserviere_pcf8574_port(
                                    self.a0,
                                    self.a1,
                                    self.a2,
                                    self.variante,
                                    u3::new(self.port),
                                )
                                .map(Into::into)
                                .map_err(Into::into)
                            // TODO set_interrupt_pin für input
                        }
                        // TODO into_output/input
                    }))
                },
            }
            status = event::Status::Captured;
        }
        status
    }
}

impl<'a, T: 'a, B: 'a + Backend> From<Widget<'a, T, B>>
    for Element<'a, Result<Anschluss, Error>, Renderer<B>>
{
    fn from(widget: Widget<'a, T, B>) -> Element<'a, Result<Anschluss, Error>, Renderer<B>> {
        Element::new(widget)
    }
}
