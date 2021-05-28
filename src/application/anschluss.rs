//! Auswahl eines Anschlusses.

use iced_aw::native::{number_input, NumberInput, TabLabel, Tabs};
use iced_graphics::{backend, button, Backend, Radio, Renderer, Row, Text};
use iced_native::{
    column,
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
    Rectangle,
};
use num_x::u3;

use crate::anschluss::{
    anschlüsse::Error, level::Level, pcf8574::Variante, Anschluss, Anschlüsse
};
mod style;

// TODO direkt Input/Output/Pwm-Varianten
#[derive(Debug)]
pub struct Auswahl {
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
}

#[derive(Debug, Clone)]
pub enum Message {
    TabSelected(usize),
    Pin(u8),
    A0(Level),
    A1(Level),
    A2(Level),
    Variante(Variante),
    Port(u8),
    Hinzufügen,
}

impl Auswahl {
    pub fn neu() -> Self {
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
        }
    }

    pub fn view(&mut self) -> iced::Element<Message> {
        let Auswahl {
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
        } = self;
        let tabs = vec![
            (
                TabLabel::Text("Pin".to_string()),
                NumberInput::new(pin_state, *pin, 32, Message::Pin).into(),
            ),
            (
                TabLabel::Text("Pcf8574-Port".to_string()),
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
                    .into(),
            ),
        ];
        Column::new()
            .push(
                Tabs::with_tabs(*active_tab, tabs, Message::TabSelected)
                    .tab_bar_style(style::TabBar),
            )
            .push(Element::from(
                Button::new(confirm_state, Text::new("Hinzufügen")).on_press(Message::Hinzufügen),
            ))
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }

    pub fn update(&mut self, message: Message) -> Option<Result<Anschluss, Error>> {
        use Message::*;
        let mut result: Option<Result<Anschluss, Error>> = None;
        match message {
            TabSelected(tab) => self.active_tab = tab,
            Pin(pin) => self.pin = pin,
            A0(a0) => self.a0 = a0,
            A1(a1) => self.a1 = a1,
            A2(a2) => self.a2 = a2,
            Variante(variante) => self.variante = variante,
            Port(port) => self.port = port,
            Hinzufügen => {
                result = Some(Anschlüsse::neu().and_then(|mut anschlüsse| {
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
                    }
                }))
            },
        }
        result
    }
}

pub struct Widget<'a, Renderer>(Column<'a, Message, Renderer>);

impl<'a, B: 'a + Backend + backend::Text> Widget<'a, Renderer<B>> {
    pub fn neu(
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
        }: &'a mut Auswahl,
    ) -> Self {
        let tabs = vec![
            (
                TabLabel::Text("Pin".to_string()),
                NumberInput::new(pin_state, *pin, 32, Message::Pin).into(),
            ),
            (
                TabLabel::Text("Pcf8574-Port".to_string()),
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
                    .into(),
            ),
        ];
        Widget(
            Column::new()
                .push(
                    Tabs::with_tabs(*active_tab, tabs, Message::TabSelected)
                        .tab_bar_style(style::TabBar),
                )
                .push(
                    Button::new(confirm_state, Text::new("Hinzufügen"))
                        .on_press(Message::Hinzufügen),
                )
                .width(Length::Fill)
                .height(Length::Fill),
        )
    }
}

impl<'a, R: column::Renderer> iced_native::Widget<Message, R> for Widget<'a, R> {
    fn width(&self) -> Length {
        <Column<'a, Message, R> as iced_native::Widget<Message, R>>::width(&self.0)
    }

    fn height(&self) -> Length {
        <Column<'a, Message, R> as iced_native::Widget<Message, R>>::height(&self.0)
    }

    fn layout(&self, renderer: &R, limits: &layout::Limits) -> layout::Node {
        <Column<'a, Message, R> as iced_native::Widget<Message, R>>::layout(
            &self.0, renderer, limits,
        )
    }

    fn draw(
        &self,
        renderer: &mut R,
        defaults: &R::Defaults,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) -> R::Output {
        <Column<'a, Message, R> as iced_native::Widget<Message, R>>::draw(
            &self.0,
            renderer,
            defaults,
            layout,
            cursor_position,
            viewport,
        )
    }

    fn hash_layout(&self, state: &mut Hasher) {
        <Column<'a, Message, R> as iced_native::Widget<Message, R>>::hash_layout(&self.0, state)
    }

    fn on_event(
        &mut self,
        _event: Event,
        _layout: Layout<'_>,
        _cursor_position: Point,
        _renderer: &R,
        _clipboard: &mut dyn Clipboard,
        _messages: &mut Vec<Message>,
    ) -> event::Status {
        todo!()
    }
}

impl<'a, B> From<Widget<'a, Renderer<B>>> for Element<'a, Message, Renderer<B>>
where
    B: 'a + Backend,
{
    fn from(widget: Widget<'a, Renderer<B>>) -> Element<'a, Message, Renderer<B>> {
        Element::new(widget)
    }
}
