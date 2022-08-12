//! Auswahl eines [Anschlusses](crate::anschluss::Anschluss).

use std::{
    collections::HashMap,
    convert::identity,
    fmt::{self, Debug, Formatter},
};

use iced_aw::pure::{NumberInput, TabLabel, Tabs};
use iced_native::{event, text, Clipboard, Event, Font, Layout, Length, Point, Shell};
use iced_pure::{
    widget::{
        tree::{State, Tag, Tree},
        Column, Radio, Row, Text,
    },
    Element, Widget,
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
    application::{macros::widget_newtype_methods, style::tab_bar::TabBar},
    eingeschränkt::{kleiner_8, InvaliderWert},
};

/// Zustand eines Widgets zur Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug)]
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
pub struct Auswahl<'a, Modus, ModusNachricht, Serialisiert, R> {
    element: Element<'a, InterneNachricht<ModusNachricht>, R>,
    zeige_modus: ZeigeModus,
    view_modus: &'a dyn Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
    update_modus: &'a dyn Fn(&mut Modus, ModusNachricht),
    make_pin: &'a dyn Fn(u8, &Modus) -> Serialisiert,
    make_port: &'a dyn Fn(Beschreibung, kleiner_8, &Modus) -> Serialisiert,
    erzeuge_zustand: &'a dyn Fn() -> Zustand<Modus>,
}

impl<Modus: Debug, ModusNachricht, Serialisiert, R> Debug
    for Auswahl<'_, Modus, ModusNachricht, Serialisiert, R>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Auswahl")
            .field("element", &"<Element>")
            .field("zeige_modus", &self.zeige_modus)
            .field("view_modus", &"<closure>")
            .field("update_modus", &"<closure>")
            .field("make_pin", &"<closure>")
            .field("make_port", &"<closure>")
            .field("erzeuge_zustand", &"<closure>")
            .finish()
    }
}

impl<'a, R> Auswahl<'a, u8, InputNachricht, InputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
{
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input(
        start_wert: Option<&'a InputAnschluss>,
        interrupt_pins: &'a HashMap<Beschreibung, u8>,
    ) -> Self {
        /*
        // FIXME sollte das von außen kommen?
        // wird verwendet als bekannte interrupt pins
        let interrupt_pins = match start_wert.map(InputAnschluss::serialisiere) {
            Some(InputSerialisiert::Pcf8574Port {
                beschreibung,
                port: _,
                interrupt: Some(pin),
            }) => HashMap::from([(beschreibung, pin)]),
            _ => HashMap::new(),
        };
        */
        Auswahl::neu_mit_interrupt_view(
            ZeigeModus::Pcf8574,
            &|pin, beschreibung| {
                interrupt_pins
                    .get(&beschreibung)
                    .map_or(NumberInput::new(*pin, 32, InputNachricht::interrupt).into(), |pin| {
                        Text::new(pin.to_string()).into()
                    })
            },
            &|modus: &mut u8, InputNachricht { interrupt: pin }| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            &|beschreibung, port, pin| InputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                interrupt: if interrupt_pins.get(&beschreibung).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
            &|| {
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
                Zustand {
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
                }
            },
        )
    }
}

impl<'a, R> Auswahl<'a, Polarität, OutputNachricht, OutputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
{
    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output(start_wert: Option<&'a OutputAnschluss>) -> Self {
        Auswahl::neu_mit_interrupt_view(
            ZeigeModus::Beide,
            &|polarität, _beschreibung| {
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
                    .into()
            },
            &|modus, OutputNachricht { polarität }| *modus = polarität,
            &|pin, polarität| OutputSerialisiert::Pin { pin, polarität: *polarität },
            &|beschreibung, port, polarität| OutputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                polarität: *polarität,
            },
            &|| {
                let (active_tab, pin, beschreibung, port, modus) = match start_wert {
                    Some(OutputAnschluss::Pin { pin, polarität }) => {
                        (TAB_PIN, Some(pin.pin()), None, None, Some(*polarität))
                    },
                    Some(OutputAnschluss::Pcf8574Port { port, polarität }) => (
                        TAB_PCF8574,
                        None,
                        Some(*port.beschreibung()),
                        Some(port.port()),
                        Some(*polarität),
                    ),
                    None => (TAB_PIN, None, None, None, None),
                };
                Zustand {
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
                }
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
{
    Column::new()
        .push(Radio::new(fst, fst_s, Some(current.clone()), to_message.clone()).spacing(0))
        .push(Radio::new(snd, snd_s, Some(current.clone()), to_message).spacing(0))
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    Modus: Copy,
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
{
    fn neu_mit_interrupt_view(
        zeige_modus: ZeigeModus,
        view_modus: &'a impl Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
        update_modus: &'a impl Fn(&mut Modus, ModusNachricht),
        make_pin: &'a impl Fn(u8, &Modus) -> Serialisiert,
        make_port: &'a impl Fn(Beschreibung, kleiner_8, &Modus) -> Serialisiert,
        erzeuge_zustand: &'a impl Fn() -> Zustand<Modus>,
    ) -> Self {
        Auswahl {
            element: Self::erzeuge_element(&erzeuge_zustand(), view_modus, zeige_modus),
            zeige_modus,
            view_modus,
            update_modus,
            make_pin,
            make_port,
            erzeuge_zustand,
        }
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
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
        let width = Length::Units(350);
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
                    .tab_bar_style(TabBar)
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
                    .tab_bar_style(TabBar)
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            },
        };
        row.into()
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Widget<Serialisiert, R>
    for Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    Modus: 'static,
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
{
    widget_newtype_methods! {element, R}

    fn tag(&self) -> Tag {
        Tag::of::<Zustand<Modus>>()
    }

    fn state(&self) -> State {
        State::new((self.erzeuge_zustand)())
    }

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Serialisiert>,
    ) -> event::Status {
        let mut internal_messages = Vec::new();
        let mut internal_shell = Shell::new(&mut internal_messages);
        let mut status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
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
        let zustand: &mut Zustand<Modus> = state.state.downcast_mut();
        for message in internal_messages {
            changed = true;
            match message {
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
                InterneNachricht::Modus(msg) => (self.update_modus)(&mut zustand.modus, msg),
            }
            status = event::Status::Captured;
        }
        if changed {
            self.element = Self::erzeuge_element(&zustand, &self.view_modus, self.zeige_modus);
            let message = if zustand.active_tab == 0 {
                (self.make_pin)(zustand.pin, &zustand.modus)
            } else {
                (self.make_port)(zustand.beschreibung, zustand.port, &zustand.modus)
            };
            shell.publish(message);
        }
        status
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R>
    From<Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>> for Element<'a, Serialisiert, R>
where
    Modus: 'static,
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
{
    fn from(auswahl: Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>) -> Self {
        Element::new(auswahl)
    }
}

/// Zustand eines Widgets zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug)]
pub struct PwmZustand {
    pin: u8,
}

impl PwmZustand {
    /// Erstelle einen neuen [PwmZustand].
    pub fn neu() -> Self {
        PwmZustand { pin: 0 }
    }
}

/// Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
pub struct Pwm<'a, R: 'a + text::Renderer<Font = Font>> {
    element: Element<'a, pwm::Serialisiert, R>,
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
    pub fn neu(PwmZustand { pin }: &'a mut PwmZustand) -> Self {
        Pwm { element: NumberInput::new(*pin, 32, pwm::Serialisiert).into(), pin }
    }
}

impl<R: text::Renderer<Font = Font>> Widget<pwm::Serialisiert, R> for Pwm<'_, R> {
    widget_newtype_methods! {element, R}

    fn on_event(
        &mut self,
        state: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &R,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, pwm::Serialisiert>,
    ) -> event::Status {
        let mut internal_messages = Vec::new();
        let mut internal_shell = Shell::new(&mut internal_messages);
        let mut status = self.element.as_widget_mut().on_event(
            &mut state.children[0],
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
