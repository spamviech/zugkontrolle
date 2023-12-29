//! Auswahl eines [Anschlusses](crate::anschluss::Anschluss).

use std::ops::DerefMut;

use iced_aw::{
    native::{NumberInput, TabLabel, Tabs},
    style::tab_bar,
};
use iced_core::{event, text, widget::Text, Element, Font, Length, Renderer};
use iced_widget::{
    scrollable::{self, Scrollable},
    Column, Container, Radio, Row, Space,
};
use log::error;

use crate::{
    anschluss::{
        level::Level,
        pcf8574::{self, Beschreibung, I2cBus, Lager, Variante},
        pin::pwm,
        polarität::Polarität,
        InputAnschluss, InputSerialisiert, OutputAnschluss, OutputSerialisiert,
    },
    application::{
        map_mit_zustand::MapMitZustand,
        style::{sammlung::Sammlung, tab_bar::TabBar},
    },
    argumente::I2cSettings,
    util::eingeschränkt::{kleiner_8, InvaliderWert},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TabId {
    Pin,
    Pcf8574,
}

/// Zustand eines Widgets zur Auswahl eines [Anschlusses](crate::anschluss::Anschluss).
#[derive(Debug, PartialEq, Eq)]
struct Zustand<T> {
    active_tab: TabId,
    pin: u8,
    beschreibung: Beschreibung,
    port: kleiner_8,
    modus: T,
}

#[derive(Debug, Clone)]
enum InterneNachricht<T> {
    TabSelected(TabId),
    Pin(u8),
    I2cBus(I2cBus),
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
#[derive(Debug)]
pub struct Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>(
    MapMitZustand<'a, Zustand<Modus>, InterneNachricht<ModusNachricht>, Serialisiert, R>,
);

impl<'a, R> Auswahl<'a, u8, InputNachricht, InputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    <R as Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_widget::container::StyleSheet
        + iced_widget::radio::StyleSheet
        + iced_widget::scrollable::StyleSheet
        + iced_widget::text::StyleSheet
        + iced_widget::text_input::StyleSheet,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input(
        start_wert: Option<&'a InputAnschluss>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputAnschluss::Pin(pin)) => (TabId::Pin, Some(pin.pin()), None, None, None),
            Some(InputAnschluss::Pcf8574Port(port)) => (
                TabId::Pcf8574,
                None,
                Some(*port.beschreibung()),
                Some(port.port()),
                port.interrupt_pin(),
            ),
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_input_aux(
            active_tab,
            pin,
            beschreibung,
            port,
            modus,
            lager,
            scrollable_style,
            settings,
        )
    }

    /// Erstelle ein Widget zur Auswahl eines [InputAnschluss](crate::anschluss::InputAnschluss).
    pub fn neu_input_s(
        start_wert: Option<InputSerialisiert>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(InputSerialisiert::Pin { pin }) => (TabId::Pin, Some(pin), None, None, None),
            Some(InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt }) => {
                (TabId::Pcf8574, None, Some(beschreibung), Some(port), interrupt)
            },
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_input_aux(
            active_tab,
            pin,
            beschreibung,
            port,
            modus,
            lager,
            scrollable_style,
            settings,
        )
    }

    fn neu_input_aux(
        active_tab: TabId,
        pin: Option<u8>,
        beschreibung: Option<Beschreibung>,
        port: Option<kleiner_8>,
        modus: Option<u8>,
        lager: &'a Lager,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        Auswahl::neu_mit_interrupt_view(
            ZeigeModus::Pcf8574,
            |pin, beschreibung| {
                let interrupt_pin = lager.interrupt_pin(&beschreibung).map_or(
                    Element::from(
                        NumberInput::new(*pin, 32, InputNachricht::interrupt).width(Length::Fill),
                    ),
                    |pin| {
                        let text: Text<'_, R> = Text::new(pin.to_string());
                        Element::from(text)
                    },
                );
                Element::from(
                    Column::new()
                        .push(
                            Container::new(Text::new("Interrupt-Pin"))
                                .width(Length::Fill)
                                .center_x(),
                        )
                        .push(interrupt_pin)
                        .width(Length::Fixed(100.)),
                )
            },
            &|modus: &mut u8, InputNachricht { interrupt: pin }| *modus = pin,
            &|pin, _input| InputSerialisiert::Pin { pin },
            |beschreibung, port, pin| InputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                interrupt: if lager.interrupt_pin(&beschreibung).is_some() {
                    None
                } else {
                    Some(*pin)
                },
            },
            move || Zustand {
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
            },
            scrollable_style,
            settings,
        )
    }
}

impl<'a, R> Auswahl<'a, Polarität, OutputNachricht, OutputSerialisiert, R>
where
    R: 'a + text::Renderer<Font = Font>,
    <R as Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_widget::container::StyleSheet
        + iced_widget::radio::StyleSheet
        + iced_widget::scrollable::StyleSheet
        + iced_widget::text::StyleSheet
        + iced_widget::text_input::StyleSheet,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output(
        start_wert: Option<&'a OutputAnschluss>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputAnschluss::Pin { pin, polarität }) => {
                (TabId::Pin, Some(pin.pin()), None, None, Some(*polarität))
            },
            Some(OutputAnschluss::Pcf8574Port { port, polarität }) => (
                TabId::Pcf8574,
                None,
                Some(*port.beschreibung()),
                Some(port.port()),
                Some(*polarität),
            ),
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_output_aux(active_tab, pin, beschreibung, port, modus, scrollable_style, settings)
    }

    /// Erstelle ein Widget zur Auswahl eines [OutputAnschluss](crate::anschluss::OutputAnschluss).
    pub fn neu_output_s(
        start_wert: Option<OutputSerialisiert>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let (active_tab, pin, beschreibung, port, modus) = match start_wert {
            Some(OutputSerialisiert::Pin { pin, polarität }) => {
                (TabId::Pin, Some(pin), None, None, Some(polarität))
            },
            Some(OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität }) => {
                (TabId::Pcf8574, None, Some(beschreibung), Some(port), Some(polarität))
            },
            None => (TabId::Pin, None, None, None, None),
        };
        Self::neu_output_aux(active_tab, pin, beschreibung, port, modus, scrollable_style, settings)
    }

    fn neu_output_aux(
        active_tab: TabId,
        pin: Option<u8>,
        beschreibung: Option<Beschreibung>,
        port: Option<kleiner_8>,
        modus: Option<Polarität>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        Auswahl::neu_mit_interrupt_view(
            ZeigeModus::Beide,
            |polarität, _beschreibung| {
                Column::new()
                    .push(Radio::new(
                        "Normal",
                        Polarität::Normal,
                        Some(*polarität),
                        OutputNachricht::polarität,
                    ))
                    .push(Radio::new(
                        "Invertiert",
                        Polarität::Invertiert,
                        Some(*polarität),
                        OutputNachricht::polarität,
                    ))
                    .into()
            },
            &|modus, OutputNachricht { polarität }| *modus = polarität,
            &|pin, polarität| OutputSerialisiert::Pin { pin, polarität: *polarität },
            |beschreibung, port, polarität| OutputSerialisiert::Pcf8574Port {
                beschreibung,
                port,
                polarität: *polarität,
            },
            move || Zustand {
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
            },
            scrollable_style,
            settings,
        )
    }
}

#[derive(Debug, Clone, Copy)]
enum ZeigeModus {
    Beide,
    Pcf8574,
}

#[allow(single_use_lifetimes)]
pub(in crate::application) fn make_radios<'a, 'b, T, M, R>(
    aktuell: &T,
    elemente: impl IntoIterator<Item = (&'b str, T)>,
    als_nachricht: impl Fn(T) -> M + Clone + 'static,
) -> Column<'a, M, R>
where
    T: Eq + Copy,
    M: 'a + Clone,
    R: 'a + text::Renderer,
    <R as Renderer>::Theme: iced_widget::radio::StyleSheet + iced_widget::text::StyleSheet,
    <R as iced_core::text::Renderer>::Font: From<Font>,
{
    let mut column = Column::new();
    let mut leerer_iterator = true;
    for (label, value) in elemente {
        leerer_iterator = false;
        column = column.push(
            Radio::new(label, value, Some(aktuell.clone()), als_nachricht.clone()).spacing(0),
        );
    }
    if leerer_iterator {
        column = column.push(Text::new("⚠").font(crate::application::fonts::EMOJI));
    }
    column
}

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    Modus: Copy,
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
    <R as Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_widget::container::StyleSheet
        + iced_widget::radio::StyleSheet
        + iced_widget::scrollable::StyleSheet
        + iced_widget::text::StyleSheet
        + iced_widget::text_input::StyleSheet,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    fn neu_mit_interrupt_view(
        zeige_modus: ZeigeModus,
        view_modus: impl 'a + Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
        update_modus: &'a impl Fn(&mut Modus, ModusNachricht),
        make_pin: &'a impl Fn(u8, &Modus) -> Serialisiert,
        make_port: impl 'a + Fn(Beschreibung, kleiner_8, &Modus) -> Serialisiert,
        erzeuge_zustand: impl 'a + Fn() -> Zustand<Modus>,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Self {
        let erzeuge_element = move |zustand: &Zustand<Modus>| {
            Self::erzeuge_element(zustand, &view_modus, zeige_modus, scrollable_style, settings)
        };
        let mapper = move |interne_nachricht,
                           zustand: &mut dyn DerefMut<Target = Zustand<Modus>>,
                           status: &mut event::Status| {
            *status = event::Status::Captured;
            match interne_nachricht {
                InterneNachricht::TabSelected(tab) => zustand.active_tab = tab,
                InterneNachricht::Pin(pin) => zustand.pin = pin,
                InterneNachricht::I2cBus(i2c_bus) => zustand.beschreibung.i2c_bus = i2c_bus,
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
                InterneNachricht::Modus(msg) => update_modus(&mut zustand.modus, msg),
            }
            let nachricht = match zustand.active_tab {
                TabId::Pin => make_pin(zustand.pin, &zustand.modus),
                TabId::Pcf8574 => make_port(zustand.beschreibung, zustand.port, &zustand.modus),
            };
            vec![nachricht]
        };
        Auswahl(MapMitZustand::neu(erzeuge_zustand, erzeuge_element, mapper))
    }
}

const PADDING: f32 = 2.5;

impl<'a, Modus, ModusNachricht, Serialisiert, R> Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>
where
    ModusNachricht: 'static + Clone,
    R: 'a + text::Renderer<Font = Font>,
    <R as Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_aw::tab_bar::StyleSheet
        + iced_widget::container::StyleSheet
        + iced_widget::radio::StyleSheet
        + iced_widget::scrollable::StyleSheet
        + iced_widget::text::StyleSheet
        + iced_widget::text_input::StyleSheet,
    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style: From<Sammlung>,
    <<R as Renderer>::Theme as tab_bar::StyleSheet>::Style: From<TabBar>,
{
    fn erzeuge_element(
        zustand: &Zustand<Modus>,
        view_modus: &impl Fn(&Modus, Beschreibung) -> Element<'a, ModusNachricht, R>,
        zeige_modus: ZeigeModus,
        scrollable_style: Sammlung,
        settings: I2cSettings,
    ) -> Element<'a, InterneNachricht<ModusNachricht>, R> {
        let Zustand { active_tab, pin, beschreibung, port, modus } = zustand;
        let element_modus = view_modus(&modus, *beschreibung);
        let Beschreibung { i2c_bus, a0, a1, a2, variante } = beschreibung;
        let view_modus_mapped = element_modus.map(InterneNachricht::Modus);
        let high_low_column =
            |level: &Level, als_nachricht: fn(Level) -> InterneNachricht<ModusNachricht>| {
                make_radios(level, [("H", Level::High), ("L", Level::Low)], als_nachricht)
            };
        let pcf8574_row = Row::new()
            .push(
                Scrollable::new(
                    Row::new()
                        .push(Text::new("I2C"))
                        .push(Space::with_width(Length::Fixed(PADDING)))
                        .push(make_radios(
                            i2c_bus,
                            [
                                ("0/1", I2cBus::I2c0_1),
                                ("3", I2cBus::I2c3),
                                ("4", I2cBus::I2c4),
                                ("5", I2cBus::I2c5),
                                ("6", I2cBus::I2c6),
                            ]
                            .into_iter()
                            .filter(|(_label, i2c_bus)| settings.aktiviert(*i2c_bus)),
                            InterneNachricht::I2cBus,
                        ))
                        .push(Space::with_width(Length::Fixed(scrollable_style.breite()))),
                )
                .height(Length::Fixed(55.))
                .style(
                    <<R as Renderer>::Theme as scrollable::StyleSheet>::Style::from(
                        scrollable_style,
                    ),
                ),
            )
            .push(high_low_column(a0, InterneNachricht::A0))
            .push(high_low_column(a1, InterneNachricht::A1))
            .push(high_low_column(a2, InterneNachricht::A2))
            .push(make_radios(
                variante,
                [("Normal", Variante::Normal), ("A", Variante::A)],
                InterneNachricht::Variante,
            ))
            .push(
                Column::new()
                    .push(Container::new(Text::new("Port")).width(Length::Fill).center_x())
                    .push(NumberInput::new(
                        u8::from(*port),
                        u8::from(kleiner_8::MAX),
                        InterneNachricht::Port,
                    ))
                    .width(Length::Fixed(75.)),
            );
        // TODO Length::Fill/Shrink funktioniert nicht richtig (Card zu klein)
        let width = Length::Fixed(600.);
        let row = match zeige_modus {
            ZeigeModus::Pcf8574 => {
                let tabs = vec![
                    (
                        TabId::Pin,
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabId::Pcf8574, TabLabel::Text("Pcf8574-Port".to_owned()), {
                        pcf8574_row.push(view_modus_mapped).into()
                    }),
                ];
                let tabs = Tabs::with_tabs(tabs, InterneNachricht::TabSelected)
                    .set_active_tab(active_tab)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs)
            },
            ZeigeModus::Beide => {
                let tabs = vec![
                    (
                        TabId::Pin,
                        TabLabel::Text("Pin".to_owned()),
                        NumberInput::new(*pin, 32, InterneNachricht::Pin).into(),
                    ),
                    (TabId::Pcf8574, TabLabel::Text("Pcf8574-Port".to_owned()), {
                        pcf8574_row.into()
                    }),
                ];
                let tabs = Tabs::with_tabs(tabs, InterneNachricht::TabSelected)
                    .set_active_tab(active_tab)
                    .tab_bar_style(TabBar.into())
                    .height(Length::Shrink)
                    .width(width);
                Row::new().push(tabs).push(view_modus_mapped)
            },
        };
        row.into()
    }
}

impl<'a, Modus, ModusNachricht, Serialisiert, R>
    From<Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>> for Element<'a, Serialisiert, R>
where
    Modus: 'static + PartialEq,
    ModusNachricht: 'a,
    Serialisiert: 'a,
    R: 'a + Renderer,
{
    fn from(auswahl: Auswahl<'a, Modus, ModusNachricht, Serialisiert, R>) -> Self {
        Element::from(auswahl.0)
    }
}

/// Zustand eines Widgets zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug, PartialEq, Eq)]
struct PwmZustand {
    pin: u8,
}

impl PwmZustand {
    /// Erstelle einen neuen [PwmZustand].
    fn neu(pin: Option<u8>) -> Self {
        PwmZustand { pin: pin.unwrap_or(0) }
    }
}

/// Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
#[derive(Debug)]
pub struct Pwm<'a, R>(MapMitZustand<'a, PwmZustand, pwm::Serialisiert, pwm::Serialisiert, R>);

impl<'a, R> Pwm<'a, R>
where
    R: 'a + iced_core::text::Renderer<Font = Font>,
    <R as Renderer>::Theme: iced_aw::number_input::StyleSheet
        + iced_core::widget::text::StyleSheet
        + iced_widget::container::StyleSheet
        + iced_widget::text_input::StyleSheet,
{
    /// Erstelle ein Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
    pub fn neu(pin: Option<&'a pwm::Pin>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(pin.map(pwm::Pin::pin));
        Pwm(MapMitZustand::neu(erzeuge_zustand, Self::erzeuge_element, Self::mapper))
    }

    /// Erstelle ein Widget zur Auswahl eines [Pwm-Pins](pwm::Pin).
    pub fn neu_s(pin: Option<pwm::Serialisiert>) -> Self {
        let erzeuge_zustand = move || PwmZustand::neu(pin.as_ref().map(|pin| pin.0));
        Pwm(MapMitZustand::neu(erzeuge_zustand, Self::erzeuge_element, Self::mapper))
    }

    fn erzeuge_element(zustand: &PwmZustand) -> Element<'a, pwm::Serialisiert, R> {
        NumberInput::new(zustand.pin, 32, pwm::Serialisiert).into()
    }

    fn mapper(
        nachricht: pwm::Serialisiert,
        zustand: &mut (dyn DerefMut<Target = PwmZustand>),
        status: &mut event::Status,
    ) -> Vec<pwm::Serialisiert> {
        *status = event::Status::Captured;
        let pwm::Serialisiert(pin) = nachricht;
        zustand.pin = pin;
        vec![nachricht]
    }
}

impl<'a, R> From<Pwm<'a, R>> for Element<'a, pwm::Serialisiert, R>
where
    R: 'a + Renderer,
{
    fn from(auswahl: Pwm<'a, R>) -> Self {
        Element::from(auswahl.0)
    }
}
