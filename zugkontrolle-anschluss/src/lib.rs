//! Mit Raspberry Pi schaltbarer Anschluss.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use std::{
    any::TypeId,
    fmt::{self, Display, Formatter},
};

use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};

use thiserror::Error;
use zugkontrolle_argumente::I2cSettings;
use zugkontrolle_id::eindeutig::KeineIdVerfügbar;
use zugkontrolle_util::eingeschränkt::kleiner_8;

use crate::{
    de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
    level::Level,
    pin::{input, output, pwm, Pin},
    polarität::{Fließend, Polarität},
    trigger::Trigger,
};

pub mod de_serialisieren;
pub mod level;
pub mod pcf8574;
pub mod pin;
#[path = "polarität.rs"]
pub mod polarität;
pub mod rppal;
pub mod trigger;

/// Verwalten nicht verwendeter [Pin]s und [`Pcf8574-Ports`](pcf8574::Port).
#[derive(Debug)]
pub struct Lager {
    /// [Lager](pin::Lager) zum verwalten nicht verwendeter [`Pin`]s.
    pub pin: pin::Lager,
    /// [Lager](pcf8574::Lager) zum verwalten nicht verwendeter [`Pcf8574-Ports`](pcf8574::Port).
    pub pcf8574: pcf8574::Lager,
}

/// Fehler beim [Initialisieren](Lager::neu) eines [`Lager`]s.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum InitFehler {
    /// Fehler beim Initialisieren des [`Pin-Lagers`](pin::Lager).
    Pin(rppal::gpio::Error),
    /// Fehler beim Initialisieren des [`Pcf8574-Lagers`](pcf8574::Lager).
    Pcf8574(pcf8574::InitFehler),
}

impl Lager {
    /// Initialisiere ein [Lager], das nicht verwendete [`Anschlüsse`](Anschluss) verwaltet.
    ///
    /// ## Errors
    ///
    /// Zugriff auf Gpio-Pins oder I2c-Kanäle nicht möglich.
    pub fn neu(settings: I2cSettings) -> Result<Lager, InitFehler> {
        let mut pin = pin::Lager::neu()?;
        let pcf8574 = pcf8574::Lager::neu(&mut pin, settings)?;
        Ok(Lager { pin, pcf8574 })
    }

    /// Reserviere einen [`Pin`].
    ///
    /// ## Errors
    ///
    /// IO-Fehler (z.B. fehlende Berechtigungen), oder der Pin ist bereits in Verwendung.
    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Anschluss, ReservierenFehler> {
        match self.pin.reserviere_pin(pin) {
            Ok(pin) => Ok(Anschluss::Pin(pin)),
            Err(fehler) => Err(fehler.into()),
        }
    }

    /// Reserviere einen [`Pcf8574-Port`](pcf8574::Port).
    ///
    /// ## Errors
    ///
    /// IO-Fehler (z.B. fehlende Berechtigungen), oder der Port ist bereits in Verwendung.
    pub fn reserviere_pcf8574_port(
        &mut self,
        beschreibung: pcf8574::Beschreibung,
        port: kleiner_8,
    ) -> Result<Anschluss, ReservierenFehler> {
        match self.pcf8574.reserviere_pcf8574_port(beschreibung, port) {
            Ok(port) => Ok(Anschluss::Pcf8574Port(port)),
            Err(fehler) => Err(fehler.into()),
        }
    }
}

/// Ein Anschluss.
#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum Anschluss {
    /// Ein [`Pin`].
    Pin(Pin),
    /// Ein [`Pcf8574-Port`](pcf8574::Port).
    Pcf8574Port(pcf8574::Port),
}

impl Display for Anschluss {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Anschluss::Pin(pin) => write!(formatter, "Pin({})", pin.pin()),
            Anschluss::Pcf8574Port(port) => {
                write!(formatter, "Pcf8574Port({port})")
            },
        }
    }
}

impl Anschluss {
    /// Konfiguriere den [Anschluss] als [`Output`](OutputAnschluss).
    pub fn als_output(self, polarität: Polarität) -> (OutputAnschluss, Option<Fehler>) {
        let gesperrt_level = Fließend::Gesperrt.mit_polarität(polarität);
        match self {
            Anschluss::Pin(pin) => {
                (OutputAnschluss::Pin { pin: pin.als_output(gesperrt_level), polarität }, None)
            },
            Anschluss::Pcf8574Port(port) => {
                let (port, fehler) = port.als_output(gesperrt_level);
                (OutputAnschluss::Pcf8574Port { port, polarität }, fehler.map(Fehler::from))
            },
        }
    }

    /// Konfiguriere den [Anschluss] als [`Input`](InputAnschluss).
    pub fn als_input(self) -> (InputAnschluss, Option<Fehler>) {
        match self {
            Anschluss::Pin(pin) => (InputAnschluss::Pin(pin.als_input()), None),
            Anschluss::Pcf8574Port(port) => {
                let (port, fehler) = port.als_input();
                (InputAnschluss::Pcf8574Port(port), fehler.map(Fehler::from))
            },
        }
    }
}

/// Ein Anschluss, konfiguriert für Output.
#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum OutputAnschluss {
    /// Ein [`Pin`](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: output::Pin,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die [`Pcf8574-Port`](pcf8574::OutputPort).
        port: pcf8574::OutputPort,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
}

impl Display for OutputAnschluss {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                write!(formatter, "Pin({}, {polarität})", pin.pin())
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                write!(formatter, "Pcf8574Port({port}, {polarität}")
            },
        }
    }
}

impl OutputAnschluss {
    /// Einstellen des [`OutputAnschlusses`](OutputAnschluss).
    ///
    /// ## Errors
    ///
    /// Fehler in der I2C-Kommunikation für einen [`Pcf8574-Port`](pcf8574::Port).
    pub fn einstellen(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                pin.schreibe(fließend.mit_polarität(*polarität));
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                port.schreibe(fließend.mit_polarität(*polarität))?;
            },
        };
        Ok(())
    }

    /// Aktuelle Einstellung des [`OutputAnschlusses`](OutputAnschluss).
    #[must_use]
    pub fn fließend(&self) -> Fließend {
        if self.ist_fließend() {
            Fließend::Fließend
        } else {
            Fließend::Gesperrt
        }
    }

    /// Ist der [`OutputAnschluss`] aktuell [`fließend`](Fließend::Fließend).
    #[must_use]
    pub fn ist_fließend(&self) -> bool {
        match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarität::Normal => pin.ist_high(),
                Polarität::Invertiert => pin.ist_low(),
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarität::Normal => port.ist_high(),
                Polarität::Invertiert => port.ist_low(),
            },
        }
    }

    /// Ist der [`OutputAnschluss`] aktuell [`gesperrt`](Fließend::Gesperrt).
    #[must_use]
    pub fn ist_gesperrt(&self) -> bool {
        !self.ist_fließend()
    }

    /// Schalte den Strom von [Fließend](Fließend::Fließend) auf [`Gesperrt`](Fließend::Gesperrt)
    /// und umgekehrt.
    ///
    /// ## Errors
    ///
    /// Fehler in der I2C-Kommunikation für einen [`Pcf8574-Port`](pcf8574::Port).
    pub fn umschalten(&mut self) -> Result<(), Fehler> {
        match self {
            OutputAnschluss::Pin { pin, .. } => pin.umschalten(),
            OutputAnschluss::Pcf8574Port { port, .. } => port.umschalten()?,
        };
        Ok(())
    }
}

/// Serialisierbare Informationen eines [`OutputAnschlusses`](OutputAnschluss).
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OutputSerialisiert {
    /// Ein [`Pin`](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: pcf8574::Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Die [`Polarität`] des Anschlusses.
        polarität: Polarität,
    },
}

impl Default for OutputSerialisiert {
    fn default() -> Self {
        OutputSerialisiert::Pin { pin: u8::default(), polarität: Polarität::default() }
    }
}

impl Display for OutputSerialisiert {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OutputSerialisiert::Pin { pin, polarität } => {
                write!(formatter, "Pin({pin}, {polarität})")
            },
            OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                write!(formatter, "Pcf8574Port({beschreibung}-{port}, {polarität}")
            },
        }
    }
}

impl OutputSerialisiert {
    /// Handelt es sich um den selben Anschluss, unabhängig von der Polarität.
    #[must_use]
    pub fn selber_anschluss(&self, other: &OutputSerialisiert) -> bool {
        match (self, other) {
            (OutputSerialisiert::Pin { pin: p0, .. }, OutputSerialisiert::Pin { pin: p1, .. }) => {
                p0 == p1
            },
            (
                OutputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung_a, port: port_a, ..
                },
                OutputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung_b, port: port_b, ..
                },
            ) => beschreibung_a == beschreibung_b && port_a == port_b,
            _ => false,
        }
    }
}

impl Serialisiere<OutputSerialisiert> for OutputAnschluss {
    fn serialisiere(&self) -> OutputSerialisiert {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                OutputSerialisiert::Pin { pin: pin.pin(), polarität: *polarität }
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                let beschreibung = port.beschreibung();
                let port = port.port();
                OutputSerialisiert::Pcf8574Port {
                    beschreibung: *beschreibung,
                    port,
                    polarität: *polarität,
                }
            },
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        Anschlüsse {
            pwm_pins: Vec::new(),
            output_anschlüsse: vec![self],
            input_anschlüsse: Vec::new(),
        }
    }
}

impl Reserviere<OutputAnschluss> for OutputSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut Lager,
        Anschlüsse { pwm_pins, output_anschlüsse, input_anschlüsse }: Anschlüsse,
        _move_arg: Self::MoveArg,
        _ref_arg: &Self::RefArg,
        _mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<OutputAnschluss> {
        let neue_polarität = match self {
            OutputSerialisiert::Pin { polarität, .. }
            | OutputSerialisiert::Pcf8574Port { polarität, .. } => polarität,
        };
        let (mut gesucht, andere): (Vec<_>, Vec<_>) = output_anschlüsse
            .into_iter()
            .partition(|anschluss| self.selber_anschluss(&anschluss.serialisiere()));
        let ein_gesuchter = gesucht.pop();
        gesucht.extend(andere);
        let anschlüsse = Anschlüsse { pwm_pins, output_anschlüsse: gesucht, input_anschlüsse };
        if let Some(mut anschluss) = ein_gesuchter {
            match &mut anschluss {
                OutputAnschluss::Pin { polarität, .. }
                | OutputAnschluss::Pcf8574Port { polarität, .. } => *polarität = neue_polarität,
            }
            Ergebnis::Wert { anschluss, anschlüsse }
        } else {
            let (anschluss_res, polarität) = match self {
                OutputSerialisiert::Pin { pin, polarität } => {
                    (lager.reserviere_pin(pin), polarität)
                },
                OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                    (lager.reserviere_pcf8574_port(beschreibung, port), polarität)
                },
            };
            match anschluss_res {
                Ok(anschluss) => {
                    let (output_anschluss, fehler) = anschluss.als_output(polarität);
                    if let Some(fehler) = fehler {
                        Ergebnis::WertMitWarnungen {
                            anschluss: output_anschluss,
                            fehler: NonEmpty::singleton(fehler),
                            anschlüsse,
                        }
                    } else {
                        Ergebnis::Wert { anschluss: output_anschluss, anschlüsse }
                    }
                },
                Err(fehler) => {
                    Ergebnis::Fehler { fehler: NonEmpty::singleton(fehler.into()), anschlüsse }
                },
            }
        }
    }
}

/// Ein Anschluss, konfiguriert für Input.
#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum InputAnschluss {
    /// Ein [`Pin`](input::Pin).
    Pin(input::Pin),
    /// Ein [`Pcf8574-Port`](pcf8574::InputPort).
    Pcf8574Port(pcf8574::InputPort),
}

impl Display for InputAnschluss {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InputAnschluss::Pin(pin) => write!(formatter, "Pin({})", pin.pin()),
            InputAnschluss::Pcf8574Port(port) => {
                write!(formatter, "Pcf8574Port({port})")
            },
        }
    }
}

/// Rufe die gleich-benannte Methode für den Pin oder Pcf8574-Port auf.
macro_rules! match_method {
    (
        $method:ident$(($($arg:ident : $arg_ty: ty),+))?
        $(, $($docstring: expr),+ $(,)?)?
    ) => {
        match_method! {$method$(($($arg : $arg_ty),+))? -> () $(, $($docstring),+)?}
    };
    (
        $method:ident$(($($arg:ident : $arg_ty: ty),+))? -> $result:ty
        $(, $($docstring: expr),+ $(,)?)?
    ) => {
        $($(#[doc = $docstring])+)?
        pub fn $method(&mut self$(, $($arg: $arg_ty),+)?) -> Result<$result, Fehler> {
            Ok(match self {
                InputAnschluss::Pin(pin) => pin.$method($($($arg),+)?)?,
                InputAnschluss::Pcf8574Port(port) => port.$method($($($arg),+)?)?,
            })
        }
    };
}

impl InputAnschluss {
    match_method! {
        lese -> Level,
        "Lese das aktuell am [InputAnschluss] anliegende [Level].",
        "",
        "## Errors",
        "",
        "Fehler in der I2C-Kommunikation mit dem Pcf8574-Port.",
    }

    match_method! {
        setze_async_interrupt(trigger: Trigger, callback: impl Fn(Level) + Send + Sync + 'static),
        "Konfiguriere einen asynchronen Interrupt Trigger.",
        "Bei auftreten wird der callback in einem separaten Thread ausgeführt.",
        "",
        "Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald",
        "[setze_async_interrupt](self::pin::input::Pin::setze_async_interrupt) oder",
        "[lösche_async_interrupt](self::pin::input::Pin::lösche_async_interrupt) aufgerufen wird",
        "oder der [input::Pin](self::pin::input::Pin) out of scope geht.",
        "",
        "## Errors",
        "",
        "Setzten des Interrupts schlug fehl.",
        "",
        "## Keine synchronen Interrupts",
        "Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.",
        "Siehe die Dokumentation der",
        "[poll_interrupts](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)",
        "Methode.",
        "> Calling poll_interrupts blocks any other calls to poll_interrupts or",
        "> InputPin::poll_interrupt until it returns. If you need to poll multiple pins simultaneously",
        "> on different threads, consider using asynchronous interrupts with",
        "> InputPin::set_async_interrupt instead.",
    }

    match_method! {
        lösche_async_interrupt,
        "Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.",
        "",
        "## Errors",
        "",
        "Löschen des async interrupts schlug fehl.",
    }
}

/// Serialisierbare Informationen eines [`InputAnschlusses`](InputAnschluss).
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InputSerialisiert {
    /// Ein [`Pin`](input::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
    },
    /// Ein [`Pcf8574-Port`](pcf8574::InputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: pcf8574::Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Der konfigurierte Interrupt-Pin des Pcf8574.
        interrupt: Option<u8>,
    },
}

impl Default for InputSerialisiert {
    fn default() -> Self {
        InputSerialisiert::Pin { pin: u8::default() }
    }
}

impl InputSerialisiert {
    /// Handelt es sich um den selben Anschluss, unabhängig vom Interrupt Pin.
    #[must_use]
    pub fn selber_anschluss(&self, other: &InputSerialisiert) -> bool {
        match (self, other) {
            (InputSerialisiert::Pin { pin: p0 }, InputSerialisiert::Pin { pin: p1 }) => p0 == p1,
            (
                InputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung_a, port: port_a, ..
                },
                InputSerialisiert::Pcf8574Port {
                    beschreibung: beschreibung_b, port: port_b, ..
                },
            ) => beschreibung_a == beschreibung_b && port_a == port_b,
            _ => false,
        }
    }

    /// Der Interrupt-Pin eines [`Pcf8574-Port`](pcf8574::Port),
    /// sofern es sich um einen handelt und einer konfiguriert ist.
    #[must_use]
    pub fn interrupt(&self) -> Option<u8> {
        if let InputSerialisiert::Pcf8574Port { interrupt, .. } = self {
            *interrupt
        } else {
            None
        }
    }
}

impl Serialisiere<InputSerialisiert> for InputAnschluss {
    fn serialisiere(&self) -> InputSerialisiert {
        match self {
            InputAnschluss::Pin(pin) => InputSerialisiert::Pin { pin: pin.pin() },
            InputAnschluss::Pcf8574Port(port) => {
                let beschreibung = *port.beschreibung();
                let interrupt = port.interrupt_pin();
                let port = port.port();
                InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt }
            },
        }
    }

    fn anschlüsse(self) -> Anschlüsse {
        Anschlüsse {
            pwm_pins: Vec::new(),
            output_anschlüsse: Vec::new(),
            input_anschlüsse: vec![self],
        }
    }
}

/// Reserviere den übergebenen [`input::Pin`].
fn reserviere_input_pin(
    lager: &mut Lager,
    anschlüsse: Anschlüsse,
    pin: u8,
) -> Ergebnis<InputAnschluss> {
    match lager.pin.reserviere_pin(pin) {
        Ok(pin) => Ergebnis::Wert { anschluss: InputAnschluss::Pin(pin.als_input()), anschlüsse },
        Err(fehler) => Ergebnis::Fehler { fehler: NonEmpty::singleton(fehler.into()), anschlüsse },
    }
}

/// Reserviere den gewünschten [`pcf8574::InputPort`].
fn reserviere_input_port(
    lager: &mut Lager,
    anschlüsse: Anschlüsse,
    beschreibung: pcf8574::Beschreibung,
    port: kleiner_8,
    neuer_interrupt: Option<u8>,
    interrupt_konfigurieren: impl FnOnce(&mut Lager, &mut pcf8574::InputPort) -> Result<(), Fehler>,
) -> Ergebnis<InputAnschluss> {
    match lager.pcf8574.reserviere_pcf8574_port(beschreibung, port) {
        Ok(port) => {
            let bisheriger_interrupt_pin = lager.pcf8574.interrupt_pin(&beschreibung);
            let (mut input_port, fehler) = port.als_input();
            let fehler_interrupt = interrupt_konfigurieren(lager, &mut input_port).err();
            let input_anschluss = InputAnschluss::Pcf8574Port(input_port);
            let fehler_vec: Vec<_> = fehler
                .into_iter()
                .map(Fehler::from)
                .chain(
                    bisheriger_interrupt_pin
                        .map(|von| ReservierenFehler::Pcf8574InterruptPinGeändert {
                            beschreibung,
                            von,
                            zu: neuer_interrupt,
                        })
                        .map(Fehler::from),
                )
                .chain(fehler_interrupt.map(Fehler::from))
                .collect();
            if let Ok(non_empty) = NonEmpty::try_from(fehler_vec) {
                Ergebnis::WertMitWarnungen {
                    anschluss: input_anschluss,
                    fehler: non_empty,
                    anschlüsse,
                }
            } else {
                Ergebnis::Wert { anschluss: input_anschluss, anschlüsse }
            }
        },
        Err(fehler) => Ergebnis::Fehler { fehler: NonEmpty::singleton(fehler.into()), anschlüsse },
    }
}

impl Reserviere<InputAnschluss> for InputSerialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();
    fn reserviere(
        self,
        lager: &mut Lager,
        Anschlüsse { pwm_pins, output_anschlüsse, input_anschlüsse }: Anschlüsse,
        _move_arg: Self::MoveArg,
        _ref_arg: &Self::RefArg,
        _mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<InputAnschluss> {
        let mut gesuchter_anschluss = None;
        let mut gesuchter_interrupt = None;
        let mut verbleibende_input_anschlüsse = Vec::with_capacity(input_anschlüsse.len());
        let gesuchter_interrupt_serialisiert = self.interrupt();
        let mut anschluss_suchen: Box<dyn FnMut(InputAnschluss)> = if let Some(interrupt) =
            &gesuchter_interrupt_serialisiert
        {
            Box::new(|anschluss| {
                if gesuchter_anschluss.is_none() && self.selber_anschluss(&anschluss.serialisiere())
                {
                    gesuchter_anschluss = Some(anschluss);
                } else {
                    match anschluss {
                        InputAnschluss::Pin(pin)
                            if gesuchter_interrupt.is_none() && pin.pin() == *interrupt =>
                        {
                            gesuchter_interrupt = Some(pin);
                        },
                        InputAnschluss::Pin(_) | InputAnschluss::Pcf8574Port(_) => {
                            verbleibende_input_anschlüsse.push(anschluss);
                        },
                    }
                }
            })
        } else {
            Box::new(|anschluss| {
                if gesuchter_anschluss.is_none() && self.selber_anschluss(&anschluss.serialisiere())
                {
                    gesuchter_anschluss = Some(anschluss);
                } else {
                    verbleibende_input_anschlüsse.push(anschluss);
                }
            })
        };
        for anschluss in input_anschlüsse {
            anschluss_suchen(anschluss);
        }
        drop(anschluss_suchen);
        let interrupt_konfigurieren =
            |lager_arg: &mut Lager, port: &mut pcf8574::InputPort| -> Result<(), Fehler> {
                if let Some(interrupt) = gesuchter_interrupt {
                    let _ = port.setze_interrupt_pin(interrupt)?;
                } else if let Some(pin) = gesuchter_interrupt_serialisiert {
                    if Some(pin) != port.interrupt_pin() {
                        let interrupt = lager_arg.pin.reserviere_pin(pin)?.als_input();
                        let _ = port.setze_interrupt_pin(interrupt)?;
                    }
                } else {
                    // Kein konfigurierter Interrupt-Pin.
                }
                Ok(())
            };
        let anschlüsse = Anschlüsse {
            output_anschlüsse,
            input_anschlüsse: verbleibende_input_anschlüsse,
            pwm_pins,
        };
        if let Some(anschluss) = gesuchter_anschluss {
            Ergebnis::Wert { anschluss, anschlüsse }
        } else {
            match self {
                InputSerialisiert::Pin { pin } => reserviere_input_pin(lager, anschlüsse, pin),
                InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt: _ } => {
                    reserviere_input_port(
                        lager,
                        anschlüsse,
                        beschreibung,
                        port,
                        gesuchter_interrupt_serialisiert,
                        interrupt_konfigurieren,
                    )
                },
            }
        }
    }
}

/// Fehler, die beim reservieren eines [`Anschluss`]es auftreten können.
#[derive(Debug, Error)]
#[allow(variant_size_differences)]
pub enum ReservierenFehler {
    /// Ein Fehler beim reservieren eines [`Pin`]s.
    #[error(transparent)]
    Pin(#[from] pin::ReservierenFehler),
    /// Ein [`Pcf8574-Port`](pcf8574::Port) wird bereits verwendet.
    #[error(transparent)]
    Pcf8574(#[from] pcf8574::InVerwendung),
    /// Der Interrupt-Pin für einen [`Pcf8574`](pcf8574::Pcf8574) wurde angepasst.
    #[error("Interrupt-Pin für Pcf8574 {beschreibung} von {von} zu {zu:?} geändert.")]
    Pcf8574InterruptPinGeändert {
        /// Der [`Pcf8574`](pcf8574::Pcf8574), dessen Interrupt-Pin angepasst wurde.
        beschreibung: pcf8574::Beschreibung,
        /// Der bisherige Interrupt-Pin.
        von: u8,
        /// Der neue Interrupt-Pin.
        zu: Option<u8>,
    },
}

/// Fehler, die bei Interaktion mit einem [`Anschluss`] auftreten können.
#[derive(Debug, Error)]
pub enum Fehler {
    /// Ein Fehler mit einem [`Input-Pin`](input::Pin).
    #[error(transparent)]
    Input(#[from] input::Fehler),
    /// Ein Fehler mit einem [`Pcf8574-Port`](pcf8574::Port).
    #[error(transparent)]
    Pcf8574(#[from] pcf8574::Fehler),
    /// Ein Fehler beim Reservieren eines [`Anschluss`]es.
    #[error(transparent)]
    Reservieren(#[from] ReservierenFehler),
    /// Ein Gleis wurde mit unbekannter [`DefinitionId`] gespeichert.
    #[error("Die Definition mit der Id {id} für Typ {type_name} ist nicht bekannt!")]
    UnbekannteGespeicherteDefinition {
        /// Die gespeicherte Id.
        id: zugkontrolle_id::Repräsentation,
        /// Die Typ-Id, zu der die Id gehört.
        type_id: TypeId,
        /// Der Typ, zu der die Id gehört.
        type_name: &'static str,
    },
    /// Alle [`Ids`](crate::gleise::id::eindeutig::Id) wurden bereits verwendet.
    /// Es ist aktuell keine eindeutige [`Id`](crate::gleise::id::eindeutig::Id) verfügbar.
    #[error(transparent)]
    KeineIdVerfügbar(#[from] KeineIdVerfügbar),
}

impl From<pin::ReservierenFehler> for Fehler {
    fn from(fehler: pin::ReservierenFehler) -> Self {
        Fehler::Reservieren(fehler.into())
    }
}
impl From<pcf8574::InVerwendung> for Fehler {
    fn from(fehler: pcf8574::InVerwendung) -> Self {
        Fehler::Reservieren(fehler.into())
    }
}
