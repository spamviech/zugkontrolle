//! Mit Raspberry Pi schaltbarer Anschluss

use std::{
    fmt::{self, Display, Formatter},
    ops::Not,
};

use log::error;
use serde::{Deserialize, Serialize};

pub use self::{
    de_serialisieren::{Reserviere, Reserviert, Serialisiere},
    pcf8574::I2cBus,
};
use crate::{kleiner_als::kleiner_8, rppal};

pub mod level;
pub use level::Level;

#[path = "anschluss/polarität.rs"]
pub mod polarität;
pub use polarität::{Fließend, Polarität};

pub mod trigger;
pub use trigger::Trigger;

pub mod pin;
pub use pin::{input, output, pwm, Pin};

pub mod pcf8574;
pub use pcf8574::Pcf8574;

pub mod de_serialisieren;

#[derive(Debug)]
pub struct Lager {
    pub pin: pin::Lager,
    pub pcf8574: pcf8574::Lager,
}

#[derive(Debug, zugkontrolle_macros::From)]
pub enum InitFehler {
    Pin(rppal::gpio::Error),
    Pcf8574(pcf8574::InitFehler),
}

impl Lager {
    pub fn neu(settings: pcf8574::I2cSettings) -> Result<Lager, InitFehler> {
        let mut pin = pin::Lager::neu()?;
        let pcf8574 = pcf8574::Lager::neu(&mut pin, settings)?;
        Ok(Lager { pin, pcf8574 })
    }

    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Anschluss, ReservierenFehler> {
        match self.pin.reserviere_pin(pin) {
            Ok(pin) => Ok(Anschluss::Pin(pin)),
            Err(fehler) => Err(fehler.into()),
        }
    }

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

/// Ein Anschluss
#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(pcf8574::Port),
}

impl Display for Anschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Anschluss::Pin(pin) => write!(f, "Pin({})", pin.pin()),
            Anschluss::Pcf8574Port(port) => {
                write!(f, "Pcf8574Port({port})")
            },
        }
    }
}

impl Anschluss {
    pub fn als_output(self, polarität: Polarität) -> Result<OutputAnschluss, Fehler> {
        let gesperrt_level = Fließend::Gesperrt.with_polarity(polarität);
        Ok(match self {
            Anschluss::Pin(pin) => {
                OutputAnschluss::Pin { pin: pin.als_output(gesperrt_level), polarität }
            },
            Anschluss::Pcf8574Port(port) => {
                OutputAnschluss::Pcf8574Port { port: port.als_output(gesperrt_level)?, polarität }
            },
        })
    }

    pub fn als_input(self) -> Result<InputAnschluss, Fehler> {
        Ok(match self {
            Anschluss::Pin(pin) => InputAnschluss::Pin(pin.als_input()),
            Anschluss::Pcf8574Port(port) => InputAnschluss::Pcf8574Port(port.als_input()?),
        })
    }
}

/// Ein Anschluss, konfiguriert für Output.
#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum OutputAnschluss {
    Pin { pin: output::Pin, polarität: Polarität },
    Pcf8574Port { port: pcf8574::OutputPort, polarität: Polarität },
}

impl Display for OutputAnschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                write!(f, "Pin({}, {polarität})", pin.pin())
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                write!(f, "Pcf8574Port({port}, {polarität}")
            },
        }
    }
}

impl OutputAnschluss {
    pub fn einstellen(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => {
                pin.schreibe(fließend.with_polarity(*polarität))
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                port.schreibe(fließend.with_polarity(*polarität))?
            },
        })
    }

    pub fn ist_fließend(&mut self) -> Result<bool, Fehler> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarität::Normal => pin.ist_high(),
                Polarität::Invertiert => pin.ist_low(),
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarität::Normal => port.ist_high()?,
                Polarität::Invertiert => port.ist_low()?,
            },
        })
    }

    pub fn ist_gesperrt(&mut self) -> Result<bool, Fehler> {
        self.ist_fließend().map(bool::not)
    }

    pub fn umschalten(&mut self) -> Result<(), Fehler> {
        Ok(match self {
            OutputAnschluss::Pin { pin, .. } => pin.umschalten(),
            OutputAnschluss::Pcf8574Port { port, .. } => port.umschalten()?,
        })
    }
}

/// Serealisierbare Informationen eines OutputAnschlusses.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OutputSerialisiert {
    Pin { pin: u8, polarität: Polarität },
    Pcf8574Port { beschreibung: pcf8574::Beschreibung, port: kleiner_8, polarität: Polarität },
}

impl OutputSerialisiert {
    // Handelt es sich um den selben Anschluss, unabhängig von der Polarität.
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

impl Serialisiere for OutputAnschluss {
    type Serialisiert = OutputSerialisiert;

    fn serialisiere(&self) -> OutputSerialisiert {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                OutputSerialisiert::Pin { pin: pin.pin(), polarität: *polarität }
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                let beschreibung = port.adresse();
                let port = port.port();
                OutputSerialisiert::Pcf8574Port {
                    beschreibung: *beschreibung,
                    port: port.into(),
                    polarität: *polarität,
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (Vec::new(), vec![self], Vec::new())
    }
}

impl Reserviere<OutputAnschluss> for OutputSerialisiert {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<OutputAnschluss> {
        let polarität = match self {
            OutputSerialisiert::Pin { polarität, .. } => polarität,
            OutputSerialisiert::Pcf8574Port { polarität, .. } => polarität,
        };
        let (mut gesucht, output_nicht_benötigt): (Vec<_>, Vec<_>) = output_anschlüsse
            .into_iter()
            .partition(|anschluss| self.selber_anschluss(&anschluss.serialisiere()));
        let anschluss = if let Some(anschluss) = gesucht.pop() {
            match anschluss {
                OutputAnschluss::Pin { pin, .. } => OutputAnschluss::Pin { pin, polarität },
                OutputAnschluss::Pcf8574Port { port, .. } => {
                    OutputAnschluss::Pcf8574Port { port, polarität }
                },
            }
        } else {
            macro_rules! unwrap_return {
                ($result:expr) => {
                    match $result {
                        Ok(anschluss) => anschluss,
                        Err(fehler) => {
                            return Err(de_serialisieren::Fehler {
                                fehler: fehler.into(),
                                pwm_pins,
                                output_anschlüsse: output_nicht_benötigt,
                                input_anschlüsse,
                            })
                        },
                    }
                };
            }
            let (anschluss_res, polarität) = match self {
                OutputSerialisiert::Pin { pin, polarität } => {
                    (lager.reserviere_pin(pin), polarität)
                },
                OutputSerialisiert::Pcf8574Port { beschreibung, port, polarität } => {
                    (lager.reserviere_pcf8574_port(beschreibung, port), polarität)
                },
            };
            let anschluss = unwrap_return!(anschluss_res);
            unwrap_return!(anschluss.als_output(polarität))
        };
        Ok(Reserviert {
            anschluss,
            pwm_nicht_benötigt: pwm_pins,
            output_nicht_benötigt,
            input_nicht_benötigt: input_anschlüsse,
        })
    }
}

/// Ein Anschluss, konfiguriert für Input.
#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum InputAnschluss {
    Pin(input::Pin),
    Pcf8574Port(pcf8574::InputPort),
}

impl Display for InputAnschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InputAnschluss::Pin(pin) => write!(f, "Pin({})", pin.pin()),
            InputAnschluss::Pcf8574Port(port) => {
                write!(f, "Pcf8574Port({port})")
            },
        }
    }
}

macro_rules! match_method {
    ( $method:ident$(($($arg:ident : $arg_ty: ty),+))?) => {
        match_method! {$method$(($($arg : $arg_ty),+))? -> ()}
    };
    ($method:ident$(($($arg:ident : $arg_ty: ty),+))? -> $result:ty) => {
        pub fn $method(&mut self$(, $($arg: $arg_ty),+)?) -> Result<$result, Fehler> {
            Ok(match self {
                InputAnschluss::Pin(pin) => pin.$method($($($arg),+)?)?,
                InputAnschluss::Pcf8574Port(port) => port.$method($($($arg),+)?)?,
            })
        }
    };
}

impl InputAnschluss {
    // /// Lese das aktuell am [InputAnschluss] anliegende [Level].
    match_method! {lese -> Level}

    // /// Konfiguriere einen asynchronen Interrupt Trigger.
    // /// Bei auftreten wird der callback in einem separaten Thread ausgeführt.
    // ///
    // /// Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald [setze_async_interrupt]
    // /// oder [lösche_async_interrupt] aufgerufen wird, oder der [InputPin] out of scope geht.
    // ///
    // /// ## Keine synchronen Interrupts
    // /// Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.
    // /// Siehe die Dokumentation der
    // /// [poll_interrupts](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)
    // /// Methode.
    // /// > Calling poll_interrupts blocks any other calls to poll_interrupts or
    // /// > InputPin::poll_interrupt until it returns. If you need to poll multiple pins simultaneously
    // /// > on different threads, consider using asynchronous interrupts with
    // /// > InputPin::set_async_interrupt instead.
    match_method! {setze_async_interrupt(trigger: Trigger, callback: impl Fn(Level) + Send + Sync + 'static)}

    // /// Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.
    match_method! {lösche_async_interrupt}
}

/// Serealisierbare Informationen eines InputAnschlusses.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InputSerialisiert {
    Pin { pin: u8 },
    Pcf8574Port { beschreibung: pcf8574::Beschreibung, port: kleiner_8, interrupt: Option<u8> },
}

impl InputSerialisiert {
    // Handelt es sich um den selben Anschluss, unabhängig vom Interrupt Pin.
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

    pub fn interrupt(&self) -> Option<u8> {
        if let InputSerialisiert::Pcf8574Port { interrupt, .. } = self {
            interrupt.clone()
        } else {
            None
        }
    }
}

impl Serialisiere for InputAnschluss {
    type Serialisiert = InputSerialisiert;

    fn serialisiere(&self) -> InputSerialisiert {
        match self {
            InputAnschluss::Pin(pin) => InputSerialisiert::Pin { pin: pin.pin() },
            InputAnschluss::Pcf8574Port(port) => {
                let beschreibung = *port.adresse();
                let interrupt = port.interrupt_pin().unwrap_or(None);
                let port = port.port();
                InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (Vec::new(), Vec::new(), vec![self])
    }
}

impl Reserviere<InputAnschluss> for InputSerialisiert {
    fn reserviere(
        self,
        lager: &mut Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<InputAnschluss> {
        let (gesuchter_anschluss, input_nicht_benötigt) =
            input_anschlüsse.into_iter().fold((None, Vec::new()), |mut acc, anschluss| {
                if self.selber_anschluss(&anschluss.serialisiere()) {
                    acc.0 = Some(anschluss)
                } else {
                    acc.1.push(anschluss)
                }
                acc
            });
        let self_interrupt = self.interrupt();
        let (gesuchter_interrupt, input_nicht_benötigt) =
            input_nicht_benötigt.into_iter().fold((None, Vec::new()), |mut acc, anschluss| {
                match (anschluss, self_interrupt) {
                    (InputAnschluss::Pin(pin), Some(save)) if pin.pin() == save => {
                        acc.0 = Some(pin)
                    },
                    (anschluss, _self_interrupt) => acc.1.push(anschluss),
                }
                acc
            });
        let interrupt_konfigurieren = |mut anschluss| -> Result<_, Fehler> {
            if let InputAnschluss::Pcf8574Port(port) = &mut anschluss {
                if let Some(interrupt) = gesuchter_interrupt {
                    let _ = port.setze_interrupt_pin(interrupt)?;
                } else if let Some(pin) = self_interrupt {
                    if Some(pin) != port.interrupt_pin()? {
                        let interrupt = lager.pin.reserviere_pin(pin)?.als_input();
                        let _ = port.setze_interrupt_pin(interrupt)?;
                    }
                }
            } else if let Some(interrupt) = self_interrupt {
                error!(
                    "Interrupt Pin {} für einen InputPin {:?} konfiguriert.",
                    interrupt, anschluss
                )
            }
            Ok(anschluss)
        };
        macro_rules! unwrap_return {
            ($result:expr) => {
                match $result {
                    Ok(anschluss) => anschluss,
                    Err(fehler) => {
                        return Err(de_serialisieren::Fehler {
                            fehler: fehler.into(),
                            pwm_pins,
                            output_anschlüsse,
                            input_anschlüsse: input_nicht_benötigt,
                        })
                    },
                }
            };
        }
        let anschluss = if let Some(anschluss) = gesuchter_anschluss {
            unwrap_return!(interrupt_konfigurieren(anschluss))
        } else {
            match self {
                InputSerialisiert::Pin { pin } => {
                    InputAnschluss::Pin(unwrap_return!(lager.pin.reserviere_pin(pin)).als_input())
                },
                InputSerialisiert::Pcf8574Port { beschreibung, port, interrupt: _ } => {
                    let port =
                        unwrap_return!(lager.pcf8574.reserviere_pcf8574_port(beschreibung, port));
                    let input_port = unwrap_return!(port.als_input());
                    unwrap_return!(interrupt_konfigurieren(InputAnschluss::Pcf8574Port(input_port)))
                },
            }
        };
        Ok(Reserviert {
            anschluss,
            pwm_nicht_benötigt: pwm_pins,
            output_nicht_benötigt: output_anschlüsse,
            input_nicht_benötigt,
        })
    }
}

#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum ReservierenFehler {
    Pin(pin::ReservierenFehler),
    Pcf8574(pcf8574::InVerwendung),
}

#[derive(Debug, zugkontrolle_macros::From)]
pub enum Fehler {
    Input(input::Fehler),
    Pcf8574(pcf8574::Fehler),
    Reservieren(ReservierenFehler),
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
