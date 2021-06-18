//! Mit Raspberry Pi schaltbarer Anschluss

use std::fmt::{self, Display, Formatter};

use ::serde::{Deserialize, Serialize};
use num_x::u3;

pub mod level;
pub use level::*;

pub mod polarity;
pub use polarity::*;

pub mod trigger;
pub use trigger::*;

pub mod pin;
pub use pin::*;

pub mod pcf8574;
pub use pcf8574::Pcf8574;

// path attribute necessary due to non-ascii module name (at least for now)
#[path = "anschluss/anschlüsse.rs"]
pub mod anschlüsse;
pub use anschlüsse::Anschlüsse;
use anschlüsse::SyncError;

pub mod serde;
pub use self::serde::*;

/// Ein Anschluss
#[derive(Debug)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(pcf8574::Port),
}

impl From<Pin> for Anschluss {
    fn from(pin: Pin) -> Self {
        Anschluss::Pin(pin)
    }
}
impl From<pcf8574::Port> for Anschluss {
    fn from(port: pcf8574::Port) -> Self {
        Anschluss::Pcf8574Port(port)
    }
}

fn write_level(f: &mut Formatter<'_>, level: &Level) -> fmt::Result {
    match level {
        Level::Low => write!(f, "L"),
        Level::High => write!(f, "H"),
    }
}
fn write_variante(f: &mut Formatter<'_>, variante: &pcf8574::Variante) -> fmt::Result {
    match variante {
        pcf8574::Variante::Normal => write!(f, " "),
        pcf8574::Variante::A => write!(f, "A"),
    }
}
fn write_adresse(
    f: &mut Formatter<'_>,
    (a0, a1, a2, variante): &(Level, Level, Level, pcf8574::Variante),
) -> fmt::Result {
    write_level(f, a0)?;
    write_level(f, a1)?;
    write_level(f, a2)?;
    write_variante(f, variante)
}

impl Display for Anschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Anschluss::Pin(pin) => write!(f, "Pin({})", pin.pin()),
            Anschluss::Pcf8574Port(port) => {
                write!(f, "Pcf8574Port(")?;
                write_adresse(f, port.adresse())?;
                write!(f, "-{})", port.port())
            }
        }
    }
}

impl Anschluss {
    pub fn into_output(self, polarität: Polarität) -> Result<OutputAnschluss, Error> {
        let gesperrt_level = Fließend::Gesperrt.with_polarity(polarität);
        Ok(match self {
            Anschluss::Pin(pin) => {
                OutputAnschluss::Pin { pin: pin.into_output(gesperrt_level), polarität }
            }
            Anschluss::Pcf8574Port(port) => {
                OutputAnschluss::Pcf8574Port { port: port.into_output(gesperrt_level)?, polarität }
            }
        })
    }

    pub fn into_input(self) -> Result<InputAnschluss, Error> {
        Ok(match self {
            Anschluss::Pin(pin) => InputAnschluss::Pin(pin.into_input()),
            Anschluss::Pcf8574Port(port) => InputAnschluss::Pcf8574Port(port.into_input()?),
        })
    }
}

/// Ein Anschluss, konfiguriert für Output.
#[derive(Debug)]
pub enum OutputAnschluss {
    Pin { pin: output::Pin, polarität: Polarität },
    Pcf8574Port { port: pcf8574::OutputPort, polarität: Polarität },
}

impl Display for OutputAnschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                write!(f, "Pin({}, {})", pin.pin(), polarität)
            }
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                write!(f, "Pcf8574Port(")?;
                write_adresse(f, port.adresse())?;
                write!(f, "-{}, {})", port.port(), polarität)
            }
        }
    }
}

impl OutputAnschluss {
    pub fn einstellen(&mut self, fließend: Fließend) -> Result<(), Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => {
                pin.write(fließend.with_polarity(*polarität))?
            }
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                port.write(fließend.with_polarity(*polarität))?
            }
        })
    }

    pub fn ist_fließend(&mut self) -> Result<bool, Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarität::Normal => pin.is_set_high()?,
                Polarität::Invertiert => pin.is_set_low()?,
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarität::Normal => port.is_set_high()?,
                Polarität::Invertiert => port.is_set_low()?,
            },
        })
    }

    pub fn ist_gesperrt(&mut self) -> Result<bool, Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarität::Normal => pin.is_set_low()?,
                Polarität::Invertiert => pin.is_set_high()?,
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarität::Normal => port.is_set_low()?,
                Polarität::Invertiert => port.is_set_high()?,
            },
        })
    }

    pub fn umstellen(&mut self) -> Result<(), Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, .. } => pin.toggle()?,
            OutputAnschluss::Pcf8574Port { port, .. } => port.toggle()?,
        })
    }
}

/// Serealisierbare Informationen eines OutputAnschlusses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OutputSave {
    Pin {
        pin: u8,
        polarität: Polarität,
    },
    Pcf8574Port {
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u8,
        polarität: Polarität,
    },
}
impl ToSave for OutputAnschluss {
    type Save = OutputSave;

    fn to_save(&self) -> OutputSave {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                OutputSave::Pin { pin: pin.pin(), polarität: *polarität }
            }
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                let (a0, a1, a2, variante) = port.adresse();
                let port = port.port();
                OutputSave::Pcf8574Port {
                    a0: *a0,
                    a1: *a1,
                    a2: *a2,
                    variante: *variante,
                    port: port.into(),
                    polarität: *polarität,
                }
            }
        }
    }
}
impl Reserviere<OutputAnschluss> for OutputSave {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<OutputAnschluss, Error> {
        let (anschluss, polarität) = match self {
            OutputSave::Pin { pin, polarität } => {
                (Anschluss::from(anschlüsse.reserviere_pin(pin)?), polarität)
            }
            OutputSave::Pcf8574Port { a0, a1, a2, variante, port, polarität } => {
                let port = u3::new(port);
                (anschlüsse.reserviere_pcf8574_port(a0, a1, a2, variante, port)?.into(), polarität)
            }
        };
        anschluss.into_output(polarität)
    }
}

/// Ein Anschluss, konfiguriert für Input.
#[derive(Debug)]
pub enum InputAnschluss {
    Pin(input::Pin),
    Pcf8574Port(pcf8574::InputPort),
}

impl Display for InputAnschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InputAnschluss::Pin(pin) => write!(f, "Pin({})", pin.pin()),
            InputAnschluss::Pcf8574Port(port) => {
                write!(f, "Pcf8574Port(")?;
                write_adresse(f, port.adresse())?;
                write!(f, "-{})", port.port())
            }
        }
    }
}

macro_rules! match_method {
    ( $method:ident$(($($arg:ident : $arg_ty: ty),+))?) => {
        match_method! {$method$(($($arg : $arg_ty),+))? -> ()}
    };
    ($method:ident$(($($arg:ident : $arg_ty: ty),+))? -> $result:ty) => {
        pub fn $method(&mut self$(, $($arg: $arg_ty),+)?) -> Result<$result, Error> {
            Ok(match self {
                InputAnschluss::Pin(pin) => pin.$method($($($arg),+)?)?,
                InputAnschluss::Pcf8574Port(port) => port.$method($($($arg),+)?)?,
            })
        }
    };
}

impl InputAnschluss {
    match_method! {read -> Level}

    match_method! {set_async_interrupt(trigger: Trigger, callback: impl FnMut(Level) + Send + 'static)}

    match_method! {clear_async_interrupt}
}

/// Serealisierbare Informationen eines InputAnschlusses.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InputSave {
    Pin {
        pin: u8,
    },
    Pcf8574Port {
        a0: Level,
        a1: Level,
        a2: Level,
        variante: pcf8574::Variante,
        port: u8,
        interrupt: Option<u8>,
    },
}
impl ToSave for InputAnschluss {
    type Save = InputSave;

    fn to_save(&self) -> InputSave {
        match self {
            InputAnschluss::Pin(pin) => InputSave::Pin { pin: pin.pin() },
            InputAnschluss::Pcf8574Port(port) => {
                let (a0, a1, a2, variante) = port.adresse();
                let interrupt = port.interrupt_pin().unwrap_or(None);
                let port = port.port();
                InputSave::Pcf8574Port {
                    a0: *a0,
                    a1: *a1,
                    a2: *a2,
                    variante: *variante,
                    port: port.into(),
                    interrupt,
                }
            }
        }
    }
}
impl Reserviere<InputAnschluss> for InputSave {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<InputAnschluss, Error> {
        Ok(match self {
            InputSave::Pin { pin } => {
                InputAnschluss::Pin(anschlüsse.reserviere_pin(pin)?.into_input())
            }
            InputSave::Pcf8574Port { a0, a1, a2, variante, port, interrupt } => {
                let port =
                    anschlüsse.reserviere_pcf8574_port(a0, a1, a2, variante, u3::new(port))?;
                let mut input_port = port.into_input()?;
                if input_port.interrupt_pin()? != interrupt {
                    if let Some(pin) = interrupt {
                        let interrupt = anschlüsse.reserviere_pin(pin)?.into_input();
                        let _ = input_port.set_interrupt_pin(interrupt);
                    }
                }
                InputAnschluss::Pcf8574Port(input_port)
            }
        })
    }
}

#[derive(Debug)]
pub enum Error {
    Anschlüsse(anschlüsse::Error),
    Output(output::Error),
    Input(input::Error),
    Pcf8574(pcf8574::Error),
}
impl From<SyncError> for Error {
    fn from(error: SyncError) -> Self {
        Error::Anschlüsse(error.into())
    }
}
impl From<anschlüsse::Error> for Error {
    fn from(error: anschlüsse::Error) -> Self {
        Error::Anschlüsse(error)
    }
}
impl From<output::Error> for Error {
    fn from(error: output::Error) -> Self {
        Error::Output(error)
    }
}
impl From<input::Error> for Error {
    fn from(error: input::Error) -> Self {
        Error::Input(error)
    }
}
impl From<pcf8574::Error> for Error {
    fn from(error: pcf8574::Error) -> Self {
        Error::Pcf8574(error)
    }
}
