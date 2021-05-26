//! Mit Raspberry Pi schaltbarer Anschluss

use std::fmt::{self, Display, Formatter};

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
            },
        }
    }
}

impl Anschluss {
    pub fn into_output(self, polarität: Polarity) -> Result<OutputAnschluss, Error> {
        Ok(match self {
            Anschluss::Pin(pin) => OutputAnschluss::Pin { pin: pin.into_output(), polarität },
            Anschluss::Pcf8574Port(port) => {
                OutputAnschluss::Pcf8574Port { port: port.into_output()?, polarität }
            },
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
    Pin { pin: output::Pin, polarität: Polarity },
    Pcf8574Port { port: pcf8574::OutputPort, polarität: Polarity },
}

impl Display for OutputAnschluss {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OutputAnschluss::Pin { pin, polarität } => {
                write!(f, "Pin({}, {})", pin.pin(), polarität)
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                write!(f, "Pcf8574Port(")?;
                write_adresse(f, port.adresse())?;
                write!(f, "-{}, {})", port.port(), polarität)
            },
        }
    }
}

impl OutputAnschluss {
    pub fn einstellen(&mut self, fließend: Fließend) -> Result<(), Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => {
                pin.write(fließend.with_polarity(*polarität))?
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                port.write(fließend.with_polarity(*polarität))?
            },
        })
    }

    pub fn ist_fließend(&mut self) -> Result<bool, Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarity::Normal => pin.is_set_high()?,
                Polarity::Inverse => pin.is_set_low()?,
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarity::Normal => port.is_set_high()?,
                Polarity::Inverse => port.is_set_low()?,
            },
        })
    }

    pub fn ist_gesperrt(&mut self) -> Result<bool, Error> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => match polarität {
                Polarity::Normal => pin.is_set_low()?,
                Polarity::Inverse => pin.is_set_high()?,
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => match polarität {
                Polarity::Normal => port.is_set_low()?,
                Polarity::Inverse => port.is_set_high()?,
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
            },
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

#[derive(Debug)]
pub enum Error {
    Output(output::Error),
    Input(input::Error),
    Pcf8574(pcf8574::Error),
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
