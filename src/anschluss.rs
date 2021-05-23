//! Mit Raspberry Pi schaltbarer Anschluss

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

impl Anschluss {
    pub fn into_output(self) -> Result<OutputAnschluss, Error> {
        Ok(match self {
            Anschluss::Pin(pin) => OutputAnschluss::Pin(pin.into_output()),
            Anschluss::Pcf8574Port(port) => OutputAnschluss::Pcf8574Port(port.into_output()?),
        })
    }

    pub fn into_input(self) -> Result<InputAnschluss, Error> {
        Ok(match self {
            Anschluss::Pin(pin) => InputAnschluss::Pin(pin.into_input()),
            Anschluss::Pcf8574Port(port) => InputAnschluss::Pcf8574Port(port.into_input()?),
        })
    }
}

macro_rules! match_method {
    ($export: ident => $method:ident$(($($arg:ident : $arg_ty: ty),+))?) => {
        match_method! {$export => $method$(($($arg : $arg_ty),+))? -> ()}
    };
    ($export: ident => $method:ident$(($($arg:ident : $arg_ty: ty),+))? -> $result:ty) => {
        pub fn $method(&mut self$(, $($arg: $arg_ty),+)?) -> Result<$result, Error> {
            Ok(match self {
                $export::Pin(pin) => pin.$method($($($arg),+)?)?,
                $export::Pcf8574Port(port) => port.$method($($($arg),+)?)?,
            })
        }
    };
}

/// Ein Anschluss, konfiguriert für Output.
#[derive(Debug)]
pub enum OutputAnschluss {
    Pin(output::Pin),
    Pcf8574Port(pcf8574::OutputPort),
}

impl From<output::Pin> for OutputAnschluss {
    fn from(pin: output::Pin) -> Self {
        OutputAnschluss::Pin(pin)
    }
}
impl From<pcf8574::OutputPort> for OutputAnschluss {
    fn from(port: pcf8574::OutputPort) -> Self {
        OutputAnschluss::Pcf8574Port(port)
    }
}

impl OutputAnschluss {
    match_method! {OutputAnschluss => write(level:Level)}

    match_method! {OutputAnschluss => is_set_high -> bool}

    match_method! {OutputAnschluss => is_set_low -> bool}

    match_method! {OutputAnschluss => toggle}
}

/// Ein Anschluss, konfiguriert für Input.
#[derive(Debug)]
pub enum InputAnschluss {
    Pin(input::Pin),
    Pcf8574Port(pcf8574::InputPort),
}

impl From<input::Pin> for InputAnschluss {
    fn from(pin: input::Pin) -> Self {
        InputAnschluss::Pin(pin)
    }
}
impl From<pcf8574::InputPort> for InputAnschluss {
    fn from(port: pcf8574::InputPort) -> Self {
        InputAnschluss::Pcf8574Port(port)
    }
}

impl InputAnschluss {
    match_method! {InputAnschluss => read -> Level}

    match_method! {InputAnschluss => set_async_interrupt(trigger: Trigger, callback: impl FnMut(Level) + Send + 'static)}

    match_method! {InputAnschluss => clear_async_interrupt}
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
