//! Mit Raspberry Pi schaltbarer Anschluss

use std::fmt::{self, Display, Formatter};

use log::error;
use num_x::u3;
use serde::{Deserialize, Serialize};

pub mod level;
pub use level::*;

#[path = "anschluss/polarität.rs"]
pub mod polarität;
pub use polarität::*;

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

pub mod speichern_laden;
pub use self::speichern_laden::{Reserviere, Reserviert, ToSave};

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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
impl OutputSave {
    // Handelt es sich um den selben Anschluss, unabhängig von der Polarität.
    pub fn selber_anschluss(&self, other: &OutputSave) -> bool {
        match (self, other) {
            (OutputSave::Pin { pin: p0, .. }, OutputSave::Pin { pin: p1, .. }) => p0 == p1,
            (
                OutputSave::Pcf8574Port {
                    a0: a0_a,
                    a1: a1_a,
                    a2: a2_a,
                    variante: variante_a,
                    port: port_a,
                    ..
                },
                OutputSave::Pcf8574Port {
                    a0: a0_b,
                    a1: a1_b,
                    a2: a2_b,
                    variante: variante_b,
                    port: port_b,
                    ..
                },
            ) => {
                a0_a == a0_b
                    && a1_a == a1_b
                    && a2_a == a2_b
                    && variante_a == variante_b
                    && port_a == port_b
            }
            _ => false,
        }
    }
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

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (Vec::new(), vec![self], Vec::new())
    }
}
impl Reserviere<OutputAnschluss> for OutputSave {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern_laden::Result<OutputAnschluss> {
        let polarität = match self {
            OutputSave::Pin { polarität, .. } => polarität,
            OutputSave::Pcf8574Port { polarität, .. } => polarität,
        };
        let (mut gesucht, output_nicht_benötigt): (Vec<_>, Vec<_>) = output_anschlüsse
            .into_iter()
            .partition(|anschluss| self.selber_anschluss(&anschluss.to_save()));
        let anschluss = if let Some(anschluss) = gesucht.pop() {
            match anschluss {
                OutputAnschluss::Pin { pin, .. } => OutputAnschluss::Pin { pin, polarität },
                OutputAnschluss::Pcf8574Port { port, .. } => {
                    OutputAnschluss::Pcf8574Port { port, polarität }
                }
            }
        } else {
            macro_rules! fehler {
                ($error:expr) => {
                    return Err(speichern_laden::Error {
                        fehler: $error.into(),
                        pwm_pins,
                        output_anschlüsse: output_nicht_benötigt,
                        input_anschlüsse,
                    })
                };
            }
            let (anschluss, polarität) = match self {
                OutputSave::Pin { pin, polarität } => (
                    Anschluss::Pin(match anschlüsse.reserviere_pin(pin) {
                        Ok(pin) => pin,
                        Err(error) => fehler!(error),
                    }),
                    polarität,
                ),
                OutputSave::Pcf8574Port { a0, a1, a2, variante, port, polarität } => {
                    let port = u3::new(port);
                    (
                        Anschluss::Pcf8574Port(
                            match anschlüsse.reserviere_pcf8574_port(a0, a1, a2, variante, port) {
                                Ok(port) => port,
                                Err(error) => fehler!(error),
                            },
                        ),
                        polarität,
                    )
                }
            };
            match anschluss.into_output(polarität) {
                Ok(anschluss) => anschluss,
                Err(error) => fehler!(error),
            }
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
impl InputSave {
    // Handelt es sich um den selben Anschluss, unabhängig vom Interrupt Pin.
    pub fn selber_anschluss(&self, other: &InputSave) -> bool {
        match (self, other) {
            (InputSave::Pin { pin: p0 }, InputSave::Pin { pin: p1 }) => p0 == p1,
            (
                InputSave::Pcf8574Port {
                    a0: a0_a,
                    a1: a1_a,
                    a2: a2_a,
                    variante: variante_a,
                    port: port_a,
                    ..
                },
                InputSave::Pcf8574Port {
                    a0: a0_b,
                    a1: a1_b,
                    a2: a2_b,
                    variante: variante_b,
                    port: port_b,
                    ..
                },
            ) => {
                a0_a == a0_b
                    && a1_a == a1_b
                    && a2_a == a2_b
                    && variante_a == variante_b
                    && port_a == port_b
            }
            _ => false,
        }
    }

    pub fn interrupt(&self) -> Option<u8> {
        if let InputSave::Pcf8574Port { interrupt, .. } = self {
            interrupt.clone()
        } else {
            None
        }
    }
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

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (Vec::new(), Vec::new(), vec![self])
    }
}
impl Reserviere<InputAnschluss> for InputSave {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern_laden::Result<InputAnschluss> {
        let (gesuchter_anschluss, input_nicht_benötigt) =
            input_anschlüsse.into_iter().fold((None, Vec::new()), |mut acc, anschluss| {
                if self.selber_anschluss(&anschluss.to_save()) {
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
                    }
                    (anschluss, _self_interrupt) => acc.1.push(anschluss),
                }
                acc
            });
        let interrupt_konfigurieren =
            |anschlüsse: &mut Anschlüsse, mut anschluss| -> Result<_, Error> {
                if let InputAnschluss::Pcf8574Port(port) = &mut anschluss {
                    if let Some(interrupt) = gesuchter_interrupt {
                        port.set_interrupt_pin(interrupt)?;
                    } else if let Some(pin) = self_interrupt {
                        if Some(pin) != port.interrupt_pin()? {
                            let interrupt = anschlüsse.reserviere_pin(pin)?.into_input();
                            port.set_interrupt_pin(interrupt)?;
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
        macro_rules! fehler {
            ($error:expr) => {
                return Err(speichern_laden::Error {
                    fehler: $error.into(),
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse: input_nicht_benötigt,
                })
            };
        }
        let anschluss = if let Some(anschluss) = gesuchter_anschluss {
            match interrupt_konfigurieren(anschlüsse, anschluss) {
                Ok(anschluss) => anschluss,
                Err(error) => fehler!(error),
            }
        } else {
            match self {
                InputSave::Pin { pin } => {
                    InputAnschluss::Pin(match anschlüsse.reserviere_pin(pin) {
                        Ok(anschluss) => anschluss.into_input(),
                        Err(error) => fehler!(error),
                    })
                }
                InputSave::Pcf8574Port { a0, a1, a2, variante, port, interrupt: _ } => {
                    let port = match anschlüsse.reserviere_pcf8574_port(
                        a0,
                        a1,
                        a2,
                        variante,
                        u3::new(port),
                    ) {
                        Ok(port) => port,
                        Err(error) => fehler!(error),
                    };
                    let input_port = match port.into_input() {
                        Ok(input_port) => input_port,
                        Err(error) => fehler!(error),
                    };
                    match interrupt_konfigurieren(
                        anschlüsse,
                        InputAnschluss::Pcf8574Port(input_port),
                    ) {
                        Ok(anschluss) => anschluss,
                        Err(error) => fehler!(error),
                    }
                }
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
