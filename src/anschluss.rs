//! Mit Raspberry Pi schaltbarer Anschluss.

use std::{
    fmt::{self, Display, Formatter},
    sync::mpsc::RecvError,
};

use log::error;
use serde::{Deserialize, Serialize};

use crate::{eingeschränkt::kleiner_8, rppal, zugtyp::FalscherLeiter};

pub use self::{
    de_serialisieren::{Reserviere, Reserviert, Serialisiere},
    pcf8574::I2cBus,
};

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

/// Verwalten nicht verwendeter [Pin]s und [Pcf8574-Ports](pcf8574::Port).
#[derive(Debug)]
pub struct Lager {
    /// [Lager](pin::Lager) zum verwalten nicht verwendeter [Pin]s.
    pub pin: pin::Lager,
    /// [Lager](pcf8574::Lager) zum verwalten nicht verwendeter [Pcf8574-Ports](pcf8574::Port).
    pub pcf8574: pcf8574::Lager,
}

/// Fehler beim [Initialisieren](Lager::neu) eines [Lager]s.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum InitFehler {
    /// Fehler beim Initialisieren des [Pin-Lagers](pin::Lager).
    Pin(rppal::gpio::Error),
    /// Fehler beim Initialisieren des [Pcf8574-Lagers](pcf8574::Lager).
    Pcf8574(pcf8574::InitFehler),
}

impl Lager {
    /// Initialisiere ein [Lager], das nicht verwendete [Anschlüsse](Anschluss) verwaltet.
    pub fn neu(settings: pcf8574::I2cSettings) -> Result<Lager, InitFehler> {
        let mut pin = pin::Lager::neu()?;
        let pcf8574 = pcf8574::Lager::neu(&mut pin, settings)?;
        Ok(Lager { pin, pcf8574 })
    }

    /// Reserviere einen [Pin].
    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Anschluss, ReservierenFehler> {
        match self.pin.reserviere_pin(pin) {
            Ok(pin) => Ok(Anschluss::Pin(pin)),
            Err(fehler) => Err(fehler.into()),
        }
    }

    /// Reserviere einen [Pcf8574-Port](pcf8574::Port).
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
    /// Ein [Pin].
    Pin(Pin),
    /// Ein [Pcf8574-Port](pcf8574::Port).
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
    /// Konfiguriere den [Anschluss] als [Output](OutputAnschluss).
    pub fn als_output(self, polarität: Polarität) -> Result<OutputAnschluss, Fehler> {
        let gesperrt_level = Fließend::Gesperrt.mit_polarität(polarität);
        Ok(match self {
            Anschluss::Pin(pin) => {
                OutputAnschluss::Pin { pin: pin.als_output(gesperrt_level), polarität }
            },
            Anschluss::Pcf8574Port(port) => {
                OutputAnschluss::Pcf8574Port { port: port.als_output(gesperrt_level)?, polarität }
            },
        })
    }

    /// Konfiguriere den [Anschluss] als [Input](InputAnschluss).
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
    /// Ein [Pin](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: output::Pin,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [Pcf8574-Port](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die [Pcf8574-Port](pcf8574::OutputPort).
        port: pcf8574::OutputPort,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
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
    /// Einstellen des [OutputAnschluss]es.
    pub fn einstellen(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        Ok(match self {
            OutputAnschluss::Pin { pin, polarität } => {
                pin.schreibe(fließend.mit_polarität(*polarität))
            },
            OutputAnschluss::Pcf8574Port { port, polarität } => {
                port.schreibe(fließend.mit_polarität(*polarität))?
            },
        })
    }

    /// Aktuelle Einstellung des [OutputAnschlusses](OutputAnschluss).
    pub fn fließend(&self) -> Fließend {
        if self.ist_fließend() {
            Fließend::Fließend
        } else {
            Fließend::Gesperrt
        }
    }

    /// Ist der [OutputAnschluss] aktuell [fließend](Fließend::Fließend).
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

    /// Ist der [OutputAnschluss] aktuell [gesperrt](Fließend::Gesperrt).
    #[inline(always)]
    pub fn ist_gesperrt(&self) -> bool {
        !self.ist_fließend()
    }

    /// Schalte den Strom von [Fließend](Fließend::Fließend) auf [Gesperrt](Fließend::Gesperrt)
    /// und umgekehrt.
    pub fn umschalten(&mut self) -> Result<(), Fehler> {
        Ok(match self {
            OutputAnschluss::Pin { pin, .. } => pin.umschalten(),
            OutputAnschluss::Pcf8574Port { port, .. } => port.umschalten()?,
        })
    }
}

/// Serialisierbare Informationen eines [OutputAnschluss]es.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OutputSerialisiert {
    /// Ein [Pin](output::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
    /// Ein [Pcf8574-Port](pcf8574::OutputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: pcf8574::Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Die [Polarität] des Anschlusses.
        polarität: Polarität,
    },
}

impl OutputSerialisiert {
    /// Handelt es sich um den selben Anschluss, unabhängig von der Polarität.
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
                let beschreibung = port.beschreibung();
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
    /// Ein [Pin](input::Pin).
    Pin(input::Pin),
    /// Ein [Pcf8574-Port](pcf8574::InputPort).
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
    }

    match_method! {
        setze_async_interrupt(trigger: Trigger, callback: impl Fn(Level) + Send + Sync + 'static),"Konfiguriere einen asynchronen Interrupt Trigger.",
        "Bei auftreten wird der callback in einem separaten Thread ausgeführt.",
        "",
        "Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald [setze_async_interrupt]",
        "oder [lösche_async_interrupt] aufgerufen wird, oder der [InputPin] out of scope geht.",
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
    }
}

/// Serialisierbare Informationen eines [InputAnschluss]es.
#[allow(missing_copy_implementations, variant_size_differences)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InputSerialisiert {
    /// Ein [Pin](input::Pin).
    Pin {
        /// Die GPIO-Zahl.
        pin: u8,
    },
    /// Ein [Pcf8574-Port](pcf8574::InputPort).
    Pcf8574Port {
        /// Die Beschreibung des Pcf8574.
        beschreibung: pcf8574::Beschreibung,
        /// Der verwendete Port.
        port: kleiner_8,
        /// Der konfigurierte Interrupt-Pin des Pcf8574.
        interrupt: Option<u8>,
    },
}

impl InputSerialisiert {
    /// Handelt es sich um den selben Anschluss, unabhängig vom Interrupt Pin.
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

    /// Der Interrupt-Pin eines [Pcf8574-Port](pcf8574::Port),
    /// sofern es sich um einen handelt und einer konfiguriert ist.
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
                let beschreibung = *port.beschreibung();
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

/// Fehler, die beim reservieren eines [Anschluss]es auftreten können.
#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum ReservierenFehler {
    /// Ein Fehler beim reservieren eines [Pin]s.
    Pin(pin::ReservierenFehler),
    /// Ein [Pcf8574-Port](pcf8574::Port) wird bereits verwendet.
    Pcf8574(pcf8574::InVerwendung),
}

/// Fehler, die bei Interaktion mit einem [Anschluss] auftreten können.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum Fehler {
    /// Ein Fehler mit einem [Input-Pin](input::Pin).
    Input(input::Fehler),
    /// Ein Fehler mit einem [Pcf8574-Port](pcf8574::Port).
    Pcf8574(pcf8574::Fehler),
    /// Ein Fehler beim Reservieren eines [Anschluss]es.
    Reservieren(ReservierenFehler),
    /// Der Name des Leiters stimmt nicht überein.
    FalscherLeiter(FalscherLeiter),
    /// Unbekannter Zugtyp beim Laden von v2-Speicherdaten.
    UnbekannterZugtyp {
        /// Der gespeicherte Zugtyp.
        zugtyp: String,
        /// Der Name des aktuellen Leiters.
        leiter: &'static str,
    },
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
