//! Mit Raspberry Pi schaltbarer Anschluss

use std::ops::Not;
use std::sync::{Arc, RwLock};

use cfg_if::cfg_if;

/// Singleton für Zugriff auf raspberry pi Anschlüsse.
#[derive(Debug)]
pub struct Anschlüsse {
    #[cfg(raspi)]
    gpio: rppal::gpio::Gpio,
    #[cfg(raspi)]
    i2c: rppal::gpio::I2c,
    #[cfg(raspi)]
    pwm: rppal::pwm::Pwm,
}
impl Anschlüsse {
    pub fn neu() -> Result<Self, Error> {
        Ok(Anschlüsse {
            #[cfg(raspi)]
            gpio: rppal::gpio::GPio::new()?,
            #[cfg(raspi)]
            i2c: rppal::i2c::I2C::new()?,
            #[cfg(raspi)]
            pwm: rppal::pwm::Pwm::new()?,
        })
    }
}

/// Ein Anschluss
#[derive(Debug)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(Pcf8574Port),
}

/// Ein Gpio Pin.
#[derive(Debug)]
pub struct Pin(#[cfg(raspi)] rppal::gpio::Pin);
/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug)]
pub struct InputPin(#[cfg(raspi)] rppal::gpio::InputPin);
/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPin(#[cfg(raspi)] rppal::gpio::OutputPin);
/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug)]
pub struct PwmPin(#[cfg(raspi)] Pwm);
#[cfg(raspi)]
enum Pwm {
    Pwm(rppal::pwm::Pwm),
    Pin(rppal::gpio::Pin),
}

/// Ein PCF8574, gesteuert über I2C.
#[derive(Debug)]
pub struct Pcf8574 {
    a0: Level,
    a1: Level,
    a2: Level,
    variante: Pcf8574Variante,
    wert: u8,
}
impl Pcf8574 {
    pub fn ports(self) -> Pcf8574Ports {
        let arc = Arc::new(RwLock::new(self));
        Pcf8574Ports {
            p0: Pcf8574Port { pcf8574: arc.clone(), port: 0 },
            p1: Pcf8574Port { pcf8574: arc.clone(), port: 1 },
            p2: Pcf8574Port { pcf8574: arc.clone(), port: 2 },
            p3: Pcf8574Port { pcf8574: arc.clone(), port: 3 },
            p4: Pcf8574Port { pcf8574: arc.clone(), port: 4 },
            p5: Pcf8574Port { pcf8574: arc.clone(), port: 5 },
            p6: Pcf8574Port { pcf8574: arc.clone(), port: 6 },
            p7: Pcf8574Port { pcf8574: arc, port: 7 },
        }
    }
}

/// Alle Ports eines PCF8574.
#[derive(Debug)]
pub struct Pcf8574Ports {
    p0: Pcf8574Port,
    p1: Pcf8574Port,
    p2: Pcf8574Port,
    p3: Pcf8574Port,
    p4: Pcf8574Port,
    p5: Pcf8574Port,
    p6: Pcf8574Port,
    p7: Pcf8574Port,
}

/// Ein Port eines PCF8574.
#[derive(Debug)]
pub struct Pcf8574Port {
    pcf8574: Arc<RwLock<Pcf8574>>,
    port: u8,
}

#[derive(Debug)]
pub enum Pcf8574Variante {
    Normal,
    A,
}

cfg_if! {
    if #[cfg(raspi)] {
        pub use rppal::gpio::Level;
    } else {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Level {
            Low,
            High,
        }
        impl Not for Level {
            type Output = Self;

            fn not(self) -> Self::Output {
                match self {
                    Level::Low => Level::High,
                    Level::High => Level::Low,
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
}
cfg_if! {
    if #[cfg(raspi)] {
        impl From<rppal::gpio::Error> for Error {
            fn from(error: rppal::gpio::Error) -> Self {
                Error::Gpio(error)
            }
        }
        impl From<rppal::i2c::Error> for Error {
            fn from(error: rppal::i2c::Error) -> Self {
                Error::I2c(error)
            }
        }
        impl From<rppal::pwm::Error> for Error {
            fn from(error: rppal::pwm::Error) -> Self {
                Error::Pwm(error)
            }
        }
    }
}
