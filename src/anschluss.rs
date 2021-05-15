//! Mit Raspberry Pi schaltbarer Anschluss

use std::ops::Not;

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
    Gpio(Gpio),
    PCF8574Port(PCF8574Port),
}

/// Ein Gpio Pin.
#[derive(Debug)]
pub struct Gpio(u8);

/// Ein Port eines PCF8574, gesteuert über I2C.
#[derive(Debug)]
pub struct PCF8574Port {
    a0: Level,
    a1: Level,
    a2: Level,
    variante: PCF8574Variante,
    port: u8,
}

#[derive(Debug)]
pub enum PCF8574Variante {
    Normal,
    A,
}

#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Low,
    High,
}
#[cfg(not(raspi))]
impl Not for Level {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Level::Low => Level::High,
            Level::High => Level::Low,
        }
    }
}

#[cfg(raspi)]
pub use rppal::gpio::Level;

#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(rppal::gpio::Error),
    #[cfg(raspi)]
    I2c(rppal::i2c::Error),
    #[cfg(raspi)]
    Pwm(rppal::pwm::Error),
}
#[cfg(raspi)]
impl From<rppal::gpio::Error> for Error {
    fn from(error: rppal::gpio::Error) -> Self {
        Error::Gpio(error)
    }
}
#[cfg(raspi)]
impl From<rppal::i2c::Error> for Error {
    fn from(error: rppal::i2c::Error) -> Self {
        Error::I2c(error)
    }
}
#[cfg(raspi)]
impl From<rppal::pwm::Error> for Error {
    fn from(error: rppal::pwm::Error) -> Self {
        Error::Pwm(error)
    }
}
