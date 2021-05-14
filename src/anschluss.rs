//! Mit Raspberry Pi schaltbarer Anschluss

use std::ops::Not;

/// Singleton für Zugriff auf raspberry pi Anschlüsse.
#[derive(Debug)]
pub struct Anschlüsse {
    #[cfg(raspi)]
    gpio: rppal::GPio,
    #[cfg(raspi)]
    i2c: rppal::I2C,
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
