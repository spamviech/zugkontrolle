//! Level eines Anschluss

#[cfg(raspi)]
use rppal::gpio;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Level {
    Low,
    High,
}
#[cfg(raspi)]
impl From<gpio::Level> for Level {
    fn from(level: gpio::Level) -> Self {
        match level {
            gpio::Level::Low => Level::Low,
            gpio::Level::High => Level::High,
        }
    }
}
#[cfg(raspi)]
impl From<Level> for gpio::Level {
    fn from(level: gpio::Level) -> Self {
        match level {
            Level::Low => gpio::Level::Low,
            Level::High => gpio::Level::High,
        }
    }
}
