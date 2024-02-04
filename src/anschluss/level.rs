//! Level eines [`Anschlusses`](crate::anschluss::Anschluss).

use serde::{Deserialize, Serialize};

use crate::rppal::gpio;

/// Level eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Level {
    /// Tiefes Volt-Level.
    Low,
    /// Hohes Volt-Level.
    High,
}

impl From<gpio::Level> for Level {
    fn from(level: gpio::Level) -> Self {
        match level {
            gpio::Level::Low => Level::Low,
            gpio::Level::High => Level::High,
        }
    }
}

impl From<Level> for gpio::Level {
    fn from(level: Level) -> Self {
        match level {
            Level::Low => gpio::Level::Low,
            Level::High => gpio::Level::High,
        }
    }
}
