//! Level eines [`Anschlusses`](crate::anschluss::Anschluss).

use enum_iterator::Sequence;
use serde::{Deserialize, Serialize};

use crate::rppal::gpio;

/// Level eines [`Anschlusses`](crate::anschluss::Anschluss).
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Sequence,
    Serialize,
    Deserialize,
)]
pub enum Level {
    /// Tiefes Volt-Level.
    #[default]
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
