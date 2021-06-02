//! Level eines Anschluss

#[cfg(not(raspi))]
use std::fmt::{Display, Formatter, Result};

use serde::{Deserialize, Serialize};

use super::level::Level;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Polarität {
    Normal,
    Invertiert,
}
impl Display for Polarität {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Polarität::Normal => "Normal",
            Polarität::Invertiert => "Invertiert",
        })
    }
}

#[cfg(raspi)]
impl From<Polarität> for rppal::pwm::Polarity {
    fn from(polarität: Polarität) -> Self {
        match polarität {
            Polarität::Normal => Polarity::Normal,
            Polarität::Invertiert => Polarity::Inverse,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fließend {
    Fließend,
    Gesperrt,
}
impl Fließend {
    pub fn with_polarity(self, polarity: Polarität) -> Level {
        match (self, polarity) {
            (Fließend::Fließend, Polarität::Normal)
            | (Fließend::Gesperrt, Polarität::Invertiert) => Level::High,
            (Fließend::Fließend, Polarität::Invertiert)
            | (Fließend::Gesperrt, Polarität::Normal) => Level::Low,
        }
    }
}
