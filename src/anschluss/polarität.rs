//! Level eines [Anschlusses](crate::anschluss::Anschluss).

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::fmt::{Display, Formatter, Result};
use std::ops::Not;

use serde::{Deserialize, Serialize};

use crate::{anschluss::level::Level, rppal};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Polarität {
    Normal,
    Invertiert,
}
impl Display for Polarität {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            match self {
                Polarität::Normal => "Normal",
                Polarität::Invertiert => "Invertiert",
            }
        )
    }
}

impl From<Polarität> for rppal::pwm::Polarity {
    fn from(polarität: Polarität) -> Self {
        match polarität {
            Polarität::Normal => rppal::pwm::Polarity::Normal,
            Polarität::Invertiert => rppal::pwm::Polarity::Inverse,
        }
    }
}

impl From<rppal::pwm::Polarity> for Polarität {
    fn from(polarity: rppal::pwm::Polarity) -> Self {
        match polarity {
            rppal::pwm::Polarity::Normal => Polarität::Normal,
            rppal::pwm::Polarity::Inverse => Polarität::Invertiert,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Fließend {
    Fließend,
    Gesperrt,
}
impl Not for Fließend {
    type Output = Fließend;

    fn not(self) -> Self::Output {
        match self {
            Fließend::Fließend => Fließend::Gesperrt,
            Fließend::Gesperrt => Fließend::Fließend,
        }
    }
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
