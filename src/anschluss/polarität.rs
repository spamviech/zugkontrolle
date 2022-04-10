//! Level eines [Anschlusses](crate::anschluss::Anschluss).

use std::fmt::{Display, Formatter, Result};
use std::ops::Not;

use serde::{Deserialize, Serialize};

use crate::{anschluss::level::Level, rppal};

/// Bei welchem [Level] fließt der Strom an einem Anschluss.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Polarität {
    /// [High](Level::High) ist [Fließend](Fließend::Fließend),
    /// [Low](Level::Low) ist [Gesperrt](Fließend::Gesperrt).
    Normal,
    /// [Low](Level::Low) ist [Fließend](Fließend::Fließend),
    /// [High](Level::High) ist [Gesperrt](Fließend::Gesperrt).
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

/// Zustand des Stroms, der von einem [Anschluss](crate::anschluss::Anschluss) gesteuert wird.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Fließend {
    /// Der Strom fließt.
    Fließend,
    /// Der Strom fließt nicht.
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
    /// Erhalte den zur [Polarität] passenden [Level].
    pub fn mit_polarität(self, polarität: Polarität) -> Level {
        match (self, polarität) {
            (Fließend::Fließend, Polarität::Normal)
            | (Fließend::Gesperrt, Polarität::Invertiert) => Level::High,
            (Fließend::Fließend, Polarität::Invertiert)
            | (Fließend::Gesperrt, Polarität::Normal) => Level::Low,
        }
    }
}
