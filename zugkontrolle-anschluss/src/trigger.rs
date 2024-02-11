//! [`Trigger`] für auslösen eines Interrupt-Events.

use std::fmt::{Display, Formatter, Result};

use serde::{Deserialize, Serialize};

use crate::{level::Level, rppal};

/// [`Trigger`] für auslösen eines Interrupt-Events.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Trigger {
    /// Deaktiviert, es wird kein Event ausgelöst.
    Disabled,
    /// Löse ein Event aus, wenn die Spannung von [Low](Level::Low) auf [`High`](Level::High) wechselt.
    RisingEdge,
    /// Löse ein Event aus, wenn die Spannung von [High](Level::High) auf [`Low`](Level::Low) wechselt.
    FallingEdge,
    /// Löse ein Event bei jeder Veränderung aus.
    Both,
}

impl Display for Trigger {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(
            formatter,
            "{}",
            match self {
                Trigger::Disabled => "Disabled",
                Trigger::RisingEdge => "Rising Edge",
                Trigger::FallingEdge => "Falling Edge",
                Trigger::Both => "Both",
            }
        )
    }
}

impl From<Trigger> for rppal::gpio::Trigger {
    fn from(trigger: Trigger) -> rppal::gpio::Trigger {
        match trigger {
            Trigger::Disabled => rppal::gpio::Trigger::Disabled,
            Trigger::RisingEdge => rppal::gpio::Trigger::RisingEdge,
            Trigger::FallingEdge => rppal::gpio::Trigger::FallingEdge,
            Trigger::Both => rppal::gpio::Trigger::Both,
        }
    }
}

impl Trigger {
    /// Ist die konfigurierte Trigger-Bedingung aufgetreten?
    pub(crate) fn callback_aufrufen(self, aktuell: Level, bisher: Level) -> bool {
        match self {
            Trigger::Both => aktuell != bisher,
            Trigger::FallingEdge => aktuell < bisher,
            Trigger::RisingEdge => aktuell > bisher,
            Trigger::Disabled => false,
        }
    }
}
