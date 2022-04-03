//! Trigger für auslösen eines Interrupt-Events.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

use std::fmt::{Display, Formatter, Result};

use serde::{Deserialize, Serialize};

use crate::{anschluss::level::Level, rppal};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Trigger {
    Disabled,
    RisingEdge,
    FallingEdge,
    Both,
}

impl Display for Trigger {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
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
    pub(in crate::anschluss) fn callback_aufrufen(&self, aktuell: Level, bisher: Level) -> bool {
        match self {
            Trigger::Both => aktuell != bisher,
            Trigger::FallingEdge => aktuell < bisher,
            Trigger::RisingEdge => aktuell > bisher,
            Trigger::Disabled => false,
        }
    }
}
