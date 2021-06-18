//! Trigger für auslösen eines Interrupt-Events

use std::fmt::{Display, Formatter, Result};

use serde::{Deserialize, Serialize};

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

#[cfg(raspi)]
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
