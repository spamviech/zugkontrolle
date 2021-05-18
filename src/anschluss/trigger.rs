//! Trigger für auslösen eines Interrupt-Events

#[cfg(not(raspi))]
use std::fmt::{Display, Formatter, Result};

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(raspi)] {
        pub use rppal::gpio::Trigger;
    } else {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Trigger {
            Disabled,
            RisingEdge,
            FallingEdge,
            Both,
        }
        impl Display for Trigger {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                write!(f, "{}", match self {
                    Trigger::Disabled => "Disabled",
                    Trigger::RisingEdge => "Rising Edge",
                    Trigger::FallingEdge => "Falling Edge",
                    Trigger::Both => "Both",
                })
            }
        }
    }
}
