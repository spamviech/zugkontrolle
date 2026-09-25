//! [`Trigger`] für auslösen eines Interrupt-Events.

use std::{
    cmp::Ordering,
    fmt::{Display, Formatter, Result},
    ops::{BitAnd, BitOr},
};

use serde::{Deserialize, Serialize};

use crate::{level::Level, rpi_pal::gpio};

/// [`Trigger`] für auslösen eines Interrupt-Events.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Trigger {
    /// Deaktiviert, es wird kein Event ausgelöst.
    Disabled = 0,
    /// Löse ein Event aus, wenn die Spannung von [Low](Level::Low) auf [`High`](Level::High) wechselt.
    #[default]
    RisingEdge = 1,
    /// Löse ein Event aus, wenn die Spannung von [High](Level::High) auf [`Low`](Level::Low) wechselt.
    FallingEdge = 2,
    /// Löse ein Event bei jeder Veränderung aus.
    Both = 3,
}

impl BitAnd for Trigger {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Trigger::Disabled, _)
            | (_, Trigger::Disabled)
            | (Trigger::RisingEdge, Trigger::FallingEdge)
            | (Trigger::FallingEdge, Trigger::RisingEdge) => Trigger::Disabled,
            (Trigger::Both, rhs) => rhs,
            (lhs, Trigger::Both) => lhs,
            (Trigger::RisingEdge, Trigger::RisingEdge) => Trigger::RisingEdge,
            (Trigger::FallingEdge, Trigger::FallingEdge) => Trigger::FallingEdge,
        }
    }
}

impl BitOr for Trigger {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Trigger::Disabled, rhs) => rhs,
            (lhs, Trigger::Disabled) => lhs,
            (Trigger::Both, _)
            | (_, Trigger::Both)
            | (Trigger::RisingEdge, Trigger::FallingEdge)
            | (Trigger::FallingEdge, Trigger::RisingEdge) => Trigger::Both,
            (Trigger::RisingEdge, Trigger::RisingEdge) => Trigger::RisingEdge,
            (Trigger::FallingEdge, Trigger::FallingEdge) => Trigger::FallingEdge,
        }
    }
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

impl From<gpio::Trigger> for Trigger {
    fn from(trigger: gpio::Trigger) -> Trigger {
        match trigger {
            gpio::Trigger::Disabled => Trigger::Disabled,
            gpio::Trigger::RisingEdge => Trigger::RisingEdge,
            gpio::Trigger::FallingEdge => Trigger::FallingEdge,
            gpio::Trigger::Both => Trigger::Both,
        }
    }
}

impl From<Trigger> for gpio::Trigger {
    fn from(trigger: Trigger) -> gpio::Trigger {
        match trigger {
            Trigger::Disabled => gpio::Trigger::Disabled,
            Trigger::RisingEdge => gpio::Trigger::RisingEdge,
            Trigger::FallingEdge => gpio::Trigger::FallingEdge,
            Trigger::Both => gpio::Trigger::Both,
        }
    }
}

impl Trigger {
    /// Ist die konfigurierte Trigger-Bedingung aufgetreten?
    /// Gibt nur [`Trigger::RisingEdge`] oder [`Trigger::FallingEdge`] als [`Some`]-Werte zurück,
    /// sofern der Callback aufgerufen werden soll.
    #[must_use]
    pub fn callback_aufrufen(self, aktuell: Level, bisher: Level) -> Option<Trigger> {
        let event = match aktuell.cmp(&bisher) {
            Ordering::Less => Trigger::RisingEdge,
            Ordering::Greater => Trigger::FallingEdge,
            Ordering::Equal => return None,
        };
        let kombiniert = self & event;
        (kombiniert != Trigger::Disabled).then_some(kombiniert)
    }

    /// Gebe das [`Level`] nach auftreten der [`Trigger`]-Bedingung zurück.
    /// Gebe [`None`] für [`Trigger::Disabled`] und [`Trigger::Both`] zurück.
    #[must_use]
    pub fn neues_level(self) -> Option<Level> {
        match self {
            Trigger::RisingEdge => Some(Level::High),
            Trigger::FallingEdge => Some(Level::Low),
            Trigger::Disabled | Trigger::Both => None,
        }
    }
}
