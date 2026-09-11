//! Interrupt trigger event.

use std::time::Duration;

use crate::{rppal::gpio, trigger::Trigger};

/// Interrupt trigger event.
#[derive(Debug, Copy, Clone)]
pub struct Event {
    /// Best estimate of time of event occurrence, measured in elapsed time since the system was booted.
    pub timestamp: Duration,
    /// Sequence number for this event in the sequence of interrupt trigger events for this pin.
    pub seqno: u32,
    /// Interrupt trigger. This will contain either [Trigger::RisingEdge] or [Trigger::FallingEdge].
    pub trigger: Trigger,
}

impl From<gpio::Event> for Event {
    fn from(value: gpio::Event) -> Self {
        Event { timestamp: value.timestamp, seqno: value.seqno, trigger: value.trigger.into() }
    }
}

impl From<Event> for gpio::Event {
    fn from(value: Event) -> Self {
        gpio::Event {
            timestamp: value.timestamp,
            seqno: value.seqno,
            trigger: value.trigger.into(),
        }
    }
}

impl Default for Event {
    fn default() -> Self {
        Self { timestamp: Duration::default(), seqno: 0, trigger: Trigger::Both }
    }
}
