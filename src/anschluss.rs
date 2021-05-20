//! Mit Raspberry Pi schaltbarer Anschluss

pub mod level;
pub use level::*;

pub mod polarity;
pub use polarity::*;

pub mod trigger;
pub use trigger::*;

pub mod pin;
pub use pin::*;

pub mod pcf8574;
pub use pcf8574::{InterruptPcf8574, Pcf8574, Port};

// path attribute necessary due to non-ascii module name (at least for now)
#[path = "anschluss/anschlüsse.rs"]
pub mod anschlüsse;
pub use anschlüsse::*;

/// Ein Anschluss
#[derive(Debug)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(Port<Pcf8574>),
}
/// Ein Anschluss, konfiguriert für Output.
#[derive(Debug)]
pub enum OutputAnschluss {
    Pin(output::Pin),
    Pcf8574Port(Port<Pcf8574>),
    InterruptPcf8574Port(Port<InterruptPcf8574>),
}
/// Ein Anschluss, konfiguriert für Input.
#[derive(Debug)]
pub enum InputAnschluss {
    Pin(input::Pin),
    Pcf8574Port(Port<InterruptPcf8574>),
}

#[cfg(test)]
mod test;
