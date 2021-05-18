//! Mit Raspberry Pi schaltbarer Anschluss

// path attribute necessary due to non-ascii module name (at least for now)
#[path = "anschluss/anschlüsse.rs"]
pub mod anschlüsse;
pub use anschlüsse::*;

pub mod pin;
pub use pin::*;

pub mod level;
pub use level::*;

pub mod polarity;
pub use polarity::*;

pub mod pcf8574;
pub use pcf8574::{InputPcf8574, OutputPcf8574, Pcf8574, Port, Ports};

/// Ein Anschluss
#[derive(Debug)]
pub enum Anschluss {
    Pin(Pin),
    Pcf8574Port(Port<Pcf8574>),
}
/// Ein Anschluss, konfiguriert für Output
#[derive(Debug)]
pub enum OutputAnschluss {
    Pin(output::Pin),
    Pcf8574Port(Port<OutputPcf8574>),
}
/// Ein Anschluss, konfiguriert für Input
#[derive(Debug)]
pub enum InputAnschluss {
    Pin(input::Pin),
    Pcf8574Port(Port<InputPcf8574>),
}

#[cfg(test)]
mod test;
