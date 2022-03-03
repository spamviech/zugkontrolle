//! Gpio Pins konfiguriert für Output.

use crate::{anschluss::level::Level, rppal::gpio};

/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug, PartialEq)]
pub struct Pin(pub(super) gpio::OutputPin);

impl Pin {
    /// Erhalte die GPIO pin number.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Sets the pin’s output state.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline(always)]
    pub fn write(&mut self, level: Level) {
        self.0.write(level.into());
    }

    /// Returns `true` if the pin's output state is set to `Level::Low`.
    #[inline(always)]
    pub fn is_set_low(&self) -> bool {
        self.0.is_set_low()
    }

    /// Returns `true` if the pin's output state is set to `Level::High`.
    #[inline(always)]
    pub fn is_set_high(&self) -> bool {
        self.0.is_set_high()
    }

    /// Toggles the pin’s output state between Low and High.
    #[inline(always)]
    pub fn toggle(&mut self) {
        self.0.toggle();
    }

    // maybe re-export more methods?
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Pin.html
}
