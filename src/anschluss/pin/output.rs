//! Gpio [Pins](Pin) konfiguriert für Output.

use crate::{anschluss::level::Level, rppal::gpio};

/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug, PartialEq)]
pub struct Pin(pub(super) gpio::OutputPin);

impl Pin {
    /// Erhalte die GPIO [Pin] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Setze den Output [Level] des [Pin].
    pub fn schreibe(&mut self, level: Level) {
        self.0.write(level.into());
    }

    /// Gibt `true` zurück, falls der Output [Level] des [Pin] auf [`Level::Low`] gesetzt ist.
    #[must_use]
    pub fn ist_low(&self) -> bool {
        self.0.is_set_low()
    }

    /// Gibt `true` zurück, falls der Output [Level] des [Pin] auf [`Level::High`] gesetzt ist.
    #[must_use]
    pub fn ist_high(&self) -> bool {
        self.0.is_set_high()
    }

    /// Wechsle den Output [Level] des [Pin]s zwischen [`Level::Low`] und [`Level::High`].
    pub fn umschalten(&mut self) {
        self.0.toggle();
    }

    // maybe re-export more methods?
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Pin.html
}
