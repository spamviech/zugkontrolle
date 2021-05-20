//! Gpio Pins konfiguriert für Output.

use cfg_if::cfg_if;
use log::debug;

use crate::anschluss::level::Level;

/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug, PartialEq)]
pub struct Pin(#[cfg(raspi)] pub(super) gpio::Pin, #[cfg(not(raspi))] pub(super) u8);

impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline]
    pub fn pin(&self) -> u8 {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.pin()
            } else {
                // Pins sollten nur auf einem Raspi erzeugbar sein!
                // Liefere Standard-Wert, der in näherer Zukunft nicht von Pins erreicht wird
                self.0
            }
        }
    }

    /// Sets the pin’s output state.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline]
    pub fn write(&mut self, level: Level) {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.write(level)
            } else {
                debug!("{:?}.write({:?})", self, level);
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::Low`.
    #[inline]
    pub fn is_set_low(&self) -> bool {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.is_set_low()
            } else {
                debug!("{:?}.is_set_low()", self);
                false
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::High`.
    #[inline]
    pub fn is_set_high(&self) -> bool {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.is_set_high()
            } else {
                debug!("{:?}.is_set_high()", self);
                false
            }
        }
    }

    // maybe re-export more methods?
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Pin.html
}
