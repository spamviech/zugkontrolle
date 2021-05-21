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
    pub fn write(&mut self, level: Level) -> Result<(), Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.write(level);
                Ok(())
            } else {
                debug!("{:?}.write({:?})", self, level);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::Low`.
    #[inline]
    pub fn is_set_low(&self) -> Result<bool, Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(self.0.is_set_low())
            } else {
                debug!("{:?}.is_set_low()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::High`.
    #[inline]
    pub fn is_set_high(&self) -> Result<bool, Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(self.0.is_set_high())
            } else {
                debug!("{:?}.is_set_high()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Toggles the pin’s output state between Low and High.
    #[inline]
    pub fn toggle(&mut self) -> Result<(), Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.toggle()
                Ok(())
            } else {
                debug!("{:?}.toggle()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    // maybe re-export more methods?
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Pin.html
}

#[derive(Debug)]
pub enum Error {
    #[cfg(not(raspi))]
    KeinRaspberryPi,
}
