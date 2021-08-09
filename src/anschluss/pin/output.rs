//! Gpio Pins konfiguriert für Output.

#[cfg(not(raspi))]
use log::debug;
#[cfg(raspi)]
use rppal::gpio;

#[cfg(not(raspi))]
use super::Wrapper;
use crate::anschluss::level::Level;

/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug, PartialEq)]
pub struct Pin(#[cfg(raspi)] pub(super) gpio::OutputPin, #[cfg(not(raspi))] pub(super) Wrapper);

impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        #[cfg(raspi)]
        {
            self.0.pin()
        }
        #[cfg(not(raspi))]
        {
            // Pins sollten nur auf einem Raspi erzeugbar sein!
            // Liefere Standard-Wert, der in näherer Zukunft nicht von Pins erreicht wird
            self.0 .0
        }
    }

    /// Sets the pin’s output state.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline(always)]
    pub fn write(&mut self, level: Level) -> Result<(), Error> {
        #[cfg(raspi)]
        {
            self.0.write(level.into());
            Ok(())
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.write({:?})", self, level);
            Err(Error::KeinRaspberryPi)
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::Low`.
    #[inline(always)]
    pub fn is_set_low(&self) -> Result<bool, Error> {
        #[cfg(raspi)]
        {
            Ok(self.0.is_set_low())
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.is_set_low()", self);
            Err(Error::KeinRaspberryPi)
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::High`.
    #[inline(always)]
    pub fn is_set_high(&self) -> Result<bool, Error> {
        #[cfg(raspi)]
        {
            Ok(self.0.is_set_high())
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.is_set_high()", self);
            Err(Error::KeinRaspberryPi)
        }
    }

    /// Toggles the pin’s output state between Low and High.
    #[inline(always)]
    pub fn toggle(&mut self) -> Result<(), Error> {
        #[cfg(raspi)]
        {
            self.0.toggle();
            Ok(())
        }
        #[cfg(not(raspi))]
        {
            debug!("{:?}.toggle()", self);
            Err(Error::KeinRaspberryPi)
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
