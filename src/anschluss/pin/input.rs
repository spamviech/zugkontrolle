//! Gpio Pins konfiguriert für Input.

use cfg_if::cfg_if;
#[cfg(not(raspi))]
use log::debug;
#[cfg(raspi)]
use rppal::gpio;

#[cfg(not(raspi))]
use super::Wrapper;
use crate::anschluss::{level::Level, trigger::Trigger};

/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug, PartialEq)]
pub struct Pin(#[cfg(raspi)] pub(super) gpio::InputPin, #[cfg(not(raspi))] pub(super) Wrapper);

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
                self.0.0
            }
        }
    }

    /// Reads the pin’s logic level.
    #[inline]
    pub fn read(&mut self) -> Result<Level, Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(self.0.read().into())
            } else {
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    // sync interrupt nicht implementiert, da global nur einer existieren kann
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts
    // "Calling poll_interrupts blocks any other calls to poll_interrupts or
    // InputPin::poll_interrupt until it returns. If you need to poll multiple pins simultaneously
    // on different threads, consider using asynchronous interrupts with
    // InputPin::set_async_interrupt instead."

    /// Configures an asynchronous interrupt trigger, which executes the callback on a separate
    /// thread when the interrupt is triggered.
    ///
    /// The callback closure or function pointer is called with a single Level argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared when
    /// set_async_interrupt is called, or when InputPin goes out of scope.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline]
    pub fn set_async_interrupt(
        &mut self,
        trigger: Trigger,
        callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(self.0.set_async_interrupt(trigger, callback)?)
            } else {
                debug!("{:?}.set_async_interrupt({}, callback)", self, trigger);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    #[inline]
    pub fn clear_async_interrupt(&mut self) -> Result<(), Error> {
        cfg_if! {
            if #[cfg(raspi)] {
                Ok(self.0.clear_async_interrupt()?)
            } else {
                debug!("{:?}.clear_async_interrupt()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(gpio::Error),
    #[cfg(not(raspi))]
    KeinRaspberryPi,
}
#[cfg(raspi)]
impl From<gpio::Error> for Error {
    fn from(error: gpio::Error) -> Self {
        Error::Gpio(error)
    }
}
