//! Gpio Pins konfiguriert für Input.

use crate::{
    anschluss::{level::Level, trigger::Trigger},
    rppal::gpio,
};

/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug, PartialEq)]
pub struct Pin(pub(super) gpio::InputPin);

impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Reads the pin’s logic level.
    #[inline(always)]
    pub fn read(&mut self) -> Result<Level, Fehler> {
        Ok(self.0.read().into())
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
    #[inline(always)]
    pub fn set_async_interrupt(
        &mut self,
        trigger: Trigger,
        #[cfg_attr(not(raspi), allow(unused_mut))] mut callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Fehler> {
        let pin = self.pin();
        Ok(self
            .0
            .set_async_interrupt(trigger.into(), move |level| callback(level.into()))
            .map_err(|fehler| Fehler::Gpio { pin, fehler })?)
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    #[inline(always)]
    pub fn clear_async_interrupt(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        Ok(self.0.clear_async_interrupt().map_err(|fehler| Fehler::Gpio { pin, fehler })?)
    }
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub enum Fehler {
    Gpio { pin: u8, fehler: gpio::Error },
}
