//! Gpio Pins konfiguriert für Input.

use crate::{
    anschluss::{level::Level, trigger::Trigger},
    rppal::gpio,
};

/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug, PartialEq)]
pub struct Pin(pub(super) gpio::InputPin);

impl Pin {
    /// Erhalte die GPIO pin number.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Lese das aktuell am [Pin] anliegende [Level].
    #[inline(always)]
    pub fn lese(&mut self) -> Result<Level, Fehler> {
        Ok(self.0.read().into())
    }

    /// Konfiguriere einen asynchronen Interrupt Trigger.
    /// Bei auftreten wird der callback in einem separaten Thread ausgeführt.
    ///
    /// Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald [setze_async_interrupt]
    /// oder [lösche_async_interrupt] aufgerufen wird, oder der [InputPin] out of scope geht.
    ///
    /// ## Keine synchronen Interrupts
    /// Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.
    /// Siehe die Dokumentation der
    /// [poll_interrupts](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)
    /// Methode.
    /// > Calling poll_interrupts blocks any other calls to poll_interrupts or
    /// > InputPin::poll_interrupt until it returns. If you need to poll multiple pins simultaneously
    /// > on different threads, consider using asynchronous interrupts with
    /// > InputPin::set_async_interrupt instead.
    #[inline(always)]
    pub fn setze_async_interrupt(
        &mut self,
        trigger: Trigger,
        mut callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Fehler> {
        let pin = self.pin();
        self.0
            .set_async_interrupt(trigger.into(), move |level| callback(level.into()))
            .map_err(|fehler| Fehler::Gpio { pin, fehler })?;
        Ok(())
    }

    /// Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.
    #[inline(always)]
    pub fn lösche_async_interrupt(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        Ok(self.0.clear_async_interrupt().map_err(|fehler| Fehler::Gpio { pin, fehler })?)
    }
}

#[derive(Debug)]
pub enum Fehler {
    Gpio { pin: u8, fehler: gpio::Error },
}
