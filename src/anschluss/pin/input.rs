//! Gpio [Pins](Pin) konfiguriert für Input.

use crate::{
    anschluss::{level::Level, trigger::Trigger},
    rppal::gpio,
};

/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug, PartialEq)]
pub struct Pin(pub(super) gpio::InputPin);

impl Pin {
    /// Erhalte die GPIO [Pin] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    // FIXME Unnötiges Result<_, Fehler> entfernen. Lesen ist immer erfolgreich!
    /// Lese das aktuell am [Pin] anliegende [Level].
    #[allow(clippy::missing_errors_doc)]
    pub fn lese(&mut self) -> Result<Level, Fehler> {
        Ok(self.0.read().into())
    }

    /// Konfiguriere einen asynchronen Interrupt Trigger.
    /// Bei auftreten wird der callback in einem separaten Thread ausgeführt.
    ///
    /// Alle vorher konfigurierten Interrupt Trigger werden gelöscht, sobald
    /// [`setze_async_interrupt`](Pin::setze_async_interrupt) oder
    /// [`lösche_async_interrupt`](Pin::lösche_async_interrupt) aufgerufen wird,
    /// oder der [`input::Pin`](Pin) out of scope geht.
    ///
    /// ## Errors
    ///
    /// Setzten des Interrupts schlug fehl.
    ///
    /// ## Keine synchronen Interrupts
    ///
    /// Obwohl rppal prinzipiell synchrone Interrupts unterstützt sind die Einschränkungen zu groß.
    /// Siehe die Dokumentation der
    /// [poll_interrupts](https://docs.rs/rppal/0.12.0/rppal/gpio/struct.Gpio.html#method.poll_interrupts)
    /// Methode.
    /// > Calling `poll_interrupts` blocks any other calls to `poll_interrupts` or
    /// > `InputPin::poll_interrupt` until it returns. If you need to poll multiple pins simultaneously
    /// > on different threads, consider using asynchronous interrupts with
    /// > `InputPin::set_async_interrupt` instead.
    pub fn setze_async_interrupt(
        &mut self,
        trigger: Trigger,
        mut callback: impl FnMut(Level) + Send + 'static,
    ) -> Result<(), Fehler> {
        let pin = self.pin();
        self.0
            .set_async_interrupt(trigger.into(), move |level| callback(level.into()))
            .map_err(|fehler| Fehler { pin, fehler })?;
        Ok(())
    }

    /// Entferne einen vorher konfigurierten asynchronen Interrupt Trigger.
    ///
    /// ## Errors
    ///
    /// Löschen des async interrupts schlug fehl.
    pub fn lösche_async_interrupt(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        self.0.clear_async_interrupt().map_err(|fehler| Fehler { pin, fehler })
    }
}

/// Ein Fehler bei Interaktion mit einem Input-[Pin].
#[derive(Debug)]
pub struct Fehler {
    /// Der betroffene Pin.
    pub pin: u8,
    /// Der aufgetretene Fehler.
    pub fehler: gpio::Error,
}
