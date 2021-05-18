//! Gpio Pins konfiguriert für Input.

use cfg_if::cfg_if;

/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug)]
pub struct Pin(#[cfg(raspi)] pub(super) gpio::InputPin, #[cfg(not(raspi))] pub(super) u8);

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

    // TODO cfg-reexport/stub-methods
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.InputPin.html
}
