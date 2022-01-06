//! Mock-Methoden oder re-export für rppal.

pub mod gpio;
pub mod i2c;
pub mod pwm;

#[cfg(raspi)]
// Wird auf nicht-Raspi-Systemen für mock-Implementierung verwendet.
use once_cell as _;
