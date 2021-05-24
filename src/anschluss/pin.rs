//! Gpio Pin in verschiedenen Konfigurationen.

pub use std::time::Duration;

use cfg_if::cfg_if;
#[cfg(raspi)]
use rppal::{gpio, pwm};

pub mod input;
pub mod output;
pub mod pwm;

/// Ein Gpio Pin.
#[derive(Debug, PartialEq)]
pub struct Pin(#[cfg(raspi)] gpio::Pin, #[cfg(not(raspi))] u8);
impl Pin {
    pub(super) fn neu(#[cfg(raspi)] pin: gpio::Pin, #[cfg(not(raspi))] pin: u8) -> Self {
        Pin(pin)
    }

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

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and disables the pin’s
    /// built-in pull-up/pull-down resistors.
    #[inline]
    pub fn into_input(self) -> input::Pin {
        input::Pin(
            #[cfg(raspi)]
            self.0.into_input(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and enables the pin’s
    /// built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when input::Pin goes out of scope if reset_on_drop is set
    /// to true (default).
    #[inline]
    pub fn into_input_pulldown(self) -> input::Pin {
        input::Pin(
            #[cfg(raspi)]
            self.0.into_input_pulldown(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and enables the pin’s
    /// built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when input::Pin goes out of scope if reset_on_drop is set
    /// to true (default).
    #[inline]
    pub fn into_input_pullup(self) -> input::Pin {
        input::Pin(
            #[cfg(raspi)]
            self.0.into_input_pullup(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    ///Consumes the Pin, returns an output::Pin and sets its mode to Output.
    #[inline]
    pub fn into_output(self) -> output::Pin {
        output::Pin(
            #[cfg(raspi)]
            self.0.into_output(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    #[cfg(raspi)]
    #[inline]
    fn pwm_channel(&self) -> Option<pwm::Channel> {
        match self.0.pin() {
            18 => Some(pwm::Channel::Pwm0),
            19 => Some(pwm::Channel::Pwm1),
            _ => None,
        }
    }

    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[cfg_attr(not(raspi), inline)]
    pub fn into_pwm(self) -> pwm::Pin {
        cfg_if! {
            if #[cfg(raspi)]
            {
                if let Some(pwm) = self.pwm_channel().and_then(|channel| pwm::Pwm::new().ok()) {
                    let config = pwm.polarity().and_then(|polarity|
                                    pwm.period().and_then(|period|
                                    pwm.pulse_width().and_then(|pulse_width|
                                    PwmConfig {time: PwmTime::Period {period, pulse_width}, polarity}))
                                ).ok();
                    pwm::Pin {pin: Pwm::Hardware(pwm, pin: self.0), config }
                } else {
                    // fallback software pwm
                    pwm::Pin {pin: Pwm::Software(self.0.into_output()), config: None }
                }
            } else {
                pwm::Pin {pin: self.0, config:None}
            }
        }
    }
}
