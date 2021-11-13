//! Gpio Pin in verschiedenen Konfigurationen.

pub use std::time::Duration;

use self::pwm::Pwm;
use crate::{anschluss::level::Level, rppal};

pub mod input;
pub mod output;
pub mod pwm;

/// Ein Gpio Pin.
#[derive(Debug, PartialEq)]
pub struct Pin(rppal::gpio::Pin);
impl Pin {
    pub(super) fn neu(pin: rppal::gpio::Pin) -> Self {
        Pin(pin)
    }

    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and disables the pin’s
    /// built-in pull-up/pull-down resistors.
    #[inline(always)]
    pub fn into_input(self) -> input::Pin {
        input::Pin(self.0.into_input())
    }

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and enables the pin’s
    /// built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when input::Pin goes out of scope if reset_on_drop is set
    /// to true (default).
    #[inline(always)]
    pub fn into_input_pulldown(self) -> input::Pin {
        input::Pin(self.0.into_input_pulldown())
    }

    /// Consumes the Pin, returns an input::Pin, sets its mode to Input, and enables the pin’s
    /// built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when input::Pin goes out of scope if reset_on_drop is set
    /// to true (default).
    #[inline(always)]
    pub fn into_input_pullup(self) -> input::Pin {
        input::Pin(self.0.into_input_pullup())
    }

    ///Consumes the Pin, returns an output::Pin and sets its mode to Output.
    #[inline(always)]
    #[cfg_attr(not(raspi), allow(unused_variables))]
    pub fn into_output(self, level: Level) -> output::Pin {
        output::Pin({
            let mut output_pin = self.0.into_output();
            output_pin.write(level.into());
            output_pin
        })
    }

    #[inline(always)]
    fn pwm_channel(&self) -> Option<rppal::pwm::Channel> {
        match self.0.pin() {
            18 => Some(rppal::pwm::Channel::Pwm0),
            19 => Some(rppal::pwm::Channel::Pwm1),
            _ => None,
        }
    }

    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[cfg_attr(not(raspi), inline)]
    pub fn into_pwm(self) -> pwm::Pin {
        if let Some(pwm) = self.pwm_channel().and_then(|channel| rppal::pwm::Pwm::new(channel).ok())
        {
            let config = pwm
                .polarity()
                .and_then(|polarität| {
                    pwm.period().and_then(|period| {
                        pwm.pulse_width().map(|pulse_width| pwm::Config {
                            time: pwm::Time::Period { period, pulse_width },
                            polarity: polarität.into(),
                        })
                    })
                })
                .ok();
            pwm::Pin { pin: Pwm::Hardware(pwm, self.0), config }
        } else {
            // fallback software pwm
            pwm::Pin { pin: Pwm::Software(self.0.into_output()), config: None }
        }
    }
}
