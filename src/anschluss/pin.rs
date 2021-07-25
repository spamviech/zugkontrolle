//! Gpio Pin in verschiedenen Konfigurationen.

#[cfg(not(raspi))]
use std::sync::mpsc::Sender;
pub use std::time::Duration;

#[cfg(raspi)]
use rppal::{self, gpio};

use super::level::Level;

pub mod input;
pub mod output;
pub mod pwm;
#[cfg(raspi)]
use pwm::Pwm;

#[cfg(not(raspi))]
#[derive(Debug)]
struct Wrapper(u8, Sender<u8>);

#[cfg(not(raspi))]
impl PartialEq for Wrapper {
    fn eq(&self, other: &Wrapper) -> bool {
        self.0 == other.0
    }
}

#[cfg(not(raspi))]
impl Drop for Wrapper {
    fn drop(&mut self) {
        let _ = self.1.send(self.0);
    }
}

/// Ein Gpio Pin.
#[derive(Debug, PartialEq)]
pub struct Pin(#[cfg(raspi)] gpio::Pin, #[cfg(not(raspi))] Wrapper);
impl Pin {
    #[cfg(raspi)]
    pub(super) fn neu(pin: gpio::Pin) -> Self {
        Pin(pin)
    }

    #[cfg(not(raspi))]
    pub(super) fn neu(pin: u8, sender: Sender<u8>) -> Self {
        Pin(Wrapper(pin, sender))
    }

    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline]
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
    #[cfg_attr(not(raspi), allow(unused_variables))]
    pub fn into_output(self, level: Level) -> output::Pin {
        output::Pin(
            #[cfg(raspi)]
            {
                let mut output_pin = self.0.into_output();
                output_pin.write(level.into());
                output_pin
            },
            #[cfg(not(raspi))]
            self.0,
        )
    }

    #[cfg(raspi)]
    #[inline]
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
        #[cfg(raspi)]
        {
            if let Some(pwm) =
                self.pwm_channel().and_then(|channel| rppal::pwm::Pwm::new(channel).ok())
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
        #[cfg(not(raspi))]
        {
            pwm::Pin { pin: self.0, config: None }
        }
    }
}
