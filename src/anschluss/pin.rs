//! Gpio Pin in verschiedenen Konfigurationen.

pub use std::time::Duration;

use cfg_if::cfg_if;
use log::debug;
#[cfg(raspi)]
use rppal::{gpio, pwm};

use super::level::Level;
use super::polarity::Polarity;

/// Ein Gpio Pin.
#[derive(Debug)]
pub struct Pin(#[cfg(raspi)] gpio::Pin, #[cfg(not(raspi))] u8);
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

    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and disables the pin’s
    /// built-in pull-up/pull-down resistors.
    #[inline]
    pub fn into_input(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and enables the pin’s
    /// built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when InputPin goes out of scope if reset_on_drop is set
    /// to true (default).
    #[inline]
    pub fn into_input_pulldown(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input_pulldown(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and enables the pin’s
    /// built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when InputPin goes out of scope if reset_on_drop is set to
    /// true (default).
    #[inline]
    pub fn into_input_pullup(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input_pullup(),
            #[cfg(not(raspi))]
            self.0,
        )
    }

    ///Consumes the Pin, returns an OutputPin and sets its mode to Output.
    #[inline]
    pub fn into_output(self) -> OutputPin {
        OutputPin(
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
    pub fn into_pwm(self, config: PwmConfig, polarity: Polarity) -> PwmPin {
        PwmPin(
            #[cfg(raspi)]
            {
                if let Some(channel) = self.pwm_channel().and_then(|channel| {
                    match config {
                        PwmConfig::Period { period, pulse_width } => rppal::pwm::Pwm::with_period(
                            channel,
                            period,
                            pulse_width,
                            polarity,
                            false,
                        ),
                        PwmConfig::Frequency { frequency, duty_cycle } => {
                            rppal::pwm::Pwm::with_frequency(
                                channel, frequency, duty_cycle, polarity, false,
                            )
                        },
                    }
                    .ok()
                }) {
                    // hardware pwm possible
                    Pwm::Hardware(channel)
                } else {
                    // fallback software pwm
                    Pwm::Software { pin: self.0.into_output(), config, polarity }
                }
            },
            #[cfg(not(raspi))]
            self.0,
        )
    }
}
/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug)]
pub struct InputPin(#[cfg(raspi)] gpio::InputPin, #[cfg(not(raspi))] u8);

impl InputPin {
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

/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPin(#[cfg(raspi)] gpio::OutputPin, #[cfg(not(raspi))] u8);

impl OutputPin {
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

    /// Sets the pin’s output state.
    #[cfg_attr(not(raspi), allow(unused_variables))]
    #[inline]
    pub fn write(&mut self, level: Level) {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.write(level)
            } else {
                debug!("{:?}.write({:?})", self, level);
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::Low`.
    #[inline]
    pub fn is_set_low(&self) -> bool {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.is_set_low()
            } else {
                debug!("{:?}.is_set_low()", self);
                false
            }
        }
    }

    /// Returns `true` if the pin's output state is set to `Level::High`.
    #[inline]
    pub fn is_set_high(&self) -> bool {
        cfg_if! {
            if #[cfg(raspi)] {
                self.0.is_set_high()
            } else {
                debug!("{:?}.is_set_high()", self);
                false
            }
        }
    }

    // maybe re-export more methods?
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.OutputPin.html
}

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug)]
pub struct PwmPin(#[cfg(raspi)] Pwm, #[cfg(not(raspi))] u8);
#[cfg(raspi)]
#[derive(Debug)]
enum Pwm {
    Hardware(pwm::Pwm),
    Software { pin: gpio::OutputPin, config: PwmConfig, polarity: Polarity },
}
/// Einstellung eines Pwm-Pulses.
#[derive(Debug)]
pub enum PwmConfig {
    /// Periodendauer und Pulsweite.
    Period { period: Duration, pulse_width: Duration },
    /// Frequenz (in Herz) und Duty-cycle ([0,1]) als Prozentwert.
    Frequency { frequency: f64, duty_cycle: f64 },
}

impl PwmPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
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
    // https://docs.rs/rppal/0.12.0/rppal/pwm/struct.Pwm.html
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.OutputPin.html#method.set_pwm
}
