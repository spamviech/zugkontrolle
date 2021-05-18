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
    pub fn into_pwm(self) -> PwmPin {
        cfg_if! {
            if #[cfg(raspi)]
            {
                if let Some(pwm) = self.pwm_channel().and_then(|channel| pwm::Pwm::new().ok()) {
                    let config = pwm.polarity().and_then(|polarity|
                                    pwm.period().and_then(|period|
                                    pwm.pulse_width().and_then(|pulse_width|
                                    PwmConfig {time: PwmTime::Period {period, pulse_width}, polarity}))
                                ).ok();
                    PwmPin {pin: Pwm::Hardware(pwm, pin: self.0), config }
                } else {
                    // fallback software pwm
                   PwmPin {pin: Pwm::Software(self.0.into_output()), config: None }
                }
            } else {
                PwmPin {pin: self.0, config:None}
            }
        }
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
pub struct PwmPin {
    #[cfg(raspi)]
    pin: Pwm,
    #[cfg(not(raspi))]
    pin: u8,
    config: Option<PwmConfig>,
}
#[cfg(raspi)]
#[derive(Debug)]
enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
    Software(gpio::OutputPin),
}
/// Einstellung eines Pwm-Pulses.
#[derive(Debug)]
pub struct PwmConfig {
    time: PwmTime,
    polarity: Polarity,
}
impl PwmConfig {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// PwmTime::valide muss /true/ sein.
    pub fn new(time: PwmTime, polarity: Polarity) -> Option<Self> {
        if time.valide() {
            Some(PwmConfig { time, polarity })
        } else {
            None
        }
    }
}
/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug)]
pub enum PwmTime {
    /// Periodendauer und Pulsweite.
    Period { period: Duration, pulse_width: Duration },
    /// Frequenz (in Herz) und Duty-cycle ([0,1]) als Prozentwert.
    Frequency { frequency: f64, duty_cycle: f64 },
}
impl PwmTime {
    /// Nicht alle Zeit-Werte erlauben einen sinnvollen Pwm-Puls.
    ///
    /// Es muss gelten:
    /// - period >= pulse_width
    /// - 0 <= duty_cycle <= 1
    pub fn valide(&self) -> bool {
        match self {
            PwmTime::Period { period, pulse_width } => pulse_width <= period,
            PwmTime::Frequency { frequency: _, duty_cycle } => {
                &0. <= duty_cycle && duty_cycle <= &1.
            },
        }
    }
}

impl PwmPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    pub fn pin(&self) -> u8 {
        cfg_if! {
            if #[cfg(raspi)] {
                match self.pin {
                    // TODO
                    Pwm::Hardware(pwm, pin) => pin.pin(),
                    Pwm::Software(pin) => pin.pin(),
                }
            } else {
                // Pins sollten nur auf einem Raspi erzeugbar sein!
                // Liefere Standard-Wert, der in näherer Zukunft nicht von Pins erreicht wird
                self.pin
            }
        }
    }

    /// Wird Hardware-Pwm verwendet?
    pub fn hardware_pwm(&self) -> bool {
        cfg_if! {
            if #[cfg(raspi)] {
                match self.0 {
                    Pwm::Hardware(_) => true,
                    Pwm::HardwareFailedConfig(_) => true,
                    Pwm::Software {..} => false,
                }
            } else {
                false
            }
        }
    }

    /// Ist der Pwm-Puls aktiv?
    pub fn is_enabled(&self) -> Option<PwmConfig> {
        // TODO what about result value???
        todo!()
    }

    /// Aktiviere den Pwm-Puls.
    pub fn enable_with_config(&mut self, config: PwmConfig) {
        // TODO what about result value???
        todo!()

        // hardware pwm
        // let polarity_result = pwm_channel.set_polarity(config.polarity);
        // let time_result = match &config.time {
        //     PwmTime::Period { period, pulse_width } => pwm_channel
        //         .set_period(period)
        //         .and_then(|pwm_channel| pwm_channel.set_pulse_width(pulse_width)),
        //     PwmTime::Frequency { frequency, duty_cycle } => pwm_channel
        //         .set_frequency(frequency)
        //         .and_then(|pwm_channel| pwm_channel.set_duty_cycle(duty_cycle)),
        // };
    }

    /// Deaktiviere den Pwm-Puls
    pub fn disable(&mut self) {
        // TODO what about result value???
        todo!()
    }

    // TODO cfg-reexport/stub-methods
    // https://docs.rs/rppal/0.12.0/rppal/pwm/struct.Pwm.html
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.OutputPin.html#method.set_pwm
}
