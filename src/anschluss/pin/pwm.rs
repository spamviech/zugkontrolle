//! Gpio Pins für Pwm konfiguriert.

use std::time::Duration;

use cfg_if::cfg_if;
use log::debug;
#[cfg(raspi)]
use rppal::{gpio, pwm};

use crate::anschluss::polarity::Polarity;

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug, PartialEq)]
pub struct Pin {
    #[cfg(raspi)]
    pub(super) pin: Pwm,
    #[cfg(not(raspi))]
    pub(super) pin: u8,
    pub(super) config: Option<Config>,
}
#[cfg(raspi)]
#[derive(Debug, PartialEq)]
enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
    Software(gpio::OutputPin),
}
/// Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    pub time: Time,
    pub polarity: Polarity,
}
impl Config {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// Time::valide muss /true/ sein.
    pub fn new(time: Time, polarity: Polarity) -> Option<Self> {
        let config = Config { time, polarity };
        if config.valide() {
            Some(config)
        } else {
            None
        }
    }

    /// Nicht alle Zeit-Werte erlauben einen sinnvollen Pwm-Puls.
    ///
    /// Es muss gelten:
    /// - period >= pulse_width
    /// - 0 <= duty_cycle <= 1
    #[inline]
    pub fn valide(&self) -> bool {
        self.time.valide()
    }
}
/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, PartialEq)]
pub enum Time {
    /// Periodendauer und Pulsweite.
    Period { period: Duration, pulse_width: Duration },
    /// Frequenz (in Herz) und Duty-cycle ([0,1]) als Prozentwert.
    Frequency { frequency: f64, duty_cycle: f64 },
}
impl Time {
    /// Nicht alle Zeit-Werte erlauben einen sinnvollen Pwm-Puls.
    ///
    /// Es muss gelten:
    /// - period >= pulse_width
    /// - 0 <= duty_cycle <= 1
    pub fn valide(&self) -> bool {
        match self {
            Time::Period { period, pulse_width } => pulse_width <= period,
            Time::Frequency { frequency: _, duty_cycle } => &0. <= duty_cycle && duty_cycle <= &1.,
        }
    }
}

impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    pub fn pin(&self) -> u8 {
        cfg_if! {
            if #[cfg(raspi)] {
                match self.pin {
                    Pwm::Hardware(pwm, pin) => pin.pin(),
                    Pwm::Software(pin) => pin.pin(),
                }
            } else {
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
    pub fn is_enabled(&self) -> Result<&Option<Config>, Error> {
        match self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, pin) => {
                if pwm_channel.is_enabled()? {
                    &self.config
                } else {
                    None
                }
            },
            #[cfg(raspi)]
            Pwm::Software(pin) => &self.config,
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.is_enabled()", self);
                Err(Error::KeinRaspberryPi)
            },
        }
    }

    /// Aktiviere den Pwm-Puls.
    pub fn enable_with_config(&mut self, config: Config) -> Result<(), Error> {
        match self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, _pin) => {
                // update nur, sofern sich Parameter geändert haben.
                if self.config.and_then(|c| c.polarity) != Some(&config.polarity) {
                    pwm_channel.set_polarity(config.polarity)?;
                }
                if self.config.and_then(|c| c.time) != Some(&config.time) {
                    match config.time {
                        Time::Period { period, pulse_width } => {
                            pwm_channel.set_period(period)?;
                            pwm_channel.set_pulse_width(pulse_width)?;
                        },
                        Time::Frequency { frequency, duty_cycle } => {
                            pwm_channel.set_frequency(frequency)?;
                            pwm_channel.set_duty_cycle(duty_cycle)?;
                        },
                    }
                }
                Ok(pwm_channel.enable()?)
            },
            #[cfg(raspi)]
            Pwm::Software(pin) => match config.time {
                Time::Period { period, pulse_width: mut pulse_width } => {
                    if polarity == Polarity::Inverse {
                        pulse_width = period - pulse_width;
                    }
                    Ok(pin.set_pwm(period, pulse_width)?)
                },
                Time::Frequency { frequency, duty_cycle: mut duty_cycle } => {
                    if polarity == Polarity::Inverse {
                        duty_cycle = 1. - duty_cycle;
                    }
                    Ok(pin.set_pwm_frequency(frequency, duty_cycle)?)
                },
            },
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.enable_with_config({:?})", self, config);
                Err(Error::KeinRaspberryPi)
            },
        }
    }

    /// Deaktiviere den Pwm-Puls
    pub fn disable(&mut self) -> Result<(), Error> {
        match self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, _pin) => {
                pwm_channel.disable()?;
            },
            #[cfg(raspi)]
            Pwm::Software(pin) => {
                pin.clear_pwm()?;
            },
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.disable()", self);
                return Err(Error::KeinRaspberryPi)
            },
        }
        #[cfg_attr(not(raspi), allow(unreachable_code))]
        {
            self.config = None;
            Ok(())
        }
    }

    // TODO cfg-reexport/stub-methods
    // https://docs.rs/rppal/0.12.0/rppal/pwm/struct.Pwm.html
    // https://docs.rs/rppal/0.12.0/rppal/gpio/struct.OutputPin.html#method.set_pwm
}

#[derive(Debug)]
pub enum Error {
    #[cfg(raspi)]
    Gpio(gpio::Error),
    #[cfg(raspi)]
    Pwm(pwm::Error),
    #[cfg(not(raspi))]
    KeinRaspberryPi,
    InvalideConfig(Config),
}
#[cfg(raspi)]
impl From<gpio::Error> for Error {
    fn from(error: gpio::Error) -> Self {
        Error::Gpio(error)
    }
}
#[cfg(raspi)]
impl From<pwm::Error> for Error {
    fn from(error: pwm::Error) -> Self {
        Error::Pwm(error)
    }
}
