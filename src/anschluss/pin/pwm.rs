//! Gpio Pins für Pwm konfiguriert.

use std::time::Duration;

#[cfg(not(raspi))]
use log::debug;
#[cfg(raspi)]
use rppal::{gpio, pwm};
use serde::{Deserialize, Serialize};

#[cfg(not(raspi))]
use super::Wrapper;
use crate::anschluss::{
    anschlüsse::Anschlüsse,
    polarität::Polarität,
    speichern_laden::{self, Reserviere, Reserviert, ToSave},
    InputAnschluss, OutputAnschluss,
};

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug, PartialEq)]
pub struct Pin {
    #[cfg(raspi)]
    pub(super) pin: Pwm,
    #[cfg(not(raspi))]
    pub(super) pin: Wrapper,
    pub(super) config: Option<Config>,
}

#[cfg(raspi)]
#[derive(Debug)]
pub(super) enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
    Software(gpio::OutputPin),
}
#[cfg(raspi)]
impl PartialEq for Pwm {
    fn eq(&self, other: &Pwm) -> bool {
        match (self, other) {
            (Pwm::Hardware(_, pin0), Pwm::Hardware(_, pin1)) => pin0 == pin1,
            (Pwm::Software(pin0), Pwm::Software(pin1)) => pin0 == pin1,
            _ => false,
        }
    }
}

/// Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    pub time: Time,
    pub polarity: Polarität,
}
impl Config {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// Time::valide muss /true/ sein.
    pub fn new(time: Time, polarity: Polarität) -> Option<Self> {
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
    #[inline(always)]
    pub fn valide(&self) -> bool {
        self.time.valide()
    }
}
/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, PartialEq)]
pub enum Time {
    /// Periodendauer und Pulsweite.
    Period { period: Duration, pulse_width: Duration },
    /// Frequenz (in Herz) und Duty-cycle (\[0,1\]) als Prozentwert.
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
        #[cfg(raspi)]
        {
            match &self.pin {
                Pwm::Hardware(_pwm, pin) => pin.pin(),
                Pwm::Software(pin) => pin.pin(),
            }
        }
        #[cfg(not(raspi))]
        {
            self.pin.0
        }
    }

    /// Wird Hardware-Pwm verwendet?
    pub fn hardware_pwm(&self) -> bool {
        #[cfg(raspi)]
        {
            match self.pin {
                Pwm::Hardware(_, _) => true,
                Pwm::Software(_) => false,
            }
        }
        #[cfg(not(raspi))]
        {
            false
        }
    }

    /// Ist der Pwm-Puls aktiv?
    pub fn is_enabled(&self) -> Result<&Option<Config>, Error> {
        match &self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, _pin) => {
                Ok(if pwm_channel.is_enabled()? {
                    &self.config
                } else {
                    // TODO
                    &None
                })
            }
            #[cfg(raspi)]
            Pwm::Software(_pin) => Ok(&self.config),
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.is_enabled()", self);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Aktiviere den Pwm-Puls.
    pub fn enable_with_config(&mut self, config: Config) -> Result<(), Error> {
        match &mut self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, _pin) => {
                // update nur, sofern sich Parameter geändert haben.
                if self.config.as_ref().map(|Config { polarity, .. }| polarity)
                    != Some(&config.polarity)
                {
                    pwm_channel.set_polarity(config.polarity.into())?;
                }
                if self.config.as_ref().map(|Config { time, .. }| time) != Some(&config.time) {
                    match config.time {
                        Time::Period { period, pulse_width } => {
                            pwm_channel.set_period(period)?;
                            pwm_channel.set_pulse_width(pulse_width)?;
                        }
                        Time::Frequency { frequency, duty_cycle } => {
                            pwm_channel.set_frequency(frequency, duty_cycle)?;
                        }
                    }
                }
                Ok(pwm_channel.enable()?)
            }
            #[cfg(raspi)]
            Pwm::Software(pin) => match config.time {
                Time::Period { period, mut pulse_width } => {
                    if config.polarity == Polarität::Invertiert {
                        pulse_width = period - pulse_width;
                    }
                    Ok(pin.set_pwm(period, pulse_width)?)
                }
                Time::Frequency { frequency, mut duty_cycle } => {
                    if config.polarity == Polarität::Invertiert {
                        duty_cycle = 1. - duty_cycle;
                    }
                    Ok(pin.set_pwm_frequency(frequency, duty_cycle)?)
                }
            },
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.enable_with_config({:?})", self, config);
                Err(Error::KeinRaspberryPi)
            }
        }
    }

    /// Deaktiviere den Pwm-Puls
    pub fn disable(&mut self) -> Result<(), Error> {
        match &mut self.pin {
            #[cfg(raspi)]
            Pwm::Hardware(pwm_channel, _pin) => {
                pwm_channel.disable()?;
            }
            #[cfg(raspi)]
            Pwm::Software(pin) => {
                pin.clear_pwm()?;
            }
            #[cfg(not(raspi))]
            _pin => {
                debug!("{:?}.disable()", self);
                return Err(Error::KeinRaspberryPi);
            }
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

/// Serealisierbare Informationen einen Pwm-Pins.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Save(pub u8);
impl ToSave for Pin {
    type Save = Save;

    fn to_save(&self) -> Save {
        Save(self.pin())
    }

    fn anschlüsse(self) -> (Vec<self::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (vec![self], Vec::new(), Vec::new())
    }
}
impl Reserviere<Pin> for Save {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<Pin>,
        output_nicht_benötigt: Vec<OutputAnschluss>,
        input_nicht_benötigt: Vec<InputAnschluss>,
    ) -> speichern_laden::Result<Pin> {
        let (mut gesucht, pwm_nicht_benötigt): (Vec<_>, Vec<_>) =
            pwm_pins.into_iter().partition(|pin| pin.to_save() == self);
        if let Some(anschluss) = gesucht.pop() {
            Ok(Reserviert {
                anschluss,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            })
        } else {
            match anschlüsse.reserviere_pin(self.0).map(super::Pin::into_pwm) {
                Ok(anschluss) => Ok(Reserviert {
                    anschluss,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }),
                Err(error) => Err(speichern_laden::Error {
                    fehler: error.into(),
                    pwm_pins: pwm_nicht_benötigt,
                    output_anschlüsse: output_nicht_benötigt,
                    input_anschlüsse: input_nicht_benötigt,
                }),
            }
        }
    }
}
