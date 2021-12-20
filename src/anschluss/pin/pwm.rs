//! Gpio Pins für Pwm konfiguriert.

use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        polarität::Polarität,
        InputAnschluss, OutputAnschluss,
    },
    rppal::{gpio, pwm},
};

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug, PartialEq)]
pub struct Pin {
    pub(super) pin: Pwm,
    pub(super) config: Option<Config>,
}

#[allow(variant_size_differences)]
#[derive(Debug)]
pub(super) enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
    Software(gpio::OutputPin),
}

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
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Config {
    pub time: Time,
    pub polarity: Polarität,
}

impl Config {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// Time::valide muss `true` sein.
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
#[derive(Debug, Clone, Copy, PartialEq)]
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
        match &self.pin {
            Pwm::Hardware(_pwm, pin) => pin.pin(),
            Pwm::Software(pin) => pin.pin(),
        }
    }

    /// Wird Hardware-Pwm verwendet?
    pub fn hardware_pwm(&self) -> bool {
        match self.pin {
            Pwm::Hardware(_, _) => true,
            Pwm::Software(_) => false,
        }
    }

    /// Ist der Pwm-Puls aktiv?
    pub fn is_enabled(&self) -> Result<&Option<Config>, Fehler> {
        match &self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                let enabled = pwm_channel
                    .is_enabled()
                    .map_err(|fehler| Fehler::Pwm { pin: self.pin(), fehler })?;
                Ok(if enabled { &self.config } else { &None })
            }
            Pwm::Software(_pin) => Ok(&self.config),
        }
    }

    /// Aktiviere den Pwm-Puls.
    pub fn enable_with_config(&mut self, config: Config) -> Result<(), Fehler> {
        let pin = self.pin();
        match &mut self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                let map_fehler = |fehler| Fehler::Pwm { pin, fehler };
                // update nur, sofern sich Parameter geändert haben.
                if self.config.as_ref().map(|Config { polarity, .. }| polarity)
                    != Some(&config.polarity)
                {
                    pwm_channel.set_polarity(config.polarity.into()).map_err(map_fehler)?;
                }
                if self.config.as_ref().map(|Config { time, .. }| time) != Some(&config.time) {
                    match config.time {
                        Time::Period { period, pulse_width } => {
                            pwm_channel.set_pulse_width(Duration::ZERO).map_err(map_fehler)?;
                            pwm_channel.set_period(period).map_err(map_fehler)?;
                            pwm_channel.set_pulse_width(pulse_width).map_err(map_fehler)?;
                        }
                        Time::Frequency { frequency, duty_cycle } => {
                            pwm_channel.set_frequency(frequency, duty_cycle).map_err(map_fehler)?;
                        }
                    }
                }
                Ok(pwm_channel.enable().map_err(map_fehler)?)
            }
            Pwm::Software(pwm_pin) => {
                let map_fehler = |fehler| Fehler::Gpio { pin, fehler };
                match config.time {
                    Time::Period { period, mut pulse_width } => {
                        if config.polarity == Polarität::Invertiert {
                            pulse_width = period - pulse_width;
                        }
                        Ok(pwm_pin.set_pwm(period, pulse_width).map_err(map_fehler)?)
                    }
                    Time::Frequency { frequency, mut duty_cycle } => {
                        if config.polarity == Polarität::Invertiert {
                            duty_cycle = 1. - duty_cycle;
                        }
                        Ok(pwm_pin.set_pwm_frequency(frequency, duty_cycle).map_err(map_fehler)?)
                    }
                }
            }
        }
    }

    /// Deaktiviere den Pwm-Puls
    pub fn disable(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        Ok(match &mut self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                pwm_channel.disable().map_err(|fehler| Fehler::Pwm { pin, fehler })?;
            }
            Pwm::Software(pwm_pin) => {
                pwm_pin.clear_pwm().map_err(|fehler| Fehler::Gpio { pin, fehler })?;
            }
        })
    }
}

#[derive(Debug)]
pub enum Fehler {
    Gpio {
        pin: u8,
        fehler: gpio::Error,
    },
    Pwm {
        pin: u8,
        fehler: pwm::Error,
    },
    #[cfg(not(raspi))]
    KeinRaspberryPi(u8),
    InvalideConfig {
        pin: u8,
        config: Config,
    },
}

/// Serealisierbare Informationen einen Pwm-Pins.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Serialisiert(pub u8);
impl Serialisiere for Pin {
    type Serialisiert = Serialisiert;

    fn serialisiere(&self) -> Serialisiert {
        Serialisiert(self.pin())
    }

    fn anschlüsse(self) -> (Vec<Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (vec![self], Vec::new(), Vec::new())
    }
}
impl Reserviere<Pin> for Serialisiert {
    fn reserviere(
        self,
        pwm_pins: Vec<Pin>,
        output_nicht_benötigt: Vec<OutputAnschluss>,
        input_nicht_benötigt: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Pin> {
        let (mut gesucht, pwm_nicht_benötigt): (Vec<_>, Vec<_>) =
            pwm_pins.into_iter().partition(|pin| pin.serialisiere() == self);
        if let Some(anschluss) = gesucht.pop() {
            Ok(Reserviert {
                anschluss,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            })
        } else {
            match anschluss::pin::Lager::reserviere_pin(todo!(), self.0).map(super::Pin::into_pwm) {
                Ok(anschluss) => Ok(Reserviert {
                    anschluss,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }),
                Err(error) => Err(de_serialisieren::Fehler {
                    fehler: error.into(),
                    pwm_pins: pwm_nicht_benötigt,
                    output_anschlüsse: output_nicht_benötigt,
                    input_anschlüsse: input_nicht_benötigt,
                }),
            }
        }
    }
}
