//! Gpio Pins für Pwm konfiguriert.

use std::time::Duration;

use cfg_if::cfg_if;

use crate::anschluss::polarity::Polarity;

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug)]
pub struct Pin {
    #[cfg(raspi)]
    pub(super) pin: Pwm,
    #[cfg(not(raspi))]
    pub(super) pin: u8,
    pub(super) config: Option<Config>,
}
#[cfg(raspi)]
#[derive(Debug)]
enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
    Software(gpio::OutputPin),
}
/// Einstellung eines Pwm-Pulses.
#[derive(Debug)]
pub struct Config {
    time: Time,
    polarity: Polarity,
}
impl Config {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// Time::valide muss /true/ sein.
    pub fn new(time: Time, polarity: Polarity) -> Option<Self> {
        if time.valide() {
            Some(Config { time, polarity })
        } else {
            None
        }
    }
}
/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug)]
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
    pub fn is_enabled(&self) -> Option<Config> {
        // TODO what about result value???
        todo!()
    }

    /// Aktiviere den Pwm-Puls.
    pub fn enable_with_config(&mut self, config: Config) {
        // TODO what about result value???
        todo!()

        // hardware pwm
        // let polarity_result = pwm_channel.set_polarity(config.polarity);
        // let time_result = match &config.time {
        //     Time::Period { period, pulse_width } => pwm_channel
        //         .set_period(period)
        //         .and_then(|pwm_channel| pwm_channel.set_pulse_width(pulse_width)),
        //     Time::Frequency { frequency, duty_cycle } => pwm_channel
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
