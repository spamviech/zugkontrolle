//! Low level Steuerung von Pwm Signalen.

use std::{
    collections::HashSet,
    f64,
    fmt::{Debug, Display},
    io,
    sync::{RwLock, RwLockWriteGuard},
    time::Duration,
};

use log::{debug, error};
use num_traits::NumCast;
use once_cell::sync::Lazy;

#[cfg(not(raspi))]
#[derive(Debug)]
struct PwmState {
    channels: HashSet<bool>,
}

#[cfg(not(raspi))]
static PWM: Lazy<RwLock<PwmState>> =
    Lazy::new(|| RwLock::new(PwmState { channels: [false, true].into_iter().collect() }));

#[cfg(not(raspi))]
impl PwmState {
    fn write_static<'t>() -> RwLockWriteGuard<'t, PwmState> {
        match PWM.write() {
            Ok(guard) => guard,
            Err(poison_error) => {
                error!("Pwm-static poisoned: {:?}", poison_error);
                poison_error.into_inner()
            }
        }
    }
}

#[cfg(raspi)]
pub type Pwm = rppal::pwm::Pwm;
#[cfg(not(raspi))]
#[derive(Debug)]
pub struct Pwm {
    channel: Channel,
    period: Duration,
    pulse_width: Duration,
    polarity: Polarity,
    enabled: bool,
}

#[cfg(not(raspi))]
impl Drop for Pwm {
    fn drop(&mut self) {
        if !PwmState::write_static().channels.insert(self.channel.als_bool()) {
            error!("Dropped pwm channel was still available: {:?}", self.channel)
        }
    }
}

#[cfg(not(raspi))]
const NANOS_PER_SEC: f64 = 1_000_000_000.0;

#[cfg(not(raspi))]
fn period(frequency: f64) -> Duration {
    let period = if frequency > 0.0 { NANOS_PER_SEC / frequency } else { 0.0 };
    Duration::from_nanos(<u64 as NumCast>::from(period.floor()).unwrap_or_else(|| {
        error!("Cast of period {} to u64 failed!", period);
        0
    }))
}

#[cfg(not(raspi))]
impl Pwm {
    pub fn new(channel: Channel) -> Result<Pwm> {
        if PwmState::write_static().channels.remove(&channel.als_bool()) {
            Ok(Pwm {
                channel,
                period: Duration::ZERO,
                pulse_width: Duration::ZERO,
                polarity: Polarity::Normal,
                enabled: false,
            })
        } else {
            Err(Error::Io(io::Error::new(io::ErrorKind::AlreadyExists, channel.to_string())))
        }
    }

    pub fn with_period(
        channel: Channel,
        period: Duration,
        pulse_width: Duration,
        polarity: Polarity,
        enabled: bool,
    ) -> Result<Pwm> {
        if period < pulse_width {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("Period {:?} is shorter than the pulse width {:?}!", period, pulse_width),
            )))
        } else if PwmState::write_static().channels.remove(&channel.als_bool()) {
            Ok(Pwm { channel, period, pulse_width, polarity, enabled })
        } else {
            Err(Error::Io(io::Error::new(io::ErrorKind::AlreadyExists, channel.to_string())))
        }
    }

    pub fn with_frequency(
        channel: Channel,
        frequency: f64,
        duty_cycle: f64,
        polarity: Polarity,
        enabled: bool,
    ) -> Result<Pwm> {
        if PwmState::write_static().channels.remove(&channel.als_bool()) {
            let duty_cycle_checked = duty_cycle.max(0.0).min(1.0);
            let period = period(frequency);
            Ok(Pwm {
                channel,
                period,
                pulse_width: period.mul_f64(duty_cycle_checked),
                polarity,
                enabled,
            })
        } else {
            Err(Error::Io(io::Error::new(io::ErrorKind::AlreadyExists, channel.to_string())))
        }
    }

    pub fn period(&self) -> Result<Duration> {
        Ok(self.period)
    }

    /// Sets the period.
    ///
    /// `period` indicates the time it takes for the PWM channel to complete one cycle.
    ///
    /// This method will fail if `period` is shorter than the current pulse width.
    pub fn set_period(&mut self, period: Duration) -> Result<()> {
        debug!("{:?}.set_period({:?})", self, period);
        if period < self.pulse_width {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!(
                    "Period {:?} is shorter than the current pulse width {:?}!",
                    period, self.pulse_width
                ),
            )))
        } else {
            self.period = period;
            Ok(())
        }
    }

    pub fn pulse_width(&self) -> Result<Duration> {
        Ok(self.pulse_width)
    }

    /// Sets the pulse width.
    ///
    /// `pulse_width` indicates the amount of time the PWM channel is active during a
    /// single period.
    ///
    /// This method will fail if `pulse_width` is longer than the current period.
    pub fn set_pulse_width(&mut self, pulse_width: Duration) -> Result<()> {
        debug!("{:?}.set_pulse_width({:?})", self, pulse_width);
        if pulse_width > self.period {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!(
                    "Pulse width {:?} is bigger then the current period {:?}!",
                    pulse_width, self.period
                ),
            )))
        } else {
            self.pulse_width = pulse_width;
            Ok(())
        }
    }

    pub fn frequency(&self) -> Result<f64> {
        let period = <f64 as NumCast>::from(self.period.as_nanos()).unwrap_or_default();
        if period > 0.0 {
            Ok(NANOS_PER_SEC / period)
        } else {
            Ok(0.0)
        }
    }

    /// Sets the frequency and duty cycle.
    ///
    /// `set_frequency` is a convenience method that converts `frequency` to a period,
    /// and calculates the duty cycle as a percentage of the frequency.
    ///
    /// `frequency` is specified in hertz (Hz).
    ///
    /// `duty_cycle` is specified as a floating point value between `0.0` (0%) and `1.0` (100%).
    pub fn set_frequency(&mut self, frequency: f64, duty_cycle: f64) -> Result<()> {
        debug!("{:?}.set_frequency({:?}, {:?})", self, frequency, duty_cycle);
        let duty_cycle_checked = duty_cycle.max(0.0).min(1.0);
        let period = period(frequency);
        self.period = period;
        self.pulse_width = period.mul_f64(duty_cycle_checked);
        Ok(())
    }

    pub fn duty_cycle(&self) -> Result<f64> {
        let period = self.period.as_secs_f64();
        if period > 0.0 {
            Ok(self.pulse_width.as_secs_f64() / period)
        } else {
            Ok(0.0)
        }
    }

    /// Sets the duty cycle.
    ///
    /// `set_duty_cycle` is a convenience method that converts `duty_cycle` to a
    /// pulse width based on the configured period.
    ///
    /// `duty_cycle` is specified as a floating point value between `0.0` (0%) and `1.0` (100%).
    pub fn set_duty_cycle(&mut self, duty_cycle: f64) -> Result<()> {
        debug!("{:?}.set_duty_cycle({:?})", self, duty_cycle);
        let duty_cycle_checked = duty_cycle.max(0.0).min(1.0);
        self.pulse_width = self.period.mul_f64(duty_cycle_checked);
        Ok(())
    }

    pub fn polarity(&self) -> Result<Polarity> {
        Ok(self.polarity)
    }

    pub fn set_polarity(&mut self, polarity: Polarity) -> Result<()> {
        debug!("{:?}.set_polarity({:?})", self, polarity);
        self.polarity = polarity;
        Ok(())
    }

    pub fn is_enabled(&self) -> Result<bool> {
        Ok(self.enabled)
    }

    pub fn enable(&mut self) -> Result<()> {
        debug!("{:?}.enable()", self);
        self.enabled = true;
        Ok(())
    }

    pub fn disable(&mut self) -> Result<()> {
        debug!("{:?}.enable()", self);
        self.enabled = false;
        Ok(())
    }
}

#[cfg(raspi)]
pub type Channel = rppal::pwm::Channel;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Channel {
    Pwm0,
    Pwm1,
}

#[cfg(not(raspi))]
impl Display for Channel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Channel as Debug>::fmt(&self, f)
    }
}

#[cfg(not(raspi))]
impl Channel {
    #[inline(always)]
    fn als_bool(self) -> bool {
        self == Channel::Pwm1
    }
}

#[cfg(raspi)]
pub type Polarity = rppal::pwm::Polarity;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Polarity {
    Normal,
    Inverse,
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(raspi)]
pub type Error = rppal::pwm::Error;
#[cfg(not(raspi))]
#[derive(Debug)]
pub enum Error {
    Io(io::Error),
}
