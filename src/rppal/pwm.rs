//! Low level Steuerung von Pwm Signalen.

#[cfg(not(raspi))]
use std::{
    f64,
    fmt::{Debug, Display},
    io,
    sync::{RwLock, RwLockWriteGuard},
    time::Duration,
};

#[cfg(not(raspi))]
use log::{debug, error};
#[cfg(not(raspi))]
use num_traits::NumCast;
#[cfg(not(raspi))]
use once_cell::sync::Lazy;

#[cfg(not(raspi))]
#[derive(Debug)]
struct PwmState {
    pwm0: Option<Pwm>,
    pwm1: Option<Pwm>,
}

#[cfg(not(raspi))]
static PWM: Lazy<RwLock<PwmState>> = Lazy::new(|| {
    RwLock::new(PwmState {
        pwm0: Some(Pwm::init(Channel::Pwm0)),
        pwm1: Some(Pwm::init(Channel::Pwm1)),
    })
});

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

    fn write_channel<'t>(&'t mut self, channel: Channel) -> &'t mut Option<Pwm> {
        let PwmState { pwm0, pwm1 } = self;
        match channel {
            Channel::Pwm0 => pwm0,
            Channel::Pwm1 => pwm1,
        }
    }
}

/// Provides access to the Raspberry Pi’s PWM peripherals.
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
        let mut guard = PwmState::write_static();
        let pwm = guard.write_channel(self.channel);
        match pwm {
            Some(pwm) => {
                error!("Dropped pwm channel {:?} was still available: {:?}\nDropped without restoring: {:?}", self.channel, pwm,self);
            }
            None => {
                *pwm = Some(Pwm {
                    channel: self.channel,
                    period: self.period,
                    pulse_width: self.pulse_width,
                    polarity: self.polarity,
                    enabled: false,
                })
            }
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
    fn init(channel: Channel) -> Pwm {
        Pwm {
            channel,
            period: Duration::ZERO,
            pulse_width: Duration::ZERO,
            polarity: Polarity::Normal,
            enabled: false,
        }
    }

    /// Constructs a new Pwm.
    pub fn new(channel: Channel) -> Result<Pwm> {
        let mut guard = PwmState::write_static();
        let pwm = guard.write_channel(channel);
        pwm.take()
            .ok_or(Error::Io(io::Error::new(io::ErrorKind::AlreadyExists, channel.to_string())))
    }

    /// Constructs a new Pwm using the specified settings.
    ///
    /// This method will fail if period is shorter than pulse_width.
    pub fn with_period(
        channel: Channel,
        period: Duration,
        pulse_width: Duration,
        polarity: Polarity,
        enabled: bool,
    ) -> Result<Pwm> {
        let mut pwm = Pwm::new(channel)?;
        if period < pulse_width {
            Err(Error::Io(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("Period {:?} is shorter than the pulse width {:?}!", period, pulse_width),
            )))
        } else {
            pwm.period = period;
            pwm.pulse_width = pulse_width;
            pwm.polarity = polarity;
            pwm.enabled = enabled;
            Ok(pwm)
        }
    }

    /// Constructs a new Pwm using the specified settings.
    pub fn with_frequency(
        channel: Channel,
        frequency: f64,
        duty_cycle: f64,
        polarity: Polarity,
        enabled: bool,
    ) -> Result<Pwm> {
        let duty_cycle_checked = duty_cycle.max(0.0).min(1.0);
        let period = period(frequency);
        Pwm::with_period(channel, period, period.mul_f64(duty_cycle_checked), polarity, enabled)
    }

    /// Returns the period.
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

    /// Returns the pulse width.
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

    /// Returns the frequency of the pwm pulse.
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

    /// Returns the duty cycle of the pwm pulse.
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

    /// Returns the polarity of the pwm pulse.
    pub fn polarity(&self) -> Result<Polarity> {
        Ok(self.polarity)
    }

    /// Sets the polarity of the pwm pulse.
    pub fn set_polarity(&mut self, polarity: Polarity) -> Result<()> {
        debug!("{:?}.set_polarity({:?})", self, polarity);
        self.polarity = polarity;
        Ok(())
    }

    /// Returns wether the pwm pulse is currently enabled
    pub fn is_enabled(&self) -> Result<bool> {
        Ok(self.enabled)
    }

    /// Enables the pwm pulse.
    pub fn enable(&mut self) -> Result<()> {
        debug!("{:?}.enable()", self);
        self.enabled = true;
        Ok(())
    }

    /// Disables the pwm pulse.
    pub fn disable(&mut self) -> Result<()> {
        debug!("{:?}.enable()", self);
        self.enabled = false;
        Ok(())
    }
}

/// Pwm channels.
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

/// Polarity of a pwm pulse.
#[cfg(raspi)]
pub type Polarity = rppal::pwm::Polarity;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Polarity {
    Normal,
    Inverse,
}

/// Result with `pwm::Error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur when accessing the PWM peripheral.
#[cfg(raspi)]
pub type Error = rppal::pwm::Error;
#[cfg(not(raspi))]
#[derive(Debug)]
pub enum Error {
    Io(io::Error),
}