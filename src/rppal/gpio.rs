//! Low level Steuerung von Gpio Pins.

#[cfg(not(raspi))]
use std::{
    collections::HashSet,
    io,
    ops::Not,
    sync::{RwLock, RwLockWriteGuard},
    time::Duration,
};

#[cfg(not(raspi))]
use log::{debug, error};
#[cfg(not(raspi))]
use once_cell::sync::Lazy;

#[cfg(not(raspi))]
#[derive(Debug)]
struct GpioState {
    pins: HashSet<u8>,
}

#[cfg(not(raspi))]
const MAX_PIN: u8 = 27;

#[cfg(not(raspi))]
static GPIO: Lazy<RwLock<GpioState>> =
    Lazy::new(|| RwLock::new(GpioState { pins: (1..=MAX_PIN).collect() }));

#[cfg(not(raspi))]
impl GpioState {
    fn write_static<'t>() -> RwLockWriteGuard<'t, GpioState> {
        match GPIO.write() {
            Ok(guard) => guard,
            Err(poison_error) => {
                error!("Gpio-static poisoned: {:?}", poison_error);
                poison_error.into_inner()
            }
        }
    }
}
/// Provides access to the Raspberry Pi’s GPIO peripheral.
#[cfg(raspi)]
pub type Gpio = rppal::gpio::Gpio;
#[cfg(not(raspi))]
#[derive(Debug, Clone)]
#[allow(missing_copy_implementations)]
pub struct Gpio;
#[cfg(not(raspi))]
impl Gpio {
    /// Constructs a new `Gpio`.
    pub fn new() -> Result<Gpio> {
        Ok(Gpio)
    }

    /// Returns a [`Pin`] for the specified BCM GPIO pin number.
    ///
    /// Retrieving a GPIO pin grants access to the pin through an owned [`Pin`] instance.
    /// If the pin is already in use, or the GPIO peripheral doesn't expose a pin with the
    /// specified number, `get` returns `Err(`[`Error::PinNotAvailable`]`)`. After a [`Pin`]
    /// (or a derived [`InputPin`], [`OutputPin`]) goes out of scope, it
    /// can be retrieved again through another `get` call.
    ///
    /// [`Pin`]: struct.Pin.html
    /// [`InputPin`]: struct.InputPin.html
    /// [`OutputPin`]: struct.OutputPin.html
    /// [`Error::PinNotAvailable`]: enum.Error.html#variant.PinNotAvailable
    pub fn get(&self, pin: u8) -> Result<Pin> {
        if GpioState::write_static().pins.remove(&pin) {
            Ok(Pin(pin))
        } else {
            Err(Error::PinNotAvailable(pin))
        }
    }
}

#[cfg(raspi)]
pub type Pin = rppal::gpio::Pin;
#[cfg(not(raspi))]
#[derive(Debug, PartialEq, Eq)]
pub struct Pin(u8);

#[cfg(not(raspi))]
impl Drop for Pin {
    fn drop(&mut self) {
        if !GpioState::write_static().pins.insert(self.0) {
            error!("Dropped pin was still available: {}", self.0)
        }
    }
}

#[cfg(not(raspi))]
impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    pub fn pin(&self) -> u8 {
        self.0
    }

    /// Consumes the `Pin` and returns an [`InputPin`]. Sets the mode to [`Input`]
    /// and disables the pin's built-in pull-up/pull-down resistors.
    ///
    /// [`InputPin`]: struct.InputPin.html
    /// [`Input`]: enum.Mode.html#variant.Input
    pub fn into_input(self) -> InputPin {
        InputPin(self, PullUpDown::Off)
    }

    /// Consumes the `Pin` and returns an [`InputPin`]. Sets the mode to [`Input`]
    /// and enables the pin's built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when `InputPin` goes out of scope if [`reset_on_drop`]
    /// is set to `true` (default).
    ///
    /// [`InputPin`]: struct.InputPin.html
    /// [`Input`]: enum.Mode.html#variant.Input
    /// [`reset_on_drop`]: struct.InputPin.html#method.set_reset_on_drop
    pub fn into_input_pulldown(self) -> InputPin {
        InputPin(self, PullUpDown::PullDown)
    }

    /// Consumes the `Pin` and returns an [`InputPin`]. Sets the mode to [`Input`]
    /// and enables the pin's built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when `InputPin` goes out of scope if [`reset_on_drop`]
    /// is set to `true` (default).
    ///
    /// [`InputPin`]: struct.InputPin.html
    /// [`Input`]: enum.Mode.html#variant.Input
    /// [`reset_on_drop`]: struct.InputPin.html#method.set_reset_on_drop
    pub fn into_input_pullup(self) -> InputPin {
        InputPin(self, PullUpDown::PullUp)
    }

    /// Consumes the `Pin` and returns an [`OutputPin`]. Sets the mode to [`Mode::Output`]
    /// and leaves the logic level unchanged.
    pub fn into_output(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the `Pin` and returns an [`OutputPin`]. Changes the logic level to
    /// [`Level::Low`] and then sets the mode to [`Mode::Output`].
    pub fn into_output_low(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the `Pin` and returns an [`OutputPin`]. Changes the logic level to
    /// [`Level::High`] and then sets the mode to [`Mode::Output`].
    pub fn into_output_high(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }
}

#[cfg(raspi)]
pub type InputPin = rppal::gpio::InputPin;
#[cfg(not(raspi))]
#[derive(Debug)]
pub struct InputPin(Pin, PullUpDown);

#[cfg(not(raspi))]
impl PartialEq for InputPin {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[cfg(not(raspi))]
impl Eq for InputPin {}

#[cfg(not(raspi))]
impl InputPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Reads the pin's logic level.
    pub fn read(&self) -> Level {
        debug!("{:?}.read()", self);
        Level::Low
    }

    /// Reads the pin's logic level, and returns `true` if it's set to [`Low`].
    ///
    /// [`Low`]: enum.Level.html#variant.Low
    pub fn is_low(&self) -> bool {
        debug!("{:?}.is_low()", self);
        true
    }

    /// Reads the pin's logic level, and returns `true` if it's set to [`High`].
    ///
    /// [`High`]: enum.Level.html#variant.High
    pub fn is_high(&self) -> bool {
        debug!("{:?}.is_high()", self);
        false
    }

    /// Configures an asynchronous interrupt trigger, which executes the callback on a
    /// separate thread when the interrupt is triggered.
    ///
    /// The callback closure or function pointer is called with a single [`Level`] argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared
    /// when `set_async_interrupt` is called, or when `InputPin` goes out of scope.
    ///
    /// [`clear_async_interrupt`]: #method.clear_async_interrupt
    /// [`Level`]: enum.Level.html
    pub fn set_async_interrupt<C>(&mut self, trigger: Trigger, _callback: C) -> Result<()>
    where
        C: FnMut(Level) + Send + 'static,
    {
        debug!("{:?}.set_async_interrupt({:?}, <callback>)", self, trigger);
        Ok(())
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    pub fn clear_async_interrupt(&mut self) -> Result<()> {
        debug!("{:?}.clear_async_interrupt()", self);
        Ok(())
    }
}

#[cfg(raspi)]
pub type OutputPin = rppal::gpio::OutputPin;
#[cfg(not(raspi))]
#[derive(Debug, PartialEq, Eq)]
pub struct OutputPin(Pin, Level);

#[cfg(not(raspi))]
impl OutputPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Returns `true` if the pin's output state is set to [`Low`].
    ///
    /// [`Low`]: enum.Level.html#variant.Low
    pub fn is_set_low(&self) -> bool {
        self.1 == Level::Low
    }

    /// Returns `true` if the pin's output state is set to [`High`].
    ///
    /// [`High`]: enum.Level.html#variant.High
    pub fn is_set_high(&self) -> bool {
        self.1 == Level::High
    }

    /// Sets the pin's output state.
    pub fn write(&mut self, level: Level) {
        debug!("{:?}.write({:?})", self, level);
        self.1 = level;
    }

    /// Toggles the pin's output state between [`Low`] and [`High`].
    ///
    /// [`Low`]: enum.Level.html#variant.Low
    /// [`High`]: enum.Level.html#variant.High
    pub fn toggle(&mut self) {
        debug!("{:?}.toggle()", self);
        self.1 = !self.1;
    }

    /// Configures a software-based PWM signal.
    pub fn set_pwm(&mut self, period: Duration, pulse_width: Duration) -> Result<()> {
        debug!("{:?}.set_pwm({:?}, {:?})", self, period, pulse_width);
        Ok(())
    }

    /// Configures a software-based PWM signal.
    pub fn set_pwm_frequency(&mut self, frequency: f64, duty_cycle: f64) -> Result<()> {
        debug!("{:?}.set_pwm_frequency({:?}, {:?})", self, frequency, duty_cycle);
        Ok(())
    }

    /// Stops a previously configured software-based PWM signal.
    pub fn clear_pwm(&mut self) -> Result<()> {
        debug!("{:?}.clear_pwm()", self);
        Ok(())
    }
}

/// Pin logic levels.
#[cfg(raspi)]
pub type Level = rppal::gpio::Level;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Low,
    High,
}

#[cfg(not(raspi))]
impl Not for Level {
    type Output = Level;

    fn not(self) -> Level {
        match self {
            Level::Low => Level::High,
            Level::High => Level::Low,
        }
    }
}

/// Built-in pull-up/pull-down resistor states.
#[cfg(raspi)]
pub type PullUpDown = rppal::gpio::PullUpDown;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug)]
pub enum PullUpDown {
    Off,
    PullDown,
    PullUp,
}

/// Interrupt trigger conditions.
#[cfg(raspi)]
pub type Trigger = rppal::gpio::Trigger;
#[cfg(not(raspi))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Trigger {
    Disabled,
    RisingEdge,
    FallingEdge,
    Both,
}

/// Result with `gpio::Error`.
pub type Result<T> = std::result::Result<T, Error>;

/// Errors that can occur when accessing the GPIO peripheral.
#[cfg(raspi)]
pub type Error = rppal::gpio::Error;
#[cfg(not(raspi))]
#[derive(Debug)]
pub enum Error {
    UnknownModel,
    PinNotAvailable(u8),
    PermissionDenied(String),
    Io(io::Error),
    ThreadPanic,
}
