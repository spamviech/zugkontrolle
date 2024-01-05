//! Low level Steuerung von Gpio Pins.

#[cfg(not(feature = "raspi"))]
use std::{collections::HashSet, io, ops::Not, time::Duration};

#[cfg(not(feature = "raspi"))]
use log::{debug, error};
#[cfg(not(feature = "raspi"))]
use parking_lot::MappedMutexGuard;

#[cfg(not(feature = "raspi"))]
use crate::rppal::LazyMutex;

#[cfg(not(feature = "raspi"))]
#[derive(Debug)]
struct GpioStore {
    pins: HashSet<u8>,
}

#[cfg(not(feature = "raspi"))]
const MIN_PIN: u8 = 0;
#[cfg(not(feature = "raspi"))]
const MAX_PIN: u8 = 27;

#[cfg(not(feature = "raspi"))]
static GPIO: LazyMutex<GpioStore> =
    LazyMutex::neu(|| GpioStore { pins: (MIN_PIN..=MAX_PIN).collect() });

#[cfg(not(feature = "raspi"))]
impl GpioStore {
    fn lock_static<'t>() -> MappedMutexGuard<'t, GpioStore> {
        GPIO.lock()
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Gpio;
#[cfg(not(feature = "raspi"))]
/// Provides access to the Raspberry Pi’s GPIO peripheral.
#[derive(Debug, Clone)]
#[allow(missing_copy_implementations)]
pub struct Gpio;

#[cfg(not(feature = "raspi"))]
impl Gpio {
    /// Constructs a new Gpio.
    pub fn new() -> Result<Gpio> {
        Ok(Gpio)
    }

    /// Returns a [Pin] for the specified BCM GPIO pin number.
    ///
    /// Retrieving a GPIO pin grants access to the pin through an owned [Pin] instance.
    /// If the pin is already in use, or the GPIO peripheral doesn't expose a pin with the
    /// specified number, get returns Err([Error::PinNotAvailable]). After a [Pin]
    /// (or a derived [InputPin], [OutputPin]) goes out of scope, it
    /// can be retrieved again through another get call.
    pub fn get(&self, pin: u8) -> Result<Pin> {
        if GpioStore::lock_static().pins.remove(&pin) {
            Ok(Pin(pin))
        } else {
            Err(Error::PinNotAvailable(pin))
        }
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Pin;
#[cfg(not(feature = "raspi"))]
/// Unconfigured GPIO pin.
#[derive(Debug, PartialEq, Eq)]
pub struct Pin(u8);

#[cfg(not(feature = "raspi"))]
impl Drop for Pin {
    fn drop(&mut self) {
        if !GpioStore::lock_static().pins.insert(self.0) {
            error!("Dropped pin was still available: {}", self.0)
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    pub fn pin(&self) -> u8 {
        self.0
    }

    /// Consumes the Pin and returns an [InputPin]. Sets the mode to [Mode::Input]
    /// and disables the pin's built-in pull-up/pull-down resistors.
    pub fn into_input(self) -> InputPin {
        InputPin(self, Bias::Off)
    }

    /// Consumes the Pin and returns an [InputPin]. Sets the mode to [Mode::Input]
    /// and enables the pin's built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when InputPin goes out of scope if `reset_on_drop`
    /// is set to true (default).
    pub fn into_input_pulldown(self) -> InputPin {
        InputPin(self, Bias::PullDown)
    }

    /// Consumes the Pin and returns an [InputPin]. Sets the mode to [Mode::Input]
    /// and enables the pin's built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when InputPin goes out of scope if `reset_on_drop`
    /// is set to true (default).
    pub fn into_input_pullup(self) -> InputPin {
        InputPin(self, Bias::PullUp)
    }

    /// Consumes the Pin and returns an [OutputPin]. Sets the mode to [Mode::Output]
    /// and leaves the logic level unchanged.
    pub fn into_output(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the Pin and returns an [OutputPin]. Changes the logic level to
    /// [Level::Low] and then sets the mode to [Mode::Output].
    pub fn into_output_low(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the [Pin] and returns an [OutputPin]. Changes the logic level to
    /// [Level::High] and then sets the mode to [Mode::Output].
    pub fn into_output_high(self) -> OutputPin {
        OutputPin(self, Level::High)
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::InputPin;
#[cfg(not(feature = "raspi"))]
#[derive(Debug)]
/// GPIO pin configured as input.
pub struct InputPin(Pin, Bias);

#[cfg(not(feature = "raspi"))]
impl PartialEq for InputPin {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[cfg(not(feature = "raspi"))]
impl Eq for InputPin {}

#[cfg(not(feature = "raspi"))]
impl InputPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.

    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Reads the pin's logic level.
    pub fn read(&self) -> Level {
        debug!("{:?}.read()", self);
        Level::Low
    }

    /// Reads the pin's logic level, and returns true if it's set to [Level::Low].
    pub fn is_low(&self) -> bool {
        debug!("{:?}.is_low()", self);
        true
    }

    /// Reads the pin's logic level, and returns true if it's set to [Level::High].
    pub fn is_high(&self) -> bool {
        debug!("{:?}.is_high()", self);
        false
    }

    /// Configures an asynchronous interrupt trigger, which executes the callback on a
    /// separate thread when the interrupt is triggered.
    ///
    /// The callback closure or function pointer is called with a single [Level] argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared
    /// when set_async_interrupt is called, or when InputPin goes out of scope.
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

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::OutputPin;
#[cfg(not(feature = "raspi"))]
#[derive(Debug, PartialEq, Eq)]
/// GPIO pin configured as output.
pub struct OutputPin(Pin, Level);

#[cfg(not(feature = "raspi"))]
impl OutputPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.

    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Returns true if the pin's output state is set to [Level::Low].
    pub fn is_set_low(&self) -> bool {
        self.1 == Level::Low
    }

    /// Returns true if the pin's output state is set to [Level::High].
    pub fn is_set_high(&self) -> bool {
        self.1 == Level::High
    }

    /// Sets the pin's output state.
    pub fn write(&mut self, level: Level) {
        debug!("{:?}.write({:?})", self, level);
        self.1 = level;
    }

    /// Toggles the pin's output state between [Level::Low] and [Level::High].
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

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Level;
#[cfg(not(feature = "raspi"))]
/// Pin logic levels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Level {
    Low,
    High,
}

#[cfg(not(feature = "raspi"))]
impl Not for Level {
    type Output = Level;

    fn not(self) -> Level {
        match self {
            Level::Low => Level::High,
            Level::High => Level::Low,
        }
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Bias;
#[cfg(not(feature = "raspi"))]
/// Built-in pull-up/pull-down resistor states.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub enum Bias {
    Off,
    PullDown,
    PullUp,
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Trigger;
#[cfg(not(feature = "raspi"))]
/// Interrupt trigger conditions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum Trigger {
    Disabled,
    RisingEdge,
    FallingEdge,
    Both,
}

/// Result with [Error].
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Error;
#[cfg(not(feature = "raspi"))]
/// Errors that can occur when accessing the GPIO peripheral.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
    UnknownModel,
    PinNotAvailable(u8),
    PermissionDenied(String),
    Io(io::Error),
    ThreadPanic,
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use ::rppal::gpio::Mode;
#[cfg(not(feature = "raspi"))]
/// Pin modes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Mode {
    Input,
    Output,
    Alt0,
    Alt1,
    Alt2,
    Alt3,
    Alt4,
    Alt5,
    Alt6,
    Alt7,
    Alt8,
}

#[cfg(not(feature = "raspi"))]
impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Mode::Input => write!(f, "In"),
            Mode::Output => write!(f, "Out"),
            Mode::Alt0 => write!(f, "Alt0"),
            Mode::Alt1 => write!(f, "Alt1"),
            Mode::Alt2 => write!(f, "Alt2"),
            Mode::Alt3 => write!(f, "Alt3"),
            Mode::Alt4 => write!(f, "Alt4"),
            Mode::Alt5 => write!(f, "Alt5"),
            Mode::Alt6 => write!(f, "Alt6"),
            Mode::Alt7 => write!(f, "Alt7"),
            Mode::Alt8 => write!(f, "Alt8"),
        }
    }
}
