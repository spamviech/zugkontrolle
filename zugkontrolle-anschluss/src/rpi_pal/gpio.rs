//! Low level Steuerung von Gpio Pins.

// Mit raspi-feature wird das rpi_pal-crate verwendet.
#![cfg_attr(feature = "raspi", allow(clippy::pub_use))]
// Dokumentation ist (modulo backticks) copy+paste vom rpi_pal-crate.
#![cfg_attr(not(feature = "raspi"), allow(clippy::missing_errors_doc))]

#[cfg(not(feature = "raspi"))]
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
    io,
    ops::Not,
    time::Duration,
};

#[cfg(not(feature = "raspi"))]
use log::{debug, error};
#[cfg(not(feature = "raspi"))]
use parking_lot::MappedMutexGuard;

#[cfg(not(feature = "raspi"))]
use crate::rpi_pal::LazyMutex;

#[cfg(not(feature = "raspi"))]
/// Set mit den aktuell verfügbaren Pins.
#[derive(Debug)]
struct GpioStore {
    /// Noch verfügbare Pins.
    pins: HashSet<u8>,
}

#[cfg(not(feature = "raspi"))]
/// Kleinste unterstützte Pin-Zahl.
const MIN_PIN: u8 = 0;
#[cfg(not(feature = "raspi"))]
/// Größte unterstützte Pin-Zahl.
const MAX_PIN: u8 = 27;

#[cfg(not(feature = "raspi"))]
/// Globales Singleton mit den aktuell verfügbaren Pins.
static GPIO: LazyMutex<GpioStore> =
    LazyMutex::neu(|| GpioStore { pins: (MIN_PIN..=MAX_PIN).collect() });

#[cfg(not(feature = "raspi"))]
impl GpioStore {
    /// Erhalte Zugriff auf das [`Singleton`](GPIO) mit den aktuell verfügbaren Pins.
    ///
    /// Der Aufruf blockiert, bis der Zugriff erhalten wurde.
    fn lock_static<'t>() -> MappedMutexGuard<'t, GpioStore> {
        GPIO.lock()
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Gpio;
#[cfg(not(feature = "raspi"))]
/// Provides access to the Raspberry Pi’s GPIO peripheral.
#[derive(Debug, Clone)]
#[expect(missing_copy_implementations)]
pub struct Gpio;

#[cfg(not(feature = "raspi"))]
impl Gpio {
    /// Constructs a new Gpio.
    pub fn new() -> Result<Gpio> {
        Ok(Gpio)
    }

    /// Returns a [`Pin`] for the specified BCM GPIO pin number.
    ///
    /// Retrieving a GPIO pin grants access to the pin through an owned [`Pin`] instance.
    /// If the pin is already in use, or the GPIO peripheral doesn't expose a pin with the
    /// specified number, get returns Err([`Error::PinNotAvailable`]). After a [`Pin`]
    /// (or a derived [`InputPin`], [`OutputPin`]) goes out of scope, it
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
pub use rpi_pal::gpio::Pin;
#[cfg(not(feature = "raspi"))]
/// Unconfigured GPIO pin.
#[derive(Debug, PartialEq, Eq)]
pub struct Pin(u8);

#[cfg(not(feature = "raspi"))]
impl Drop for Pin {
    fn drop(&mut self) {
        if !GpioStore::lock_static().pins.insert(self.0) {
            error!("Dropped pin was still available: {}", self.0);
        }
    }
}

#[cfg(not(feature = "raspi"))]
impl Pin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0
    }

    /// Consumes the Pin and returns an [`InputPin`]. Sets the mode to [`Mode::Input`]
    /// and disables the pin's built-in pull-up/pull-down resistors.
    #[must_use]
    pub fn into_input(self) -> InputPin {
        InputPin(self, Bias::Off)
    }

    /// Consumes the Pin and returns an [`InputPin`]. Sets the mode to [`Mode::Input`]
    /// and enables the pin's built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when `InputPin` goes out of scope if `reset_on_drop`
    /// is set to true (default).
    #[must_use]
    pub fn into_input_pulldown(self) -> InputPin {
        InputPin(self, Bias::PullDown)
    }

    /// Consumes the Pin and returns an [`InputPin`]. Sets the mode to [`Mode::Input`]
    /// and enables the pin's built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when `InputPin` goes out of scope if `reset_on_drop`
    /// is set to true (default).
    #[must_use]
    pub fn into_input_pullup(self) -> InputPin {
        InputPin(self, Bias::PullUp)
    }

    /// Consumes the Pin and returns an [`OutputPin`]. Sets the mode to [`Mode::Output`]
    /// and leaves the logic level unchanged.
    #[must_use]
    pub fn into_output(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the Pin and returns an [`OutputPin`]. Changes the logic level to
    /// [`Level::Low`] and then sets the mode to [`Mode::Output`].
    #[must_use]
    pub fn into_output_low(self) -> OutputPin {
        OutputPin(self, Level::Low)
    }

    /// Consumes the [`Pin`] and returns an [`OutputPin`]. Changes the logic level to
    /// [`Level::High`] and then sets the mode to [`Mode::Output`].
    #[must_use]
    pub fn into_output_high(self) -> OutputPin {
        OutputPin(self, Level::High)
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::InputPin;
#[cfg(not(feature = "raspi"))]
#[derive(Debug)]
/// GPIO pin configured as input.
pub struct InputPin(Pin, #[expect(dead_code)] Bias);

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
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Reads the pin's logic level.
    #[must_use]
    pub fn read(&self) -> Level {
        debug!("{self:?}.read()");
        Level::Low
    }

    /// Reads the pin's logic level, and returns true if it's set to [`Level::Low`].
    #[must_use]
    pub fn is_low(&self) -> bool {
        debug!("{self:?}.is_low()");
        true
    }

    /// Reads the pin's logic level, and returns true if it's set to [`Level::High`].
    #[must_use]
    pub fn is_high(&self) -> bool {
        debug!("{self:?}.is_high()");
        false
    }

    /// Configures an asynchronous interrupt trigger, which executes the callback on a
    /// separate thread when the interrupt is triggered.
    ///
    /// An optional debounce duration can be specified to filter unwanted input noise.
    ///
    /// The callback closure or function pointer is called with a single [`Event`] argument.
    ///
    /// Any previously configured (a)synchronous interrupt triggers for this pin are cleared
    /// when `set_async_interrupt` is called, or when `InputPin` goes out of scope.
    ///
    /// [`clear_async_interrupt`]: #method.clear_async_interrupt
    /// [`Event`]: struct.Event.html
    pub fn set_async_interrupt<C>(
        &mut self,
        trigger: Trigger,
        _debounce: Option<Duration>,
        _callback: C,
    ) -> Result<()>
    where
        C: FnMut(Event) + Send + 'static,
    {
        debug!("{self:?}.set_async_interrupt({trigger:?}, <callback>)");
        Ok(())
    }

    /// Removes a previously configured asynchronous interrupt trigger.
    pub fn clear_async_interrupt(&mut self) -> Result<()> {
        debug!("{self:?}.clear_async_interrupt()");
        Ok(())
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::OutputPin;
#[cfg(not(feature = "raspi"))]
#[derive(Debug, PartialEq, Eq)]
/// GPIO pin configured as output.
pub struct OutputPin(Pin, Level);

#[cfg(not(feature = "raspi"))]
impl OutputPin {
    /// Returns the GPIO pin number.
    ///
    /// Pins are addressed by their BCM numbers, rather than their physical location.
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Returns true if the pin's output state is set to [`Level::Low`].
    #[must_use]
    pub fn is_set_low(&self) -> bool {
        self.1 == Level::Low
    }

    /// Returns true if the pin's output state is set to [`Level::High`].
    #[must_use]
    pub fn is_set_high(&self) -> bool {
        self.1 == Level::High
    }

    /// Sets the pin's output state.
    pub fn write(&mut self, level: Level) {
        debug!("{self:?}.write({level:?})");
        self.1 = level;
    }

    /// Toggles the pin's output state between [`Level::Low`] and [`Level::High`].
    pub fn toggle(&mut self) {
        debug!("{self:?}.toggle()");
        self.1 = !self.1;
    }

    /// Configures a software-based PWM signal.
    pub fn set_pwm(&mut self, period: Duration, pulse_width: Duration) -> Result<()> {
        debug!("{self:?}.set_pwm({period:?}, {pulse_width:?})");
        Ok(())
    }

    /// Configures a software-based PWM signal.
    pub fn set_pwm_frequency(&mut self, frequency: f64, duty_cycle: f64) -> Result<()> {
        debug!("{self:?}.set_pwm_frequency({frequency:?}, {duty_cycle:?})");
        Ok(())
    }

    /// Stops a previously configured software-based PWM signal.
    pub fn clear_pwm(&mut self) -> Result<()> {
        debug!("{self:?}.clear_pwm()");
        Ok(())
    }
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Level;
#[cfg(not(feature = "raspi"))]
/// Pin logic levels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[expect(missing_docs)]
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
pub use rpi_pal::gpio::Bias;
#[cfg(not(feature = "raspi"))]
/// Built-in pull-up/pull-down resistor states.
#[derive(Clone, Copy, Debug)]
#[expect(missing_docs)]
pub enum Bias {
    Off,
    PullDown,
    PullUp,
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Trigger;
#[cfg(not(feature = "raspi"))]
/// Interrupt trigger conditions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[expect(missing_docs)]
pub enum Trigger {
    Disabled,
    RisingEdge,
    FallingEdge,
    Both,
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Event;
#[cfg(not(feature = "raspi"))]
/// Interrupt trigger event.
#[derive(Debug, Copy, Clone)]
pub struct Event {
    /// Best estimate of time of event occurrence, measured in elapsed time since the system was booted.
    pub timestamp: Duration,
    /// Sequence number for this event in the sequence of interrupt trigger events for this pin.
    pub seqno: u32,
    /// Interrupt trigger. This will contain either [`Trigger::RisingEdge`] or [`Trigger::FallingEdge`].
    pub trigger: Trigger,
}

#[cfg(not(feature = "raspi"))]
impl Default for Event {
    fn default() -> Self {
        Self { timestamp: Duration::default(), seqno: 0, trigger: Trigger::Both }
    }
}

#[expect(clippy::absolute_paths, reason = "disambiguate mit self::Result")]
/// Result with [`Error`].
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Error;
#[cfg(not(feature = "raspi"))]
/// Errors that can occur when accessing the GPIO peripheral.
#[derive(Debug)]
#[expect(missing_docs)]
pub enum Error {
    UnknownModel,
    PinNotAvailable(u8),
    PermissionDenied(String),
    Io(io::Error),
    ThreadPanic,
}

#[cfg(feature = "raspi")]
#[doc(inline)]
pub use rpi_pal::gpio::Mode;
#[cfg(not(feature = "raspi"))]
/// Pin modes.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
#[repr(u8)]
#[expect(missing_docs)]
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
impl Display for Mode {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Mode::Input => write!(formatter, "In"),
            Mode::Output => write!(formatter, "Out"),
            Mode::Alt0 => write!(formatter, "Alt0"),
            Mode::Alt1 => write!(formatter, "Alt1"),
            Mode::Alt2 => write!(formatter, "Alt2"),
            Mode::Alt3 => write!(formatter, "Alt3"),
            Mode::Alt4 => write!(formatter, "Alt4"),
            Mode::Alt5 => write!(formatter, "Alt5"),
            Mode::Alt6 => write!(formatter, "Alt6"),
            Mode::Alt7 => write!(formatter, "Alt7"),
            Mode::Alt8 => write!(formatter, "Alt8"),
        }
    }
}
