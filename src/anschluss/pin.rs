//! Gpio Pin in verschiedenen Konfigurationen.

#[cfg(raspi)]
use rppal::{gpio, pwm};

/// Ein Gpio Pin.
#[derive(Debug)]
pub struct Pin(#[cfg(raspi)] gpio::Pin);
impl Pin {
    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and disables the pin’s
    /// built-in pull-up/pull-down resistors.
    pub fn into_input(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input(),
        )
    }

    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and enables the pin’s
    /// built-in pull-down resistor.
    ///
    /// The pull-down resistor is disabled when InputPin goes out of scope if reset_on_drop is set
    /// to true (default).
    pub fn into_input_pulldown(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input_pulldown(),
        )
    }

    /// Consumes the Pin, returns an InputPin, sets its mode to Input, and enables the pin’s
    /// built-in pull-up resistor.
    ///
    /// The pull-up resistor is disabled when InputPin goes out of scope if reset_on_drop is set to
    /// true (default).
    pub fn into_input_pullup(self) -> InputPin {
        InputPin(
            #[cfg(raspi)]
            self.0.into_input_pullup(),
        )
    }

    ///Consumes the Pin, returns an OutputPin and sets its mode to Output.
    pub fn into_output(self) -> OutputPin {
        OutputPin(
            #[cfg(raspi)]
            self.0.into_output(),
        )
    }

    #[cfg(raspi)]
    fn pwm_channel(&self) -> Option<pwm::Channel> {
        match self.0.pin() {
            18 => Some(pwm::Channel::Pwm0),
            19 => Some(pwm::Channel::Pwm1),
            _ => None,
        }
    }

    pub fn into_pwm(self) -> PwmPin {
        PwmPin(
            #[cfg(raspi)]
            {
                if let Some(channel) =
                    self.pwm_channel().and_then(|channel| rppal::pwm::Pwm::new(channel).ok())
                {
                    // hardware pwm possible
                    Pwm::Hardware(channel)
                } else {
                    // fallback software pwm
                    Pwm::Software(self.0.into_output())
                }
            },
        )
    }
}
/// Ein Gpio Pin konfiguriert für Input.
#[derive(Debug)]
pub struct InputPin(#[cfg(raspi)] gpio::InputPin);
/// Ein Gpio Pin konfiguriert für Output.
#[derive(Debug)]
pub struct OutputPin(#[cfg(raspi)] gpio::OutputPin);
/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug)]
pub struct PwmPin(#[cfg(raspi)] Pwm);
#[cfg(raspi)]
enum Pwm {
    Hardware(pwm::Pwm),
    Software(gpio::OutputPin),
}
