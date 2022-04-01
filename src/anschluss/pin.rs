//! Gpio Pin in verschiedenen Konfigurationen.

use self::pwm::Pwm;
use crate::{
    anschluss::level::Level,
    eingeschränkt::{InvaliderWert, NullBisEins},
    rppal,
};

pub mod input;
pub mod output;
pub mod pwm;

#[derive(Debug)]
pub struct Lager(rppal::gpio::Gpio);

impl Lager {
    #[inline(always)]
    pub fn neu() -> Result<Lager, rppal::gpio::Error> {
        rppal::gpio::Gpio::new().map(Lager)
    }

    /// Reserviere den gewählten Gpio pin.
    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Pin, ReservierenFehler> {
        match self.0.get(pin) {
            Ok(pin) => Ok(Pin(pin)),
            Err(fehler) => Err(ReservierenFehler { pin, fehler }),
        }
    }
}

#[derive(Debug)]
pub struct ReservierenFehler {
    pub pin: u8,
    pub fehler: rppal::gpio::Error,
}

/// Ein Gpio Pin.
#[derive(Debug, PartialEq, Eq)]
pub struct Pin(rppal::gpio::Pin);

impl Pin {
    /// Erhalte die GPIO [Pin] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[inline(always)]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Konsumiere den [Pin], gebe einen [input::Pin] zurück, setze seinen Modus als Input
    /// und deaktiviere die eingebauten pull-up/pull-down Widerstände des [Pin]s.
    #[inline(always)]
    pub fn als_input(self) -> input::Pin {
        input::Pin(self.0.into_input())
    }

    /// Konsumiere den [Pin], gebe einen [input::Pin] zurück, setze seinen Modus auf Input
    /// und aktiviere den eingebauten pull-down Widerstand des [Pin]s.
    ///
    /// Der pull-down Widerstand wird deaktiviert, wenn der [input::Pin] out of scope geht.
    #[inline(always)]
    pub fn als_input_pulldown(self) -> input::Pin {
        input::Pin(self.0.into_input_pulldown())
    }

    /// Konsumiere den [Pin], gebe einen [input::Pin] zurück, setze seinen Modus als Input
    /// und aktiviere den eingebauten pull-up Widerstand des [Pin]s.
    ///
    /// Der pull-up Widerstand wird deaktiviert, wenn der [input::Pin] out of scope geht.
    #[inline(always)]
    pub fn als_input_pullup(self) -> input::Pin {
        input::Pin(self.0.into_input_pullup())
    }

    /// Konsumiere den [Pin], geben einen [output::Pin] und setze seinen Modus auf Output
    /// mit dem übergebenen [Level]
    #[inline(always)]
    pub fn als_output(self, level: Level) -> output::Pin {
        let modus_ändern = match level {
            Level::Low => rppal::gpio::Pin::into_output_low,
            Level::High => rppal::gpio::Pin::into_output_high,
        };
        output::Pin(modus_ändern(self.0))
    }

    #[inline(always)]
    fn pwm_channel(&self) -> Option<rppal::pwm::Channel> {
        match self.0.pin() {
            18 => Some(rppal::pwm::Channel::Pwm0),
            19 => Some(rppal::pwm::Channel::Pwm1),
            _ => None,
        }
    }

    /// Konsumiere den [Pin] und geben einen [pwm::Pin] zurück.
    pub fn als_pwm(self) -> pwm::Pin {
        if let Some(pwm) = self.pwm_channel().and_then(|channel| rppal::pwm::Pwm::new(channel).ok())
        {
            let pwm_konfiguration = || {
                let polarity = pwm.polarity().ok()?;
                let periode = pwm.period().ok()?;
                let frequenz = 1. / periode.as_secs_f64();
                let puls_weite = pwm.pulse_width().ok()?;
                let betriebszyklus =
                    NullBisEins::try_from(puls_weite.as_secs_f64() * frequenz).ok()?;
                Some(pwm::Konfiguration {
                    zeit: pwm::Zeit { frequenz, betriebszyklus },
                    polarität: polarity.into(),
                })
            };
            let konfiguration = pwm_konfiguration();
            pwm::Pin { pin: Pwm::Hardware(pwm, self.0), konfiguration }
        } else {
            // fallback software pwm
            pwm::Pin { pin: Pwm::Software(self.0.into_output()), konfiguration: None }
        }
    }
}
