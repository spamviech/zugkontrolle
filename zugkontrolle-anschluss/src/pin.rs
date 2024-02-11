//! Gpio [`Pin`] in verschiedenen Konfigurationen.

use zugkontrolle_util::eingeschränkt::{NichtNegativ, NullBisEins};

use crate::{level::Level, pwm::Pwm, rppal};

pub mod input;
pub mod output;
pub mod pwm;

/// Verwalten aller nicht verwendeten [`Pins`](Pin).
#[derive(Debug)]
pub struct Lager(rppal::gpio::Gpio);

impl Lager {
    /// Erstelle ein neues [`Lager`].
    ///
    /// ## Errors
    ///
    /// Zugriff auf Gpio-Pins nicht möglich.
    pub fn neu() -> Result<Lager, rppal::gpio::Error> {
        rppal::gpio::Gpio::new().map(Lager)
    }

    /// Reserviere den gewählten Gpio [`Pin`].
    ///
    /// ## Errors
    ///
    /// Der [`Pin`] konnte nicht reserviert werden, z.B. weil er bereits verwendet wird.
    pub fn reserviere_pin(&mut self, pin: u8) -> Result<Pin, ReservierenFehler> {
        match self.0.get(pin) {
            Ok(pin) => Ok(Pin(pin)),
            Err(fehler) => Err(ReservierenFehler { pin, fehler }),
        }
    }
}

/// Fehler beim reservieren eines [`Pins`](Pin).
#[derive(Debug)]
pub struct ReservierenFehler {
    /// Der gewünschte [`Pin`].
    pub pin: u8,
    /// Der aufgetretene Fehler.
    pub fehler: rppal::gpio::Error,
}

/// Ein Gpio Pin.
#[derive(Debug, PartialEq, Eq)]
pub struct Pin(rppal::gpio::Pin);

impl Pin {
    /// Erhalte die GPIO [`Pin`] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[must_use]
    pub fn pin(&self) -> u8 {
        self.0.pin()
    }

    /// Konsumiere den [`Pin`], gebe einen [`input::Pin`] zurück, setze seinen Modus als Input
    /// und deaktiviere die eingebauten pull-up/pull-down Widerstände des [`Pin`]s.
    #[must_use]
    pub fn als_input(self) -> input::Pin {
        input::Pin(self.0.into_input())
    }

    /// Konsumiere den [`Pin`], gebe einen [`input::Pin`] zurück, setze seinen Modus auf Input
    /// und aktiviere den eingebauten pull-down Widerstand des [`Pin`]s.
    ///
    /// Der pull-down Widerstand wird deaktiviert, wenn der [`input::Pin`] out of scope geht.
    #[must_use]
    pub fn als_input_pulldown(self) -> input::Pin {
        input::Pin(self.0.into_input_pulldown())
    }

    /// Konsumiere den [`Pin`], gebe einen [`input::Pin`] zurück, setze seinen Modus als Input
    /// und aktiviere den eingebauten pull-up Widerstand des [`Pin`]s.
    ///
    /// Der pull-up Widerstand wird deaktiviert, wenn der [`input::Pin`] out of scope geht.
    #[must_use]
    pub fn als_input_pullup(self) -> input::Pin {
        input::Pin(self.0.into_input_pullup())
    }

    /// Konsumiere den [`Pin`], geben einen [`output::Pin`] und setze seinen Modus auf Output
    /// mit dem übergebenen [`Level`]
    #[must_use]
    pub fn als_output(self, level: Level) -> output::Pin {
        let modus_ändern = match level {
            Level::Low => rppal::gpio::Pin::into_output_low,
            Level::High => rppal::gpio::Pin::into_output_high,
        };
        output::Pin(modus_ändern(self.0))
    }

    /// Erhalte den zum Pin gehörigen [`pwm::Channel`].
    fn pwm_channel(&self) -> Option<rppal::pwm::Channel> {
        match self.0.pin() {
            18 => Some(rppal::pwm::Channel::Pwm0),
            19 => Some(rppal::pwm::Channel::Pwm1),
            _ => None,
        }
    }

    /// Konsumiere den [`Pin`] und geben einen [`pwm::Pin`] zurück.
    #[must_use]
    pub fn als_pwm(self) -> pwm::Pin {
        if let Some(pwm) = self.pwm_channel().and_then(|channel| rppal::pwm::Pwm::new(channel).ok())
        {
            let konfiguration = pwm
                .polarity()
                .and_then(|polarity| {
                    pwm.frequency().and_then(|frequency| {
                        pwm.duty_cycle().map(|duty_cycle| (polarity, frequency, duty_cycle))
                    })
                })
                .ok()
                .and_then(|(polarity, frequency, duty_cycle)| {
                    // Vermutlich sind beide Werte in Ordnung,
                    // die Funktion wird aber nicht häufig aufgerufen.
                    // Die Performance-Kosten durch das zusätzliche Überprüfen spielen daher keine Rolle.
                    let frequenz = NichtNegativ::try_from(frequency).ok()?;
                    let betriebszyklus = NullBisEins::try_from(duty_cycle).ok()?;
                    Some(pwm::Konfiguration {
                        zeit: pwm::Zeit { frequenz, betriebszyklus },
                        polarität: polarity.into(),
                    })
                });
            pwm::Pin { pin: Pwm::Hardware(pwm, self.0), konfiguration }
        } else {
            // fallback software pwm
            pwm::Pin { pin: Pwm::Software(self.0.into_output()), konfiguration: None }
        }
    }
}
