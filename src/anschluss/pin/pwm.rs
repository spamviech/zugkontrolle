//! Gpio [Pins](Pin) für Pwm konfiguriert.

use nonempty::NonEmpty;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{Anschlüsse, Ergebnis, Reserviere, Serialisiere},
        pin::Pin as EinPin,
        polarität::Polarität,
    },
    rppal::{gpio, pwm},
    util::eingeschränkt::{NichtNegativ, NullBisEins},
};

/// Hard- oder Software-erzeugtes Pwm-Signal. Erlaubt exklusive Steuerung der zugehörigen Pins.
#[allow(variant_size_differences)]
#[derive(Debug)]
pub(in crate::anschluss::pin) enum Pwm {
    /// Hardware-Pwm.
    Hardware(pwm::Pwm, gpio::Pin),
    /// Software-Pwm.
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

impl Eq for Pwm {}

/// Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Konfiguration {
    /// Die [Zeit]-Einstellungen eines Pwm-Pulses.
    pub zeit: Zeit,
    /// Die [Polarität] eines Pwm-Pulses.
    pub polarität: Polarität,
}

/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Zeit {
    /// Frequenz des Pwm-Pulses in Herz.
    pub frequenz: NichtNegativ,
    /// [Fließend](crate::anschluss::polarität::Fließend::Fließend)-Anteil einer Periodendauer.
    pub betriebszyklus: NullBisEins,
}

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug, PartialEq)]
pub struct Pin {
    /// Der Pin.
    pub(in crate::anschluss::pin) pin: Pwm,
    /// Das aktuell anliegende Pwm-Signal
    pub(in crate::anschluss::pin) konfiguration: Option<Konfiguration>,
}

impl Pin {
    /// Erhalte die GPIO [Pin] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    #[must_use]
    pub fn pin(&self) -> u8 {
        match &self.pin {
            Pwm::Hardware(_pwm, pin) => pin.pin(),
            Pwm::Software(pin) => pin.pin(),
        }
    }

    /// Wird Hardware-Pwm verwendet?
    #[must_use]
    pub fn hardware_pwm(&self) -> bool {
        match self.pin {
            Pwm::Hardware(_, _) => true,
            Pwm::Software(_) => false,
        }
    }

    /// Ist der Pwm-Puls aktiv?
    ///
    /// ## Errors
    ///
    /// Fehler beim auslesen des aktuellen Hardware-Pwm Signals.
    pub fn ist_aktiv(&self) -> Result<&Option<Konfiguration>, Fehler> {
        match &self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                let enabled = pwm_channel
                    .is_enabled()
                    .map_err(|fehler| Fehler::Pwm { pin: self.pin(), fehler })?;
                Ok(if enabled { &self.konfiguration } else { &None })
            },
            Pwm::Software(_pin) => Ok(&self.konfiguration),
        }
    }

    /// Aktiviere den Pwm-Puls.
    ///
    /// ## Errors
    ///
    /// Fehler beim einstellen des Pwm-Signals.
    pub fn aktiviere_mit_konfiguration(
        &mut self,
        konfiguration: Konfiguration,
    ) -> Result<(), Fehler> {
        let pin = self.pin();
        match &mut self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                let map_fehler = |fehler| Fehler::Pwm { pin, fehler };
                // update nur, sofern sich Parameter geändert haben.
                if self.konfiguration.as_ref().map(|Konfiguration { polarität, .. }| polarität)
                    != Some(&konfiguration.polarität)
                {
                    pwm_channel.set_polarity(konfiguration.polarität.into()).map_err(map_fehler)?;
                }
                if self.konfiguration.as_ref().map(|Konfiguration { zeit, .. }| zeit)
                    != Some(&konfiguration.zeit)
                {
                    let Zeit { frequenz, betriebszyklus } = konfiguration.zeit;
                    pwm_channel
                        .set_frequency(frequenz.into(), betriebszyklus.into())
                        .map_err(map_fehler)?;
                }
                pwm_channel.enable().map_err(map_fehler)?;
            },
            Pwm::Software(pwm_pin) => {
                let map_fehler = |fehler| Fehler::Gpio { pin, fehler };
                // konfiguration.zeit wird hier kopiert, ein verändern ist demnach kein Problem
                let Zeit { frequenz, mut betriebszyklus } = konfiguration.zeit;
                if konfiguration.polarität == Polarität::Invertiert {
                    // NullBisEins hat eine saturating Add-Implementierung
                    #[allow(clippy::arithmetic_side_effects)]
                    {
                        betriebszyklus = NullBisEins::MAX - betriebszyklus;
                    }
                }
                pwm_pin
                    .set_pwm_frequency(frequenz.into(), betriebszyklus.into())
                    .map_err(map_fehler)?;
            },
        }
        self.konfiguration = Some(konfiguration);
        Ok(())
    }

    /// Deaktiviere den Pwm-Puls.
    ///
    /// ## Errors
    ///
    /// Fehler beim deaktivieren des Pwm-Signals.
    pub fn deaktiviere(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        match &mut self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                pwm_channel.disable().map_err(|fehler| Fehler::Pwm { pin, fehler })?;
            },
            Pwm::Software(pwm_pin) => {
                pwm_pin.clear_pwm().map_err(|fehler| Fehler::Gpio { pin, fehler })?;
            },
        }
        Ok(())
    }
}

/// Fehler bei Interaktion mit einem Pwm-Puls.
#[derive(Debug)]
pub enum Fehler {
    /// Fehler eines GPIO-[Pin]s.
    Gpio {
        /// Der betroffene [Pin].
        pin: u8,
        /// Der aufgetretene Fehler.
        fehler: gpio::Error,
    },
    /// Fehler beim erzeugen des Pwm-Pulses.
    Pwm {
        /// Der betroffene [Pin].
        pin: u8,
        /// Der aufgetretene Fehler.
        fehler: pwm::Error,
    },
}

/// Serialisierbare Informationen einen Pwm-Pins.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Serialisiert(pub u8);

impl Serialisiere<Serialisiert> for Pin {
    fn serialisiere(&self) -> Serialisiert {
        Serialisiert(self.pin())
    }

    fn anschlüsse(self) -> Anschlüsse {
        Anschlüsse {
            pwm_pins: vec![self],
            output_anschlüsse: Vec::new(),
            input_anschlüsse: Vec::new(),
        }
    }
}

impl Reserviere<Pin> for Serialisiert {
    type MoveArg = ();
    type RefArg = ();
    type MutRefArg = ();

    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        Anschlüsse { pwm_pins, output_anschlüsse, input_anschlüsse }: Anschlüsse,
        _move_arg: Self::MoveArg,
        _ref_arg: &Self::RefArg,
        _mut_ref_arg: &mut Self::MutRefArg,
    ) -> Ergebnis<Pin> {
        let (mut gesucht, andere): (Vec<_>, Vec<_>) =
            pwm_pins.into_iter().partition(|pin| pin.serialisiere() == self);
        let vorhandener_anschluss = gesucht.pop();
        gesucht.extend(andere);
        let anschlüsse = Anschlüsse { pwm_pins: gesucht, output_anschlüsse, input_anschlüsse };
        if let Some(anschluss) = vorhandener_anschluss {
            Ergebnis::Wert { anschluss, anschlüsse }
        } else {
            match lager.pin.reserviere_pin(self.0).map(EinPin::als_pwm) {
                Ok(anschluss) => Ergebnis::Wert { anschluss, anschlüsse },
                Err(fehler) => {
                    Ergebnis::Fehler { fehler: NonEmpty::singleton(fehler.into()), anschlüsse }
                },
            }
        }
    }
}
