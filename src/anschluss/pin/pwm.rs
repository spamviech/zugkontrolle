//! Gpio Pins für Pwm konfiguriert.

use std::time::Duration;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        polarität::Polarität,
        InputAnschluss, OutputAnschluss,
    },
    rppal::{gpio, pwm},
};

#[allow(variant_size_differences)]
#[derive(Debug)]
pub(in crate::anschluss::pin) enum Pwm {
    Hardware(pwm::Pwm, gpio::Pin),
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
    pub zeit: Zeit,
    pub polarität: Polarität,
}

impl Konfiguration {
    /// Smart-Konstruktor um invalide Konfigurationen zu verbieten.
    ///
    /// [Zeit::valide] muss `true` sein.
    pub fn new(zeit: Zeit, polarität: Polarität) -> Option<Konfiguration> {
        let konfiguration = Konfiguration { zeit, polarität };
        if konfiguration.valide() {
            Some(konfiguration)
        } else {
            None
        }
    }

    /// Nicht alle Zeit-Werte erlauben einen sinnvollen Pwm-Puls.
    ///
    /// Es muss gelten:
    /// - `period >= pulse_width`
    /// - `0 <= duty_cycle <= 1`
    #[inline(always)]
    pub fn valide(&self) -> bool {
        self.zeit.valide()
    }
}

/// Zeit-Einstellung eines Pwm-Pulses.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Zeit {
    /// Periodendauer und Pulsweite.
    Periode { periode: Duration, puls_weite: Duration },
    /// Frequenz (in Herz) und Duty-cycle (\[0,1\]) als Prozentwert.
    Frequenz { frequenz: f64, betriebszyklus: f64 },
}

impl Zeit {
    /// Nicht alle Zeit-Werte erlauben einen sinnvollen Pwm-Puls.
    ///
    /// Es muss gelten:
    /// - `period >= pulse_width`
    /// - `0 <= duty_cycle <= 1`
    pub fn valide(&self) -> bool {
        match self {
            Zeit::Periode { periode, puls_weite } => puls_weite <= periode,
            Zeit::Frequenz { frequenz: _, betriebszyklus } => {
                0. <= *betriebszyklus && *betriebszyklus <= 1.
            },
        }
    }
}

/// Ein Gpio Pin konfiguriert für Pwm.
#[derive(Debug, PartialEq)]
pub struct Pin {
    pub(in crate::anschluss::pin) pin: Pwm,
    pub(in crate::anschluss::pin) konfiguration: Option<Konfiguration>,
}

impl Pin {
    /// Erhalte die GPIO [Pin] Nummer.
    ///
    /// Pins werden über ihre BCM Nummer angesprochen, nicht ihre physische Position.
    pub fn pin(&self) -> u8 {
        match &self.pin {
            Pwm::Hardware(_pwm, pin) => pin.pin(),
            Pwm::Software(pin) => pin.pin(),
        }
    }

    /// Wird Hardware-Pwm verwendet?
    pub fn hardware_pwm(&self) -> bool {
        match self.pin {
            Pwm::Hardware(_, _) => true,
            Pwm::Software(_) => false,
        }
    }

    /// Ist der Pwm-Puls aktiv?
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
                    match konfiguration.zeit {
                        Zeit::Periode { periode, puls_weite } => {
                            pwm_channel.set_pulse_width(Duration::ZERO).map_err(map_fehler)?;
                            pwm_channel.set_period(periode).map_err(map_fehler)?;
                            pwm_channel.set_pulse_width(puls_weite).map_err(map_fehler)?;
                        },
                        Zeit::Frequenz { frequenz, betriebszyklus } => {
                            pwm_channel
                                .set_frequency(frequenz, betriebszyklus)
                                .map_err(map_fehler)?;
                        },
                    }
                }
                pwm_channel.enable().map_err(map_fehler)?;
            },
            Pwm::Software(pwm_pin) => {
                let map_fehler = |fehler| Fehler::Gpio { pin, fehler };
                // konfiguration.zeit wird hier kopiert, ein verändern ist demnach kein Problem
                match konfiguration.zeit {
                    Zeit::Periode { periode, mut puls_weite } => {
                        if konfiguration.polarität == Polarität::Invertiert {
                            puls_weite = periode - puls_weite;
                        }
                        pwm_pin.set_pwm(periode, puls_weite).map_err(map_fehler)?;
                    },
                    Zeit::Frequenz { frequenz, mut betriebszyklus } => {
                        if konfiguration.polarität == Polarität::Invertiert {
                            betriebszyklus = 1. - betriebszyklus;
                        }
                        pwm_pin.set_pwm_frequency(frequenz, betriebszyklus).map_err(map_fehler)?;
                    },
                }
            },
        }
        self.konfiguration = Some(konfiguration);
        Ok(())
    }

    /// Deaktiviere den Pwm-Puls
    pub fn deaktiviere(&mut self) -> Result<(), Fehler> {
        let pin = self.pin();
        Ok(match &mut self.pin {
            Pwm::Hardware(pwm_channel, _pin) => {
                pwm_channel.disable().map_err(|fehler| Fehler::Pwm { pin, fehler })?;
            },
            Pwm::Software(pwm_pin) => {
                pwm_pin.clear_pwm().map_err(|fehler| Fehler::Gpio { pin, fehler })?;
            },
        })
    }
}

#[derive(Debug)]
pub enum Fehler {
    Gpio { pin: u8, fehler: gpio::Error },
    Pwm { pin: u8, fehler: pwm::Error },
    InvalideKonfiguration { pin: u8, konfiguration: Konfiguration },
}

/// Serealisierbare Informationen einen Pwm-Pins.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Serialisiert(pub u8);
impl Serialisiere for Pin {
    type Serialisiert = Serialisiert;

    fn serialisiere(&self) -> Serialisiert {
        Serialisiert(self.pin())
    }

    fn anschlüsse(self) -> (Vec<Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        (vec![self], Vec::new(), Vec::new())
    }
}
impl Reserviere<Pin> for Serialisiert {
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<Pin>,
        output_nicht_benötigt: Vec<OutputAnschluss>,
        input_nicht_benötigt: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Pin> {
        let (mut gesucht, pwm_nicht_benötigt): (Vec<_>, Vec<_>) =
            pwm_pins.into_iter().partition(|pin| pin.serialisiere() == self);
        if let Some(anschluss) = gesucht.pop() {
            Ok(Reserviert {
                anschluss,
                pwm_nicht_benötigt,
                output_nicht_benötigt,
                input_nicht_benötigt,
            })
        } else {
            match lager.pin.reserviere_pin(self.0).map(super::Pin::als_pwm) {
                Ok(anschluss) => Ok(Reserviert {
                    anschluss,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }),
                Err(error) => Err(de_serialisieren::Fehler {
                    fehler: error.into(),
                    pwm_pins: pwm_nicht_benötigt,
                    output_anschlüsse: output_nicht_benötigt,
                    input_anschlüsse: input_nicht_benötigt,
                }),
            }
        }
    }
}
