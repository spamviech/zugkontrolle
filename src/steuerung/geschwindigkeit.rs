//! Einstellen der Geschwindigkeit.

use std::usize;
use std::{thread::sleep, time::Duration};

use non_empty_vec::NonEmpty;

use crate::anschluss::{self, pwm, Fließend, OutputAnschluss, Polarity};

#[derive(Debug)]
pub struct Geschwindigkeit<Leiter> {
    pub name: String,
    pub leiter: Leiter,
}

impl pwm::Pin {
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        faktor: f64,
        polarity: Polarity,
    ) -> Result<(), pwm::Error> {
        debug_assert!(
            0. < faktor && faktor <= 1.,
            "Faktor muss zwischen 0 und 1 liegen: {}",
            faktor
        );
        self.enable_with_config(pwm::Config {
            polarity,
            time: pwm::Time::Frequency {
                frequency: PWM_FREQUENZ,
                duty_cycle: faktor * wert as f64 / u8::MAX as f64,
            },
        })
    }
}
fn geschwindigkeit_ks(
    geschwindigkeit: &mut NonEmpty<OutputAnschluss>,
    letzter_wert: &mut usize,
    wert: u8,
) -> Result<(), Error> {
    let wert_usize = wert as usize;
    let length = geschwindigkeit.len().get();
    if wert_usize > length {
        return Err(Error::ZuWenigAnschlüsse { benötigt: wert, vorhanden: length })
    }
    // aktuellen Anschluss ausstellen
    geschwindigkeit[*letzter_wert].einstellen(Fließend::Gesperrt)?;
    // neuen anstellen
    *letzter_wert = wert_usize;
    geschwindigkeit[wert_usize].einstellen(Fließend::Gesperrt)?;
    Ok(())
}

#[derive(Debug)]
pub enum Mittelleiter {
    Pwm {
        pin: pwm::Pin,
        polarität: Polarity,
    },
    KonstanteSpannung {
        geschwindigkeit: NonEmpty<OutputAnschluss>,
        letzter_wert: usize,
        umdrehen: OutputAnschluss,
    },
}

// TODO als Zugtyp-Eigenschaft?
const STOPPZEIT: Duration = Duration::from_millis(500);
const PWM_FREQUENZ: f64 = 50.;
// TODO Zugtyp-Eigenschaft, wenn Mittelleiter gewählt
// oder allgemein (max_duty_cycle)?
const FRAC_FAHRSPANNUNG_ÜBERSPANNUNG: f64 = 16. / 25.;
const UMDREHENZEIT: Duration = Duration::from_millis(500);

impl Geschwindigkeit<Mittelleiter> {
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-u8::MAX
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste)
    pub fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Error> {
        match &mut self.leiter {
            Mittelleiter::Pwm { pin, polarität } => {
                Ok(pin.geschwindigkeit(wert, FRAC_FAHRSPANNUNG_ÜBERSPANNUNG, *polarität)?)
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    pub fn umdrehen(&mut self) -> Result<(), Error> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        Ok(match &mut self.leiter {
            Mittelleiter::Pwm { pin, polarität } => {
                pin.enable_with_config(pwm::Config {
                    polarity: *polarität,
                    time: pwm::Time::Frequency { frequency: PWM_FREQUENZ, duty_cycle: 1. },
                })?;
                sleep(UMDREHENZEIT);
                pin.disable()?
            },
            Mittelleiter::KonstanteSpannung { umdrehen, .. } => {
                umdrehen.einstellen(Fließend::Fließend)?;
                sleep(UMDREHENZEIT);
                umdrehen.einstellen(Fließend::Gesperrt)?
            },
        })
    }
}

#[derive(Debug)]
pub enum Zweileiter {
    Pwm {
        geschwindigkeit: pwm::Pin,
        polarität: Polarity,
        fahrtrichtung: OutputAnschluss,
    },
    KonstanteSpannung {
        geschwindigkeit: NonEmpty<OutputAnschluss>,
        letzter_wert: usize,
        fahrtrichtung: OutputAnschluss,
    },
}

impl Geschwindigkeit<Zweileiter> {
    pub fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Error> {
        match &mut self.leiter {
            Zweileiter::Pwm { geschwindigkeit, polarität, .. } => {
                Ok(geschwindigkeit.geschwindigkeit(wert, 1., *polarität)?)
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }

    pub fn fahrtrichtung(&mut self, neue_fahrtrichtung: Fahrtrichtung) -> Result<(), Error> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        let fahrtrichtung = match &mut self.leiter {
            Zweileiter::Pwm { fahrtrichtung, .. } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        };
        Ok(fahrtrichtung.einstellen(neue_fahrtrichtung.into())?)
    }

    pub fn umdrehen(&mut self) -> Result<(), Error> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        let fahrtrichtung = match &mut self.leiter {
            Zweileiter::Pwm { fahrtrichtung, .. } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        };
        Ok(fahrtrichtung.umstellen()?)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Fahrtrichtung {
    Vorwärts,
    Rückwärts,
}
impl From<Fahrtrichtung> for Fließend {
    fn from(fahrtrichtung: Fahrtrichtung) -> Self {
        match fahrtrichtung {
            Fahrtrichtung::Vorwärts => Fließend::Fließend,
            Fahrtrichtung::Rückwärts => Fließend::Gesperrt,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Anschluss(anschluss::Error),
    Pwm(pwm::Error),
    ZuWenigAnschlüsse { benötigt: u8, vorhanden: usize },
}
impl From<anschluss::Error> for Error {
    fn from(error: anschluss::Error) -> Self {
        Error::Anschluss(error)
    }
}
impl From<pwm::Error> for Error {
    fn from(error: pwm::Error) -> Self {
        Error::Pwm(error)
    }
}
