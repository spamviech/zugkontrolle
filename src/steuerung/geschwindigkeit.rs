//! Einstellen der Geschwindigkeit.

use std::collections::BTreeMap;
use std::usize;
use std::{thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    self,
    pwm,
    Anschlüsse,
    Fließend,
    OutputAnschluss,
    OutputSave,
    Polarität,
    Reserviere,
    ToSave,
};
use crate::non_empty::{MaybeEmpty, NonEmpty};

#[derive(Debug, Serialize, Deserialize)]
pub struct Geschwindigkeit<Leiter> {
    pub leiter: Leiter,
}

impl<T, S> ToSave<Geschwindigkeit<S>> for Geschwindigkeit<T>
where
    T: ToSave<S>,
    S: Serialize + for<'de> Deserialize<'de>,
{
    fn to_save(&self) -> Geschwindigkeit<S> {
        Geschwindigkeit { leiter: self.leiter.to_save() }
    }
}

impl<T: Reserviere<R>, R> Reserviere<Geschwindigkeit<R>> for Geschwindigkeit<T> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
    ) -> Result<Geschwindigkeit<R>, anschluss::Error> {
        Ok(Geschwindigkeit { leiter: self.leiter.reserviere(anschlüsse)? })
    }
}

impl pwm::Pin {
    fn geschwindigkeit(
        &mut self,
        wert: u8,
        faktor: f64,
        polarity: Polarität,
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
    let length = geschwindigkeit.len();
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

pub type MittelleiterSave = Mittelleiter<pwm::Save, OutputSave>;
#[derive(Debug, Serialize, Deserialize)]
pub enum Mittelleiter<Pwm = pwm::Pin, Anschluss = OutputAnschluss> {
    Pwm {
        pin: Pwm,
        polarität: Polarität,
    },
    KonstanteSpannung {
        geschwindigkeit: NonEmpty<Anschluss>,
        letzter_wert: usize,
        umdrehen: Anschluss,
    },
}

impl ToSave<MittelleiterSave> for Mittelleiter {
    fn to_save(&self) -> MittelleiterSave {
        match self {
            Mittelleiter::Pwm { pin, polarität } => {
                Mittelleiter::Pwm { pin: pin.to_save(), polarität: *polarität }
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, umdrehen } => {
                Mittelleiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .iter()
                        .map(OutputAnschluss::to_save)
                        .collect::<MaybeEmpty<_>>()
                        .unwrap(),
                    letzter_wert: *letzter_wert,
                    umdrehen: umdrehen.to_save(),
                }
            },
        }
    }
}
impl Reserviere<Mittelleiter> for MittelleiterSave {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<Mittelleiter, anschluss::Error> {
        Ok(match self {
            Mittelleiter::Pwm { pin, polarität } => {
                Mittelleiter::Pwm { pin: pin.reserviere(anschlüsse)?, polarität }
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                Mittelleiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .into_iter()
                        .map(|anschluss| anschluss.reserviere(anschlüsse))
                        .collect::<Result<MaybeEmpty<_>, _>>()?
                        .unwrap(),
                    letzter_wert: 0,
                    umdrehen: umdrehen.reserviere(anschlüsse)?,
                }
            },
        })
    }
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

pub type ZweileiterSave = Zweileiter<pwm::Save, OutputSave>;
#[derive(Debug, Serialize, Deserialize)]
pub enum Zweileiter<Pwm = pwm::Pin, Anschluss = OutputAnschluss> {
    Pwm {
        geschwindigkeit: Pwm,
        polarität: Polarität,
        fahrtrichtung: Anschluss,
    },
    KonstanteSpannung {
        geschwindigkeit: NonEmpty<Anschluss>,
        letzter_wert: usize,
        fahrtrichtung: Anschluss,
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

impl ToSave<ZweileiterSave> for Zweileiter {
    fn to_save(&self) -> ZweileiterSave {
        match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => Zweileiter::Pwm {
                geschwindigkeit: geschwindigkeit.to_save(),
                polarität: *polarität,
                fahrtrichtung: fahrtrichtung.to_save(),
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, fahrtrichtung } => {
                Zweileiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .iter()
                        .map(OutputAnschluss::to_save)
                        .collect::<MaybeEmpty<_>>()
                        .unwrap(),
                    letzter_wert: *letzter_wert,
                    fahrtrichtung: fahrtrichtung.to_save(),
                }
            },
        }
    }
}
impl Reserviere<Zweileiter> for ZweileiterSave {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<Zweileiter, anschluss::Error> {
        Ok(match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => Zweileiter::Pwm {
                geschwindigkeit: geschwindigkeit.reserviere(anschlüsse)?,
                polarität,
                fahrtrichtung: fahrtrichtung.reserviere(anschlüsse)?,
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                Zweileiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .into_iter()
                        .map(|anschluss| anschluss.reserviere(anschlüsse))
                        .collect::<Result<MaybeEmpty<_>, _>>()?
                        .unwrap(),
                    letzter_wert: 0,
                    fahrtrichtung: fahrtrichtung.reserviere(anschlüsse)?,
                }
            },
        })
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

/// Name einer Geschwindigkeit.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Leiter> = BTreeMap<Name, Geschwindigkeit<Leiter>>;
