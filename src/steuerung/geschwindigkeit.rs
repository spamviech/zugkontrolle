//! Einstellen der Geschwindigkeit.

use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
    thread::sleep,
    time::Duration,
    usize,
};

use log::error;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self, pwm,
        speichern::{self, Reserviere, Reserviert, ToSave},
        Anschlüsse, Fließend, InputAnschluss, OutputAnschluss, OutputSave, Polarität,
    },
    non_empty::{MaybeEmpty, NonEmpty},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Geschwindigkeit<Leiter> {
    pub leiter: Leiter,
}

impl<Anschluss, T: ToSave> ToSave for Geschwindigkeit<T> {
    type Save = Geschwindigkeit<T::Save>;

    fn to_save(&self) -> Geschwindigkeit<T::Save> {
        Geschwindigkeit { leiter: self.leiter.to_save() }
    }
}

impl<T: Reserviere<R>, R> Reserviere<Geschwindigkeit<R>> for Geschwindigkeit<T> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern::Result<Geschwindigkeit<R>> {
        let Reserviert {
            anschluss: leiter,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.leiter.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: Geschwindigkeit { leiter },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

fn geschwindigkeit_pwm(
    pin: &mut pwm::Pin,
    wert: u8,
    faktor: f64,
    polarity: Polarität,
) -> Result<(), pwm::Error> {
    debug_assert!(0. < faktor && faktor <= 1., "Faktor muss zwischen 0 und 1 liegen: {}", faktor);
    pin.enable_with_config(pwm::Config {
        polarity,
        time: pwm::Time::Frequency {
            frequency: PWM_FREQUENZ,
            duty_cycle: faktor * wert as f64 / u8::MAX as f64,
        },
    })
}
fn geschwindigkeit_ks(
    geschwindigkeit: &mut NonEmpty<OutputAnschluss>,
    letzter_wert: &mut usize,
    wert: u8,
) -> Result<(), Error> {
    let wert_usize = wert as usize;
    let length = geschwindigkeit.len();
    if wert_usize > length {
        return Err(Error::ZuWenigAnschlüsse { benötigt: wert, vorhanden: length });
    }
    // aktuellen Anschluss ausstellen
    if *letzter_wert == 0 {
        // Geschwindigkeit war aus, es muss also kein Anschluss ausgeschaltet werden
    } else if let Some(anschluss) = geschwindigkeit.get_mut(*letzter_wert - 1) {
        anschluss.einstellen(Fließend::Gesperrt)?;
    } else {
        error!(
            "Letzter Wert ist {}, Geschwindigkeit hat aber nur {} Anschlüsse!",
            letzter_wert, length
        )
    }
    // neuen anstellen
    *letzter_wert = wert_usize;
    if wert_usize > 0 {
        geschwindigkeit.get_mut(wert_usize - 1).unwrap().einstellen(Fließend::Fließend)?;
    }
    Ok(())
}

pub type MittelleiterSave = Mittelleiter<pwm::Save, OutputSave>;
#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl Display for Mittelleiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Mittelleiter::Pwm { pin, polarität } => {
                write!(f, "Pwm({}, {})", pin.pin(), polarität)
            }
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                write!(f, "KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", anschluss)?;
                }
                write!(f, "-{})", umdrehen)
            }
        }
    }
}

impl ToSave for Mittelleiter {
    type Save = MittelleiterSave;

    fn to_save(&self) -> MittelleiterSave {
        match self {
            Mittelleiter::Pwm { pin, polarität } => {
                Mittelleiter::Pwm { pin: pin.to_save(), polarität: *polarität }
            }
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
            }
        }
    }
}
impl Reserviere<Mittelleiter> for MittelleiterSave {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern::Result<Mittelleiter> {
        Ok(match self {
            Mittelleiter::Pwm { pin, polarität } => {
                let Reserviert {
                    anschluss: pin,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = pin.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
                Reserviert {
                    anschluss: Mittelleiter::Pwm { pin, polarität },
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }
            }
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                let Reserviert {
                    anschluss: head,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.head.reserviere(
                    anschlüsse,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                )?;
                let (tail, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt) =
                    geschwindigkeit.tail.into_iter().fold(
                        Ok((
                            Vec::new(),
                            pwm_nicht_benötigt,
                            output_nicht_benötigt,
                            input_nicht_benötigt,
                        )),
                        |acc_res, save| match acc_res {
                            Ok(mut acc) => match save.reserviere(anschlüsse, acc.1.into_iter()) {
                                Ok(Reserviert {
                                    anschluss,
                                    pwm_nicht_benötigt,
                                    output_nicht_benötigt,
                                    input_nicht_benötigt,
                                }) => {
                                    acc.0.push(anschluss);
                                    Ok(acc)
                                }
                                Err(speichern::Error {
                                    fehler,
                                    pwm_pins,
                                    output_anschlüsse,
                                    input_anschlüsse,
                                }) => {
                                    output_anschlüsse.extend(acc.0.into_iter());
                                    Err(speichern::Error {
                                        fehler,
                                        pwm_pins,
                                        output_anschlüsse,
                                        input_anschlüsse,
                                    })
                                }
                            },
                            error => error,
                        },
                    )?;
                let Reserviert {
                    anschluss: umdrehen,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = umdrehen.reserviere(
                    anschlüsse,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                )?;
                Reserviert {
                    anschluss: Mittelleiter::KonstanteSpannung {
                        geschwindigkeit: NonEmpty { head, tail },
                        letzter_wert: 0,
                        umdrehen,
                    },
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }
            }
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
                Ok(geschwindigkeit_pwm(pin, wert, FRAC_FAHRSPANNUNG_ÜBERSPANNUNG, *polarität)?)
            }
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            }
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
            }
            Mittelleiter::KonstanteSpannung { umdrehen, .. } => {
                umdrehen.einstellen(Fließend::Fließend)?;
                sleep(UMDREHENZEIT);
                umdrehen.einstellen(Fließend::Gesperrt)?
            }
        })
    }
}

pub type ZweileiterSave = Zweileiter<pwm::Save, OutputSave>;
#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl Display for Zweileiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                write!(f, "Pwm({}, {}-{})", geschwindigkeit.pin(), polarität, fahrtrichtung)
            }
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                write!(f, "KonstanteSpannung(")?;
                let mut first = true;
                for anschluss in geschwindigkeit.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", anschluss)?;
                }
                write!(f, "-{})", fahrtrichtung)
            }
        }
    }
}

impl Geschwindigkeit<Zweileiter> {
    pub fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Error> {
        match &mut self.leiter {
            Zweileiter::Pwm { geschwindigkeit, polarität, .. } => {
                Ok(geschwindigkeit_pwm(geschwindigkeit, wert, 1., *polarität)?)
            }
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            }
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

impl ToSave for Zweileiter {
    type Save = ZweileiterSave;

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
            }
        }
    }
}
impl Reserviere<Zweileiter> for ZweileiterSave {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern::Result<Zweileiter> {
        Ok(match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                let Reserviert {
                    anschluss: geschwindigkeit,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.reserviere(
                    anschlüsse,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                )?;
                let Reserviert {
                    anschluss: fahrtrichtung,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = fahrtrichtung.reserviere(
                    anschlüsse,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                )?;
                Reserviert {
                    anschluss: Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung },
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }
            }
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                let Reserviert {
                    anschluss: head,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.head.reserviere(
                    anschlüsse,
                    pwm_pins,
                    output_anschlüsse,
                    input_anschlüsse,
                )?;
                let (tail, pwm_nicht_benötigt, output_nicht_benötigt, input_nicht_benötigt) =
                    geschwindigkeit.tail.into_iter().fold(
                        Ok((
                            Vec::new(),
                            pwm_nicht_benötigt,
                            output_nicht_benötigt,
                            input_nicht_benötigt,
                        )),
                        |acc_res, save| match acc_res {
                            Ok(mut acc) => match save.reserviere(anschlüsse, acc.1.into_iter()) {
                                Ok(Reserviert {
                                    anschluss,
                                    pwm_nicht_benötigt,
                                    output_nicht_benötigt,
                                    input_nicht_benötigt,
                                }) => {
                                    acc.0.push(anschluss);
                                    Ok(acc)
                                }
                                Err(speichern::Error {
                                    fehler,
                                    pwm_pins,
                                    output_anschlüsse,
                                    input_anschlüsse,
                                }) => {
                                    output_anschlüsse.extend(acc.0.into_iter());
                                    Err(speichern::Error {
                                        fehler,
                                        pwm_pins,
                                        output_anschlüsse,
                                        input_anschlüsse,
                                    })
                                }
                            },
                            error => error,
                        },
                    )?;
                let Reserviert {
                    anschluss: fahrtrichtung,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = fahrtrichtung.reserviere(
                    anschlüsse,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                )?;
                Reserviert {
                    anschluss: Zweileiter::KonstanteSpannung {
                        geschwindigkeit: NonEmpty { head, tail },
                        letzter_wert: 0,
                        fahrtrichtung,
                    },
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }
            }
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
impl Display for Fahrtrichtung {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Fahrtrichtung::Vorwärts => "Vorwärts",
                Fahrtrichtung::Rückwärts => "Rückwärts",
            }
        )
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
