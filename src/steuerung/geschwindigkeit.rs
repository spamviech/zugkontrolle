//! Einstellen der Geschwindigkeit.

use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
    thread::sleep,
    time::Duration,
    usize,
};

use ::serde::{Deserialize, Serialize};
use log::error;

use crate::anschluss::{
    self, pwm,
    serde::{self, Reserviere, Reserviert, ToSave},
    Anschlüsse, Fließend, OutputAnschluss, OutputSave, Polarität,
};
use crate::non_empty::{MaybeEmpty, NonEmpty};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Geschwindigkeit<Leiter> {
    pub leiter: Leiter,
}

impl<T: ToSave> ToSave for Geschwindigkeit<T> {
    type Save = Geschwindigkeit<T::Save>;

    fn to_save(&self) -> Geschwindigkeit<T::Save> {
        Geschwindigkeit { leiter: self.leiter.to_save() }
    }
}

impl<T: Reserviere<R>, R> Reserviere<Geschwindigkeit<R>> for Geschwindigkeit<T> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = Geschwindigkeit<R>>,
    ) -> serde::Result<Geschwindigkeit<R>> {
        let konvertiere_leiter = |leiter_vec: Vec<R>| {
            leiter_vec.into_iter().map(|leiter| Geschwindigkeit { leiter }).collect()
        };
        let Reserviert { anschluss: leiter, nicht_benötigt: nicht_benötigte_leiter } = self
            .leiter
            .reserviere(anschlüsse, bisherige_anschlüsse.map(|Geschwindigkeit { leiter }| leiter))
            .map_err(|serde::Error { fehler, bisherige_anschlüsse: bisherige_leiter }| {
                serde::Error { fehler, bisherige_anschlüsse: konvertiere_leiter(bisherige_leiter) }
            })?;
        Ok(Reserviert {
            anschluss: Geschwindigkeit { leiter },
            nicht_benötigt: konvertiere_leiter(nicht_benötigte_leiter),
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
        bisherige_anschlüsse: impl Iterator<Item = Mittelleiter>,
    ) -> serde::Result<Mittelleiter> {
        let (pwm_pins, ks_anschlüsse, save) =
            bisherige_anschlüsse.fold((Vec::new(), Vec::new(), Vec::new()), |acc, mittelleiter| {
                acc.2.push(mittelleiter.to_save());
                match mittelleiter {
                    Mittelleiter::Pwm { pin, polarität } => acc.0.push(pin),
                    Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, umdrehen } => {
                        acc.1.extend(geschwindigkeit.into_iter().chain(std::iter::once(umdrehen)))
                    }
                }
                acc
            });
        let anschluss_sammlung = |pwm_pins: Vec<pwm::Pin>, ks_anschlüsse: Vec<OutputAnschluss>| {
            let mut pwm_map: HashMap<_, _> =
                pwm_pins.into_iter().map(|pin| (pin.to_save(), pin)).collect();
            let mut ks_map: HashMap<_, _> = ks_anschlüsse
                .into_iter()
                .map(|anschluss| (anschluss.to_save(), anschluss))
                .collect();
            save.into_iter()
                .filter_map(|save| match save {
                    Mittelleiter::Pwm { pin, polarität } => {
                        pwm_map.remove(&pin).map(|pin| Mittelleiter::Pwm { pin, polarität })
                    }
                    Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, umdrehen } => {
                        // TODO Mittelleiter wird nur vollständig oder gar nicht zurückgegeben
                        // (stattdessen via drop an singleton ANSCHLÜSSE)
                        // bei Fehler sind alle vollständig,
                        // daher nur ein Problem wenn vor laden nicht aufgeräumt wurde
                        ks_map.remove(&umdrehen).and_then(|umdrehen| {
                            geschwindigkeit
                                .iter()
                                .map(|save| ks_map.remove(save))
                                .collect::<Option<Vec<OutputAnschluss>>>()
                                .and_then(NonEmpty::from_vec)
                                .map(|geschwindigkeit| Mittelleiter::KonstanteSpannung {
                                    geschwindigkeit,
                                    letzter_wert,
                                    umdrehen,
                                })
                        })
                    }
                })
                .collect::<Vec<Mittelleiter>>()
        };
        Ok(match self {
            Mittelleiter::Pwm { pin, polarität } => {
                let Reserviert { anschluss: pin, nicht_benötigt } = pin
                    .reserviere(anschlüsse, pwm_pins.into_iter())
                    .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                        fehler,
                        bisherige_anschlüsse: anschluss_sammlung(
                            bisherige_anschlüsse,
                            ks_anschlüsse,
                        ),
                    })?;
                Reserviert {
                    anschluss: Mittelleiter::Pwm { pin, polarität },
                    nicht_benötigt: anschluss_sammlung(nicht_benötigt, ks_anschlüsse),
                }
            }
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                let Reserviert { anschluss: head, nicht_benötigt } = geschwindigkeit
                    .head
                    .reserviere(anschlüsse, ks_anschlüsse.into_iter())
                    .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                        fehler,
                        bisherige_anschlüsse: anschluss_sammlung(pwm_pins, bisherige_anschlüsse),
                    })?;
                let (tail, nicht_benötigt) = geschwindigkeit.tail.into_iter().fold(
                    Ok((Vec::new(), nicht_benötigt)),
                    |acc_res, save| match acc_res {
                        Ok(mut acc) => match save.reserviere(anschlüsse, acc.1.into_iter()) {
                            Ok(Reserviert { anschluss, nicht_benötigt }) => {
                                acc.0.push(anschluss);
                                Ok((acc.0, nicht_benötigt))
                            }
                            Err(serde::Error { fehler, mut bisherige_anschlüsse }) => {
                                bisherige_anschlüsse.extend(acc.0.into_iter());
                                Err(serde::Error {
                                    fehler,
                                    bisherige_anschlüsse: anschluss_sammlung(
                                        pwm_pins,
                                        bisherige_anschlüsse,
                                    ),
                                })
                            }
                        },
                        error => error,
                    },
                )?;
                let Reserviert { anschluss: umdrehen, nicht_benötigt } = umdrehen
                    .reserviere(anschlüsse, nicht_benötigt.into_iter())
                    .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                        fehler,
                        bisherige_anschlüsse: anschluss_sammlung(pwm_pins, bisherige_anschlüsse),
                    })?;
                Reserviert {
                    anschluss: Mittelleiter::KonstanteSpannung {
                        geschwindigkeit: NonEmpty { head, tail },
                        letzter_wert: 0,
                        umdrehen,
                    },
                    nicht_benötigt: anschluss_sammlung(pwm_pins, nicht_benötigt),
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
        bisherige_anschlüsse: impl Iterator<Item = Zweileiter>,
    ) -> serde::Result<Zweileiter> {
        let (pwm_pins, ks_anschlüsse, save) =
            bisherige_anschlüsse.fold((Vec::new(), Vec::new(), Vec::new()), |acc, mittelleiter| {
                acc.2.push(mittelleiter.to_save());
                match mittelleiter {
                    Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                        acc.0.push(geschwindigkeit);
                        acc.1.push(fahrtrichtung);
                    }
                    Zweileiter::KonstanteSpannung {
                        geschwindigkeit,
                        letzter_wert,
                        fahrtrichtung,
                    } => acc
                        .1
                        .extend(geschwindigkeit.into_iter().chain(std::iter::once(fahrtrichtung))),
                }
                acc
            });
        let anschluss_sammlung = |pwm_pins: Vec<pwm::Pin>, ks_anschlüsse: Vec<OutputAnschluss>| {
            let mut pwm_map: HashMap<_, _> =
                pwm_pins.into_iter().map(|pin| (pin.to_save(), pin)).collect();
            let mut ks_map: HashMap<_, _> = ks_anschlüsse
                .into_iter()
                .map(|anschluss| (anschluss.to_save(), anschluss))
                .collect();
            save.into_iter()
                .filter_map(|save| match save {
                    Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                        pwm_map.remove(&geschwindigkeit).and_then(|geschwindigkeit| {
                            ks_map.remove(&fahrtrichtung).map(|fahrtrichtung| Zweileiter::Pwm {
                                geschwindigkeit,
                                polarität,
                                fahrtrichtung,
                            })
                        })
                    }
                    Zweileiter::KonstanteSpannung {
                        geschwindigkeit,
                        letzter_wert,
                        fahrtrichtung,
                    } => {
                        // TODO Zweileiter wird nur vollständig oder gar nicht zurückgegeben
                        // (stattdessen via drop an singleton ANSCHLÜSSE)
                        // bei Fehler sind alle vollständig,
                        // daher nur ein Problem wenn vor laden nicht aufgeräumt wurde
                        ks_map.remove(&fahrtrichtung).and_then(|fahrtrichtung| {
                            geschwindigkeit
                                .iter()
                                .map(|save| ks_map.remove(save))
                                .collect::<Option<Vec<OutputAnschluss>>>()
                                .and_then(NonEmpty::from_vec)
                                .map(|geschwindigkeit| Zweileiter::KonstanteSpannung {
                                    geschwindigkeit,
                                    letzter_wert,
                                    fahrtrichtung,
                                })
                        })
                    }
                })
                .collect::<Vec<Zweileiter>>()
        };
        Ok(match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                let Reserviert { anschluss: geschwindigkeit, nicht_benötigt: pwm_nicht_benötigt } =
                    geschwindigkeit.reserviere(anschlüsse, pwm_pins.into_iter()).map_err(
                        |serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                            fehler,
                            bisherige_anschlüsse: anschluss_sammlung(
                                bisherige_anschlüsse,
                                ks_anschlüsse,
                            ),
                        },
                    )?;
                let Reserviert { anschluss: fahrtrichtung, nicht_benötigt: ks_nicht_benötigt } =
                    fahrtrichtung.reserviere(anschlüsse, ks_anschlüsse.into_iter()).map_err(
                        |serde::Error { fehler, bisherige_anschlüsse }| {
                            pwm_nicht_benötigt.push(geschwindigkeit);
                            serde::Error {
                                fehler,
                                bisherige_anschlüsse: anschluss_sammlung(
                                    pwm_nicht_benötigt,
                                    bisherige_anschlüsse,
                                ),
                            }
                        },
                    )?;
                Reserviert {
                    anschluss: Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung },
                    nicht_benötigt: anschluss_sammlung(pwm_nicht_benötigt, ks_nicht_benötigt),
                }
            }
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                let Reserviert { anschluss: head, nicht_benötigt } = geschwindigkeit
                    .head
                    .reserviere(anschlüsse, ks_anschlüsse.into_iter())
                    .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                        fehler,
                        bisherige_anschlüsse: anschluss_sammlung(pwm_pins, bisherige_anschlüsse),
                    })?;
                let (tail, nicht_benötigt) = geschwindigkeit.tail.into_iter().fold(
                    Ok((Vec::new(), nicht_benötigt)),
                    |acc_res, save| match acc_res {
                        Ok(mut acc) => match save.reserviere(anschlüsse, acc.1.into_iter()) {
                            Ok(Reserviert { anschluss, nicht_benötigt }) => {
                                acc.0.push(anschluss);
                                Ok((acc.0, nicht_benötigt))
                            }
                            Err(serde::Error { fehler, mut bisherige_anschlüsse }) => {
                                bisherige_anschlüsse.extend(acc.0.into_iter());
                                Err(serde::Error {
                                    fehler,
                                    bisherige_anschlüsse: anschluss_sammlung(
                                        pwm_pins,
                                        bisherige_anschlüsse,
                                    ),
                                })
                            }
                        },
                        error => error,
                    },
                )?;
                let Reserviert { anschluss: fahrtrichtung, nicht_benötigt } = fahrtrichtung
                    .reserviere(anschlüsse, nicht_benötigt.into_iter())
                    .map_err(|serde::Error { fehler, bisherige_anschlüsse }| serde::Error {
                        fehler,
                        bisherige_anschlüsse: anschluss_sammlung(pwm_pins, bisherige_anschlüsse),
                    })?;
                Reserviert {
                    anschluss: Zweileiter::KonstanteSpannung {
                        geschwindigkeit: NonEmpty { head, tail },
                        letzter_wert: 0,
                        fahrtrichtung,
                    },
                    nicht_benötigt: anschluss_sammlung(pwm_pins, nicht_benötigt),
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
