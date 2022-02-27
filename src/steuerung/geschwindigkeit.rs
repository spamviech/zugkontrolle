//! Einstellen der Geschwindigkeit.

use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    sync::{mpsc::Sender, Arc},
    thread::{self, sleep},
    time::Duration,
    usize,
};

use log::{debug, error};
use nonempty::NonEmpty;
use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        polarität::{Fließend, Polarität},
        InputAnschluss, OutputAnschluss, OutputSerialisiert,
    },
    maybe_empty::MaybeEmpty,
};

pub trait Leiter {
    /// 0 deaktiviert die Stromzufuhr.
    /// Werte über dem Maximalwert werden wie der Maximalwert behandelt.
    /// Pwm: 0-u8::MAX
    /// Konstante Spannung: 0-#Anschlüsse (geordnete Liste)
    fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Fehler>;
}

#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Geschwindigkeit<Leiter> {
    leiter: Arc<Mutex<Leiter>>,
}

impl<L: Leiter> Geschwindigkeit<L> {
    pub fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Fehler> {
        self.lock_leiter().geschwindigkeit(wert)
    }
}

impl<Leiter: Display> Display for Geschwindigkeit<Leiter> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.lock_leiter())
    }
}

impl<Leiter> Geschwindigkeit<Leiter> {
    pub fn neu(leiter: Leiter) -> Self {
        Geschwindigkeit { leiter: Arc::new(Mutex::new(leiter)) }
    }

    #[inline(always)]
    pub(crate) fn lock_leiter<'t>(&'t self) -> MutexGuard<'t, Leiter> {
        self.leiter.lock()
    }
}
#[derive(Serialize, Deserialize)]
// TODO verwende custom derive-Macros
// `zugkontrolle_macros`-Macros erzeugen Fehler, wenn `Leiter::Serialisiert` verwendet wird
// `serde`-Macros erzeugen zusätzliche Constraints mit `<Leiter as Serialisiert>::Serialisiert`
//
// #[derive(zugkontrolle_macros::Clone, zugkontrolle_macros::Debug, Serialize, Deserialize)]
// #[zugkontrolle_clone(Leiter::Serialisiert: Clone)]
// #[zugkontrolle_debug(Leiter::Serialisiert: Debug)]
pub struct GeschwindigkeitSerialisiert<Leiter: Serialisiere> {
    pub leiter: Leiter::Serialisiert,
}

impl<Leiter> Clone for GeschwindigkeitSerialisiert<Leiter>
where
    Leiter: Serialisiere,
    Leiter::Serialisiert: Clone,
{
    fn clone(&self) -> Self {
        Self { leiter: self.leiter.clone() }
    }
}

impl<Leiter> Debug for GeschwindigkeitSerialisiert<Leiter>
where
    Leiter: Serialisiere,
    Leiter::Serialisiert: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("GeschwindigkeitSerialisiert").field("leiter", &self.leiter).finish()
    }
}

impl<T: Serialisiere> Serialisiere for Geschwindigkeit<T> {
    type Serialisiert = GeschwindigkeitSerialisiert<T>;

    fn serialisiere(&self) -> GeschwindigkeitSerialisiert<T> {
        GeschwindigkeitSerialisiert { leiter: self.lock_leiter().serialisiere() }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.leiter) {
            Ok(mutex) => mutex.into_inner().anschlüsse(),
            Err(_arc) => {
                // while-Schleife (mit thread::yield bei Err) bis nur noch eine Arc-Referenz besteht
                // (Ok wird zurückgegeben) wäre möglich, kann aber zur nicht-Terminierung führen
                // Gebe stattdessen keine Anschlüsse zurück
                (Vec::new(), Vec::new(), Vec::new())
            },
        }
    }
}

impl<T: Serialisiere> Reserviere<Geschwindigkeit<T>> for GeschwindigkeitSerialisiert<T> {
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Geschwindigkeit<T>> {
        let Reserviert {
            anschluss: leiter,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.leiter.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: Geschwindigkeit::neu(leiter),
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
) -> Result<(), pwm::Fehler> {
    debug_assert!(
        0. < faktor && faktor <= 1.,
        "Pwm-Faktor muss zwischen 0 und 1 liegen: {}",
        faktor
    );
    pin.enable_with_config(pwm::Config {
        polarity,
        time: pwm::Time::Frequency {
            frequency: PWM_FREQUENZ,
            duty_cycle: faktor * f64::from(wert) / f64::from(u8::MAX),
        },
    })
}
fn geschwindigkeit_ks(
    geschwindigkeit: &mut NonEmpty<OutputAnschluss>,
    letzter_wert: &mut usize,
    wert: u8,
) -> Result<(), Fehler> {
    let wert_usize = wert as usize;
    let length = geschwindigkeit.len();
    if wert_usize > length {
        return Err(Fehler::ZuWenigAnschlüsse { benötigt: wert, vorhanden: length });
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
    if wert_usize > 0 {
        geschwindigkeit.get_mut(wert_usize - 1).unwrap().einstellen(Fließend::Fließend)?;
    }
    *letzter_wert = wert_usize;
    Ok(())
}

pub type MittelleiterSerialisiert = Mittelleiter<pwm::Serialisiert, OutputSerialisiert>;
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "Pwm: Clone + Serialize, Anschluss: Clone + Serialize"))]
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
            },
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
            },
        }
    }
}

impl Serialisiere for Mittelleiter {
    type Serialisiert = MittelleiterSerialisiert;

    fn serialisiere(&self) -> MittelleiterSerialisiert {
        match self {
            Mittelleiter::Pwm { pin, polarität } => {
                Mittelleiter::Pwm { pin: pin.serialisiere(), polarität: *polarität }
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, umdrehen } => {
                Mittelleiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .iter()
                        .map(OutputAnschluss::serialisiere)
                        .collect::<MaybeEmpty<_>>()
                        .unwrap(),
                    letzter_wert: *letzter_wert,
                    umdrehen: umdrehen.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            Mittelleiter::Pwm { pin, polarität: _ } => pin.anschlüsse(),
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                let acc = umdrehen.anschlüsse();
                geschwindigkeit.into_iter().fold(acc, |mut acc, anschluss| {
                    let (pwm, output, input) = anschluss.anschlüsse();
                    acc.0.extend(pwm.into_iter());
                    acc.1.extend(output.into_iter());
                    acc.2.extend(input.into_iter());
                    acc
                })
            },
        }
    }
}
impl Reserviere<Mittelleiter> for MittelleiterSerialisiert {
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Mittelleiter> {
        Ok(match self {
            Mittelleiter::Pwm { pin, polarität } => {
                let Reserviert {
                    anschluss: pin,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = pin.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?;
                Reserviert {
                    anschluss: Mittelleiter::Pwm { pin, polarität },
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                }
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, umdrehen } => {
                let Reserviert {
                    anschluss: head,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.head.reserviere(
                    lager,
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
                            Ok(mut acc) => match save.reserviere(lager, acc.1, acc.2, acc.3) {
                                Ok(Reserviert {
                                    anschluss,
                                    pwm_nicht_benötigt,
                                    output_nicht_benötigt,
                                    input_nicht_benötigt,
                                }) => {
                                    acc.0.push(anschluss);
                                    acc.1 = pwm_nicht_benötigt;
                                    acc.2 = output_nicht_benötigt;
                                    acc.3 = input_nicht_benötigt;
                                    Ok(acc)
                                },
                                Err(mut error) => {
                                    error.output_anschlüsse.extend(acc.0.into_iter());
                                    Err(error)
                                },
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
                    lager,
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

impl Leiter for Mittelleiter {
    fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Fehler> {
        match self {
            Mittelleiter::Pwm { pin, polarität } => {
                Ok(geschwindigkeit_pwm(pin, wert, FRAC_FAHRSPANNUNG_ÜBERSPANNUNG, *polarität)?)
            },
            Mittelleiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }
}

impl Geschwindigkeit<Mittelleiter> {
    pub fn umdrehen(&mut self) -> Result<(), Fehler> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        match &mut *self.lock_leiter() {
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
        }
        Ok(())
    }

    pub fn async_umdrehen<Nachricht: Send + 'static>(
        &mut self,
        sender: Sender<Nachricht>,
        erzeuge_nachricht: impl FnOnce(Fehler) -> Nachricht + Send + 'static,
    ) {
        let mut clone = self.clone();
        let _ = thread::spawn(move || {
            if let Err(fehler) = clone.umdrehen() {
                let send_result = sender.send(erzeuge_nachricht(fehler));
                if let Err(fehler) = send_result {
                    debug!("Message-Channel für Geschwindigkeit geschlossen: {:?}", fehler)
                }
            }
        });
    }

    pub(crate) fn ks_länge(&self) -> Option<usize> {
        match &*self.lock_leiter() {
            Mittelleiter::Pwm { .. } => None,
            Mittelleiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.len()),
        }
    }
}

pub type ZweileiterSerialisiert = Zweileiter<pwm::Serialisiert, OutputSerialisiert>;
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "Pwm: Clone + Serialize, Anschluss: Clone + Serialize"))]
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
            },
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
            },
        }
    }
}

impl Leiter for Zweileiter {
    fn geschwindigkeit(&mut self, wert: u8) -> Result<(), Fehler> {
        match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, .. } => {
                Ok(geschwindigkeit_pwm(geschwindigkeit, wert, 1., *polarität)?)
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, .. } => {
                geschwindigkeit_ks(geschwindigkeit, letzter_wert, wert)
            },
        }
    }
}

impl Geschwindigkeit<Zweileiter> {
    pub fn fahrtrichtung(&mut self, neue_fahrtrichtung: Fahrtrichtung) -> Result<(), Fehler> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        let mut guard = self.lock_leiter();
        let fahrtrichtung = match &mut *guard {
            Zweileiter::Pwm { fahrtrichtung, .. } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        };
        Ok(fahrtrichtung.einstellen(neue_fahrtrichtung.into())?)
    }

    pub fn async_fahrtrichtung<Nachricht: Send + 'static>(
        &mut self,
        neue_fahrtrichtung: Fahrtrichtung,
        sender: Sender<Nachricht>,
        erzeuge_nachricht: impl FnOnce(Fehler) -> Nachricht + Send + 'static,
    ) {
        let mut clone = self.clone();
        let _ = thread::spawn(move || {
            if let Err(fehler) = clone.fahrtrichtung(neue_fahrtrichtung) {
                let send_result = sender.send(erzeuge_nachricht(fehler));
                if let Err(fehler) = send_result {
                    debug!("Message-Channel für Geschwindigkeit geschlossen: {:?}", fehler)
                }
            }
        });
    }

    pub fn umdrehen(&mut self) -> Result<(), Fehler> {
        self.geschwindigkeit(0)?;
        sleep(STOPPZEIT);
        let mut guard = self.lock_leiter();
        let fahrtrichtung = match &mut *guard {
            Zweileiter::Pwm { fahrtrichtung, .. } => fahrtrichtung,
            Zweileiter::KonstanteSpannung { fahrtrichtung, .. } => fahrtrichtung,
        };
        Ok(fahrtrichtung.umstellen()?)
    }

    pub fn async_umdrehen<Nachricht: Send + 'static>(
        &mut self,
        sender: Sender<Nachricht>,
        erzeuge_nachricht: impl FnOnce(Fehler) -> Nachricht + Send + 'static,
    ) {
        let mut clone = self.clone();
        let _ = thread::spawn(move || {
            if let Err(fehler) = clone.umdrehen() {
                let send_result = sender.send(erzeuge_nachricht(fehler));
                if let Err(fehler) = send_result {
                    debug!("Message-Channel für Geschwindigkeit geschlossen: {:?}", fehler)
                }
            }
        });
    }

    pub(crate) fn ks_länge(&self) -> Option<usize> {
        match &*self.lock_leiter() {
            Zweileiter::Pwm { .. } => None,
            Zweileiter::KonstanteSpannung { geschwindigkeit, .. } => Some(geschwindigkeit.len()),
        }
    }
}

impl Serialisiere for Zweileiter {
    type Serialisiert = ZweileiterSerialisiert;

    fn serialisiere(&self) -> ZweileiterSerialisiert {
        match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => Zweileiter::Pwm {
                geschwindigkeit: geschwindigkeit.serialisiere(),
                polarität: *polarität,
                fahrtrichtung: fahrtrichtung.serialisiere(),
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert, fahrtrichtung } => {
                Zweileiter::KonstanteSpannung {
                    geschwindigkeit: geschwindigkeit
                        .iter()
                        .map(OutputAnschluss::serialisiere)
                        .collect::<MaybeEmpty<_>>()
                        .unwrap(),
                    letzter_wert: *letzter_wert,
                    fahrtrichtung: fahrtrichtung.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match self {
            Zweileiter::Pwm { geschwindigkeit, polarität: _, fahrtrichtung } => {
                let (mut pwm0, mut output0, mut input0) = geschwindigkeit.anschlüsse();
                let (pwm1, output1, input1) = fahrtrichtung.anschlüsse();
                pwm0.extend(pwm1.into_iter());
                output0.extend(output1.into_iter());
                input0.extend(input1.into_iter());
                (pwm0, output0, input0)
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                let acc = fahrtrichtung.anschlüsse();
                geschwindigkeit.into_iter().fold(acc, |mut acc, anschluss| {
                    let (pwm, output, input) = anschluss.anschlüsse();
                    acc.0.extend(pwm.into_iter());
                    acc.1.extend(output.into_iter());
                    acc.2.extend(input.into_iter());
                    acc
                })
            },
        }
    }
}
impl Reserviere<Zweileiter> for ZweileiterSerialisiert {
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Zweileiter> {
        Ok(match self {
            Zweileiter::Pwm { geschwindigkeit, polarität, fahrtrichtung } => {
                let Reserviert {
                    anschluss: geschwindigkeit,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.reserviere(
                    lager,
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
                    lager,
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
            },
            Zweileiter::KonstanteSpannung { geschwindigkeit, letzter_wert: _, fahrtrichtung } => {
                let Reserviert {
                    anschluss: head,
                    pwm_nicht_benötigt,
                    output_nicht_benötigt,
                    input_nicht_benötigt,
                } = geschwindigkeit.head.reserviere(
                    lager,
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
                            Ok(mut acc) => match save.reserviere(lager, acc.1, acc.2, acc.3) {
                                Ok(Reserviert {
                                    anschluss,
                                    pwm_nicht_benötigt,
                                    output_nicht_benötigt,
                                    input_nicht_benötigt,
                                }) => {
                                    acc.0.push(anschluss);
                                    acc.1 = pwm_nicht_benötigt;
                                    acc.2 = output_nicht_benötigt;
                                    acc.3 = input_nicht_benötigt;
                                    Ok(acc)
                                },
                                Err(mut error) => {
                                    error.output_anschlüsse.extend(acc.0.into_iter());
                                    Err(error)
                                },
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
                    lager,
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
impl Display for Fahrtrichtung {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

#[derive(Debug, zugkontrolle_macros::From)]
#[allow(variant_size_differences)]
pub enum Fehler {
    Anschluss(anschluss::Fehler),
    Pwm(pwm::Fehler),
    ZuWenigAnschlüsse { benötigt: u8, vorhanden: usize },
}

/// Name einer Geschwindigkeit.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

pub type Map<Leiter> = HashMap<Name, Geschwindigkeit<Leiter>>;
pub type MapSerialisiert<Leiter> = HashMap<Name, GeschwindigkeitSerialisiert<Leiter>>;
