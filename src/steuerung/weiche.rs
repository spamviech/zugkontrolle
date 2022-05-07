//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    sync::{mpsc::Sender, Arc},
    thread::{sleep, JoinHandle},
    time::Duration,
};

use log::debug;
use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Serialisiere},
        pin::pwm,
        polarität::Fließend,
        Fehler, InputAnschluss, OutputAnschluss,
    },
    gleis::gleise::steuerung::Steuerung,
    nachschlagen::Nachschlagen,
    steuerung::plan::async_ausführen,
    typen::canvas::Cache,
};

/// Name einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
/// Die [Steuerung](WeicheSteuerung) und der [Name] einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BenannteWeiche<Steuerung> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die Steuerung der Weiche.
    steuerung: Steuerung,
}

/// Die Steuerung einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSteuerung<Richtung, Anschlüsse> {
    /// Die aktuelle Richtung der Weiche.
    pub aktuelle_richtung: Richtung,
    /// Die Richtung vor der aktuellen Richtung.
    pub letzte_richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

/// Steuerung und Name eines Schaltbaren Gleises.
pub type Weiche<Richtung, Anschlüsse> =
    BenannteWeiche<Arc<Mutex<Steuerung<WeicheSteuerung<Richtung, Anschlüsse>>>>>;

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erstelle eine neue [Weichen-Steuerung](Weiche).
    pub fn neu(
        name: Name,
        aktuelle_richtung: Richtung,
        letzte_richtung: Richtung,
        anschlüsse: Anschlüsse,
        cache: Arc<Mutex<Cache>>,
    ) -> Self {
        Weiche {
            name,
            steuerung: Arc::new(Mutex::new(Steuerung::neu(
                WeicheSteuerung { aktuelle_richtung, letzte_richtung, anschlüsse },
                cache,
            ))),
        }
    }
}

impl<Richtung: Clone, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erhalte die aktuelle Richtung einer [Weiche].
    pub fn aktuelle_richtung(&self) -> Richtung {
        self.steuerung.lock().as_ref().aktuelle_richtung.clone()
    }

    /// Erhalte die aktuelle und letzte Richtung einer [Weiche].
    pub fn aktuelle_und_letzte_richtung(&self) -> (Richtung, Richtung) {
        let guard = self.steuerung.lock();
        let WeicheSteuerung { aktuelle_richtung, letzte_richtung, .. } = guard.as_ref();
        (aktuelle_richtung.clone(), letzte_richtung.clone())
    }
}

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
pub type WeicheSerialisiert<Richtung, Anschlüsse> =
    BenannteWeiche<WeicheSteuerung<Richtung, Anschlüsse>>;

impl<Richtung, Anschlüsse> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [WeicheSerialisiert].
    pub fn neu(name: Name, steuerung: WeicheSteuerung<Richtung, Anschlüsse>) -> Self {
        WeicheSerialisiert { name, steuerung }
    }

    /// Erhalte [Name] und [Steuerung](WeicheSteuerung) einer [WeicheSerialisiert].
    pub fn name_und_steuerung(self) -> (Name, WeicheSteuerung<Richtung, Anschlüsse>) {
        let WeicheSerialisiert { name, steuerung } = self;
        (name, steuerung)
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: Richtung, schalten_zeit: Duration) -> Result<(), Fehler> {
        Self::schalten_aux(&mut self.steuerung, richtung, schalten_zeit, None::<fn()>)?;
        Ok(())
    }

    fn schalten_aux(
        mutex: &mut Arc<Mutex<Steuerung<WeicheSteuerung<Richtung, Anschlüsse>>>>,
        richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let mut guard = mutex.lock();
        let weiche_steuerung = guard.as_mut();
        let letzte_richtung = weiche_steuerung.letzte_richtung.clone();
        let aktuelle_richtung = weiche_steuerung.aktuelle_richtung.clone();
        weiche_steuerung.letzte_richtung = aktuelle_richtung.clone();
        weiche_steuerung.aktuelle_richtung = richtung.clone();
        drop(guard);
        if let Some(aktualisieren) = aktualisieren {
            aktualisieren()
        }
        macro_rules! bei_fehler_zurücksetzen {
            ($result: expr) => {
                if let Err(fehler) = $result {
                    let mut guard = mutex.lock();
                    let mut_ref = guard.as_mut();
                    mut_ref.aktuelle_richtung = aktuelle_richtung;
                    mut_ref.letzte_richtung = letzte_richtung;
                    return Err(fehler);
                }
            };
        }
        bei_fehler_zurücksetzen!(mutex
            .lock()
            .as_mut()
            .anschlüsse
            .erhalte_mut(&richtung)
            .einstellen(Fließend::Fließend));
        sleep(schalten_zeit);
        bei_fehler_zurücksetzen!(mutex
            .lock()
            .as_mut()
            .anschlüsse
            .erhalte_mut(&richtung)
            .einstellen(Fließend::Gesperrt));
        Ok(())
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone + Send + 'static,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send + 'static,
{
    /// Schalte eine [Weiche] auf die übergebene `Richtung`.
    pub fn async_schalten<Nachricht: Send + 'static>(
        &mut self,
        richtung: Richtung,
        schalten_zeit: Duration,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()> {
        let name_clone = self.name.clone();
        let sender_clone = sender.clone();
        let erzeuge_aktualisieren_nachricht_clone = erzeuge_aktualisieren_nachricht.clone();
        let sende_aktualisieren_nachricht =
            erzeuge_aktualisieren_nachricht_clone.map(|erzeuge_nachricht| {
                move || {
                    if let Err(fehler) = sender_clone.send(erzeuge_nachricht()) {
                        debug!(
                            "Kein Empfänger für Aktualisieren-Nachricht bei Schalten der Weiche {}: {:?}",
                            name_clone.0, fehler
                        )
                    }
                }
            });
        let name_clone = self.name.clone();
        let schalten_aux = Self::schalten_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |_mutex_clone, fehler| erzeuge_fehler_nachricht(fehler),
            format!("für Schalten der Weiche {}", name_clone.0),
            schalten_aux(self.steuerung, richtung, schalten_zeit, sende_aktualisieren_nachricht)
        )
    }
}

#[allow(single_use_lifetimes)]
impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
    <T as Serialisiere>::Serialisiert: Reserviere<T, Arg = ()>,
{
    type Serialisiert = WeicheSerialisiert<Richtung, T::Serialisiert>;

    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, T::Serialisiert> {
        WeicheSerialisiert {
            name: self.name.clone(),
            steuerung: {
                let guard = self.steuerung.lock();
                let weiche_steuerung = guard.as_ref();
                WeicheSteuerung {
                    aktuelle_richtung: weiche_steuerung.aktuelle_richtung.clone(),
                    letzte_richtung: weiche_steuerung.letzte_richtung.clone(),
                    anschlüsse: weiche_steuerung.anschlüsse.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.steuerung) {
            Ok(mutex) => mutex
                .into_inner()
                .konsumiere(|weiche_steuerung| weiche_steuerung.anschlüsse.anschlüsse()),
            Err(_arc) => {
                // while-Schleife (mit thread::yield bei Err) bis nur noch eine Arc-Referenz besteht
                // (Ok wird zurückgegeben) wäre möglich, kann aber zur nicht-Terminierung führen
                // Gebe stattdessen keine Anschlüsse zurück
                (Vec::new(), Vec::new(), Vec::new())
            },
        }
    }
}

#[allow(single_use_lifetimes)]
impl<Richtung, T, R> Reserviere<Weiche<Richtung, R>> for WeicheSerialisiert<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere,
    T: Reserviere<R, Arg = ()>,
{
    type Arg = Arc<Mutex<Cache>>;
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
        canvas: Arc<Mutex<Cache>>,
    ) -> de_serialisieren::Result<Weiche<Richtung, R>> {
        let BenannteWeiche {
            name,
            steuerung: WeicheSteuerung { aktuelle_richtung, letzte_richtung, anschlüsse },
        } = self;
        let reserviert = anschlüsse
            .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, ())?
            .konvertiere(|anschlüsse| {
                Weiche::neu(name, aktuelle_richtung, letzte_richtung, anschlüsse, canvas)
            });
        Ok(reserviert)
    }
}
