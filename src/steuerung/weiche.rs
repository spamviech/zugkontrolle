//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    mem,
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
pub struct BenannteWeiche<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
    pub richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

// TODO nach dreiwege verschieben?
/// Die aktuelle und letzte Richtung einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheRichtung<Richtung> {
    /// Die aktuelle Richtung der Weiche.
    pub aktuelle_richtung: Richtung,
    /// Die Richtung vor der aktuellen Richtung.
    pub letzte_richtung: Richtung,
}

/// Steuerung und Name eines Schaltbaren Gleises.
pub type Weiche<Richtung, Anschlüsse> =
    BenannteWeiche<Arc<Mutex<Steuerung<Richtung>>>, Arc<Mutex<Anschlüsse>>>;

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erstelle eine neue [Weichen-Steuerung](Weiche).
    pub fn neu(
        name: Name,
        richtung: Richtung,
        anschlüsse: Anschlüsse,
        cache: Arc<Mutex<Cache>>,
    ) -> Self {
        Weiche {
            name,
            richtung: Arc::new(Mutex::new(Steuerung::neu(richtung, cache))),
            anschlüsse: Arc::new(Mutex::new(anschlüsse)),
        }
    }
}

impl<Richtung: Clone, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erhalte die aktuelle Richtung einer [Weiche].
    pub fn richtung(&self) -> Richtung {
        self.richtung.lock().as_ref().clone()
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erhalte einen Teil der aktuellen Richtung einer [Weiche].
    pub fn richtung_ausschnitt<T: Clone>(&self, f: impl FnOnce(&Richtung) -> &T) -> T {
        f(self.richtung.lock().as_ref()).clone()
    }
}

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
pub type WeicheSerialisiert<Richtung, Anschlüsse> = BenannteWeiche<Richtung, Anschlüsse>;

impl<Richtung, Anschlüsse> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [WeicheSerialisiert].
    pub fn neu_serialisiert(name: Name, richtung: Richtung, anschlüsse: Anschlüsse) -> Self {
        WeicheSerialisiert { name, richtung, anschlüsse }
    }

    /// Erhalte [Name] und Richtung und Anschlüsse einer [WeicheSerialisiert].
    pub fn name_und_steuerung(self) -> (Name, Richtung, Anschlüsse) {
        let WeicheSerialisiert { name, richtung, anschlüsse } = self;
        (name, richtung, anschlüsse)
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss>,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: Richtung, schalten_zeit: Duration) -> Result<(), Fehler> {
        Self::schalten_aux(
            &self.richtung,
            &self.anschlüsse,
            richtung,
            schalten_zeit,
            None::<fn()>,
        )?;
        Ok(())
    }

    fn schalten_aux(
        richtung: &Arc<Mutex<Steuerung<Richtung>>>,
        anschlüsse: &Arc<Mutex<Anschlüsse>>,
        neue_richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let mut guard = richtung.lock();
        let weiche_richtung = guard.as_mut();
        let bisherige_richtung = mem::replace(weiche_richtung, neue_richtung.clone());
        drop(guard);
        if let Some(aktualisieren) = aktualisieren {
            aktualisieren()
        }
        macro_rules! bei_fehler_zurücksetzen {
            ($result: expr) => {
                if let Err(fehler) = $result {
                    let _ = mem::replace(weiche_richtung, bisherige_richtung);
                    return Err(fehler);
                }
            };
        }
        bei_fehler_zurücksetzen!(anschlüsse
            .lock()
            .erhalte_mut(&neue_richtung)
            .einstellen(Fließend::Fließend));
        sleep(schalten_zeit);
        bei_fehler_zurücksetzen!(anschlüsse
            .lock()
            .erhalte_mut(&neue_richtung)
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
        let schalten_aux = |(richtung, anschlüsse): &mut _,
                            neue_richtung,
                            schalten_zeit,
                            sende_aktualisieren_nachricht| {
            Self::schalten_aux(
                richtung,
                anschlüsse,
                neue_richtung,
                schalten_zeit,
                sende_aktualisieren_nachricht,
            )
        };
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |_mutex_clone, fehler| erzeuge_fehler_nachricht(fehler),
            format!("für Schalten der Weiche {}", name_clone.0),
            schalten_aux(
                (self.richtung.clone(), self.anschlüsse.clone()),
                richtung,
                schalten_zeit,
                sende_aktualisieren_nachricht
            )
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
            richtung: self.richtung.lock().as_ref().clone(),
            anschlüsse: self.anschlüsse.lock().serialisiere(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.anschlüsse) {
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
        let BenannteWeiche { name, richtung, anschlüsse } = self;
        let reserviert = anschlüsse
            .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, ())?
            .konvertiere(|anschlüsse| Weiche::neu(name, richtung, anschlüsse, canvas));
        Ok(reserviert)
    }
}
