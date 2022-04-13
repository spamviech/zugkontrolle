//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    mem,
    sync::{mpsc::Sender, Arc},
    thread::{sleep, JoinHandle},
    time::Duration,
};

use parking_lot::Mutex;
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        polarität::Fließend,
        Fehler, InputAnschluss, OutputAnschluss,
    },
    nachschlagen::Nachschlagen,
    steuerung::plan::async_ausführen,
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
    pub steuerung: Steuerung,
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
    BenannteWeiche<Arc<Mutex<WeicheSteuerung<Richtung, Anschlüsse>>>>;

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erstelle eine neue [Weichen-Steuerung](Weiche).
    pub fn neu(
        name: Name,
        aktuelle_richtung: Richtung,
        letzte_richtung: Richtung,
        anschlüsse: Anschlüsse,
    ) -> Self {
        Weiche {
            name,
            steuerung: Arc::new(Mutex::new(WeicheSteuerung {
                aktuelle_richtung,
                letzte_richtung,
                anschlüsse,
            })),
        }
    }
}

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
pub type WeicheSerialisiert<Richtung, Anschlüsse> =
    BenannteWeiche<WeicheSteuerung<Richtung, Anschlüsse>>;

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: Richtung, schalten_zeit: Duration) -> Result<(), Fehler> {
        Self::schalten_aux(&mut self.steuerung, richtung, schalten_zeit)?;
        Ok(())
    }

    fn schalten_aux(
        mutex: &mut Arc<Mutex<WeicheSteuerung<Richtung, Anschlüsse>>>,
        richtung: Richtung,
        schalten_zeit: Duration,
    ) -> Result<(), Fehler> {
        // TODO zurücksetzen bei fehler
        let mut guard = mutex.lock();
        guard.letzte_richtung = mem::replace(&mut guard.aktuelle_richtung, richtung.clone());
        drop(guard);
        mutex.lock().anschlüsse.erhalte_mut(&richtung).einstellen(Fließend::Fließend)?;
        sleep(schalten_zeit);
        mutex.lock().anschlüsse.erhalte_mut(&richtung).einstellen(Fließend::Gesperrt)?;
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
        erzeuge_nachricht: impl FnOnce(Fehler) -> Nachricht + Send + 'static,
    ) -> JoinHandle<()> {
        let name_clone = self.name.clone();
        let schalten = Self::schalten_aux;
        async_ausführen!(
            sender,
            |_mutex_clone, fehler| erzeuge_nachricht(fehler),
            format!("für Schalten der Weiche {}", name_clone.0),
            schalten(self.steuerung, richtung, schalten_zeit)
        )
    }
}

#[allow(single_use_lifetimes)]
impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = WeicheSerialisiert<Richtung, T::Serialisiert>;

    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, T::Serialisiert> {
        WeicheSerialisiert {
            name: self.name.clone(),
            steuerung: {
                let guard = self.steuerung.lock();
                WeicheSteuerung {
                    aktuelle_richtung: guard.aktuelle_richtung.clone(),
                    letzte_richtung: guard.letzte_richtung.clone(),
                    anschlüsse: guard.anschlüsse.serialisiere(),
                }
            },
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.steuerung) {
            Ok(mutex) => mutex.into_inner().anschlüsse.anschlüsse(),
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
    T: Reserviere<R>,
{
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Weiche<Richtung, R>> {
        let Reserviert {
            anschluss: anschlüsse,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.steuerung.anschlüsse.reserviere(
            lager,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(Reserviert {
            anschluss: Weiche::neu(
                self.name,
                self.steuerung.aktuelle_richtung,
                self.steuerung.letzte_richtung,
                anschlüsse,
            ),
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}
