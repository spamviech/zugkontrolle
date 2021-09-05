//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    sync::{mpsc::Sender, Arc, Mutex, PoisonError},
    thread::{self, sleep},
    time::Duration,
};

use log::{debug, error};
use serde::{Deserialize, Serialize};

use crate::anschluss::{
    de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
    pwm, Anschlüsse, Fehler, Fließend, InputAnschluss, OutputAnschluss,
};
use crate::lookup::Lookup;

fn heile_poison<T>(poison_error: PoisonError<T>, name: &Name) -> T {
    error!("Anschlüsse-Mutex für Weiche {} poisoned!", name.0);
    poison_error.into_inner()
}

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
#[derive(Debug, Clone)]
pub struct Weiche<Richtung, Anschlüsse> {
    pub name: Name,
    pub aktuelle_richtung: Richtung,
    pub letzte_richtung: Richtung,
    anschlüsse: Arc<Mutex<Anschlüsse>>,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    pub fn neu(
        name: Name,
        aktuelle_richtung: Richtung,
        letzte_richtung: Richtung,
        anschlüsse: Anschlüsse,
    ) -> Self {
        Weiche {
            name,
            aktuelle_richtung,
            letzte_richtung,
            anschlüsse: Arc::new(Mutex::new(anschlüsse)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
    pub name: Name,
    pub aktuelle_richtung: Richtung,
    pub letzte_richtung: Richtung,
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Lookup<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: &Richtung) -> Result<(), Fehler> {
        Self::schalten_aux(&mut self.anschlüsse, &self.name, richtung)?;
        self.letzte_richtung = self.aktuelle_richtung.clone();
        self.aktuelle_richtung = richtung.clone();
        Ok(())
    }

    fn schalten_aux(
        mutex: &mut Arc<Mutex<Anschlüsse>>,
        name: &Name,
        richtung: &Richtung,
    ) -> Result<(), Fehler> {
        let mut anschlüsse = mutex.lock().unwrap_or_else(|poison_error| {
            error!("Anschlüsse-Mutex von Weiche {} poisoned!", name.0);
            poison_error.into_inner()
        });
        let anschluss = anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        Ok(())
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone + Send + 'static,
    Anschlüsse: Lookup<Richtung, OutputAnschluss> + Send + 'static,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn async_schalten<Nachricht: Send + 'static>(
        &mut self,
        richtung: Richtung,
        sender: Sender<Nachricht>,
        erzeuge_nachricht: impl FnOnce(Fehler) -> Nachricht + Send + 'static,
    ) {
        let name_clone = self.name.clone();
        let mut mutex_clone = self.anschlüsse.clone();
        let richtung_clone = richtung.clone();
        thread::spawn(move || {
            if let Err(fehler) = Self::schalten_aux(&mut mutex_clone, &name_clone, &richtung_clone)
            {
                let send_result = sender.send(erzeuge_nachricht(fehler));
                if let Err(fehler) = send_result {
                    debug!("Message-Channel für Weiche {} geschlossen: {:?}", name_clone.0, fehler)
                }
            }
        });
        self.letzte_richtung = self.aktuelle_richtung.clone();
        self.aktuelle_richtung = richtung;
    }
}

impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = WeicheSerialisiert<Richtung, T::Serialisiert>;

    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, T::Serialisiert> {
        WeicheSerialisiert {
            name: self.name.clone(),
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self
                .anschlüsse
                .lock()
                .unwrap_or_else(|poison_error| heile_poison(poison_error, &self.name))
                .serialisiere(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        let name = self.name;
        match Arc::try_unwrap(self.anschlüsse) {
            Ok(mutex) => mutex
                .into_inner()
                .unwrap_or_else(|poison_error| heile_poison(poison_error, &name))
                .anschlüsse(),
            Err(_arc) => {
                // while-Schleife (mit thread::yield bei Err) bis nur noch eine Arc-Referenz besteht
                // (Ok wird zurückgegeben) wäre möglich, kann aber zur nicht-Terminierung führen
                // Gebe stattdessen keine Anschlüsse zurück
                (Vec::new(), Vec::new(), Vec::new())
            }
        }
    }
}
impl<Richtung, T, R> Reserviere<Weiche<Richtung, R>> for WeicheSerialisiert<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere,
    T: Reserviere<R>,
{
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Weiche<Richtung, R>> {
        let Reserviert {
            anschluss: anschlüsse,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.anschlüsse.reserviere(
            anschlüsse,
            pwm_pins,
            output_anschlüsse,
            input_anschlüsse,
        )?;
        Ok(Reserviert {
            anschluss: Weiche::neu(
                self.name,
                self.aktuelle_richtung,
                self.letzte_richtung,
                anschlüsse,
            ),
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(400);
