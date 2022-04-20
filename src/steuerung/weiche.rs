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
    nachschlagen::Nachschlagen,
    steuerung::plan::async_ausführen,
};

/// Name einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
/// Die [Steuerung](WeicheSteuerung) und der [Name] einer [Weiche].
#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Weiche<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle Richtung der Weiche.
    // TODO Steuerung verwenden, damit GUI-update getriggert wird
    pub steuerung: Arc<Mutex<WeicheSteuerung<Richtung, Anschlüsse>>>,
}

/// Die [Steuerung](WeicheSteuerung) einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSteuerung<Richtung, Anschlüsse> {
    /// Die aktuelle Richtung der Weiche.
    pub aktuelle_richtung: Richtung,
    /// Die Richtung vor der aktuellen Richtung.
    pub letzte_richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse> {
    /// Erstelle eine neue [Weiche].
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

impl<Richtung, Anschlüsse> AsMut<WeicheSteuerung<Richtung, Anschlüsse>>
    for WeicheSteuerung<Richtung, Anschlüsse>
{
    fn as_mut(&mut self) -> &mut WeicheSteuerung<Richtung, Anschlüsse> {
        self
    }
}

impl<Richtung, Anschlüsse> WeicheSteuerung<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    fn schalten_aux<S: AsMut<WeicheSteuerung<Richtung, Anschlüsse>>>(
        mutex: &Arc<Mutex<S>>,
        richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let mut_ref = mutex.lock().as_mut();
        let letzte_richtung = mut_ref.letzte_richtung.clone();
        let aktuelle_richtung = mut_ref.aktuelle_richtung.clone();
        mut_ref.letzte_richtung = aktuelle_richtung.clone();
        mut_ref.aktuelle_richtung = richtung.clone();
        drop(mut_ref);
        if let Some(aktualisieren) = aktualisieren {
            aktualisieren()
        }
        macro_rules! bei_fehler_zurücksetzen {
            ($result: expr) => {
                if let Err(fehler) = $result {
                    let mut_ref = mutex.lock().as_mut();
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
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: Richtung, schalten_zeit: Duration) -> Result<(), Fehler> {
        WeicheSteuerung::schalten_aux(&mut self.steuerung, richtung, schalten_zeit, None::<fn()>)?;
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
        let schalten_aux = WeicheSteuerung::schalten_aux;
        async_ausführen!(
            sender,
            erzeuge_aktualisieren_nachricht,
            |_mutex_clone, fehler| erzeuge_fehler_nachricht(fehler),
            format!("für Schalten der Weiche {}", name_clone.0),
            schalten_aux(
                &mut self.steuerung,
                richtung,
                schalten_zeit,
                sende_aktualisieren_nachricht
            )
        )
    }
}

/// Serialisierbare Repräsentation von [Name] und [Steuerung](WeicheSteuerung) einer [Weiche].
#[derive(zugkontrolle_macros::Debug, zugkontrolle_macros::Clone, Serialize, Deserialize)]
#[serde(bound(serialize = "Richtung: Serialize", deserialize = "Richtung: Deserialize<'de>"))]
#[zugkontrolle_debug(Richtung: Debug)]
#[zugkontrolle_debug(Anschlüsse: Serialisiere, <Anschlüsse as Serialisiere>::Serialisiert: Debug)]
#[zugkontrolle_clone(Richtung: Clone)]
#[zugkontrolle_clone(Anschlüsse: Serialisiere, <Anschlüsse as Serialisiere>::Serialisiert: Clone)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse: Serialisiere> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle Richtung der Weiche.
    pub steuerung: WeicheSteuerung<Richtung, <Anschlüsse as Serialisiere>::Serialisiert>,
}

impl<Richtung, Anschlüsse> PartialEq for WeicheSerialisiert<Richtung, Anschlüsse>
where
    Richtung: PartialEq,
    Anschlüsse: Serialisiere,
    <Anschlüsse as Serialisiere>::Serialisiert: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.steuerung == other.steuerung
    }
}

impl<Richtung, Anschlüsse> Eq for WeicheSerialisiert<Richtung, Anschlüsse>
where
    Richtung: Eq,
    Anschlüsse: Serialisiere,
    <Anschlüsse as Serialisiere>::Serialisiert: Eq,
{
}

impl<Richtung, Anschlüsse> Hash for WeicheSerialisiert<Richtung, Anschlüsse>
where
    Richtung: Hash,
    Anschlüsse: Serialisiere,
    <Anschlüsse as Serialisiere>::Serialisiert: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.steuerung.hash(state);
    }
}

impl<Richtung, Anschlüsse: Serialisiere> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [Weiche].
    pub fn neu(
        name: Name,
        aktuelle_richtung: Richtung,
        letzte_richtung: Richtung,
        anschlüsse: <Anschlüsse as Serialisiere>::Serialisiert,
    ) -> Self {
        WeicheSerialisiert {
            name,
            steuerung: WeicheSteuerung { aktuelle_richtung, letzte_richtung, anschlüsse },
        }
    }
}

#[allow(single_use_lifetimes)]
impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = WeicheSerialisiert<Richtung, T>;

    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, T> {
        let guard = self.steuerung.lock();
        let steuerung = WeicheSteuerung {
            aktuelle_richtung: guard.aktuelle_richtung.clone(),
            letzte_richtung: guard.letzte_richtung.clone(),
            anschlüsse: guard.anschlüsse.serialisiere(),
        };
        WeicheSerialisiert { name: self.name.clone(), steuerung }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.steuerung) {
            Ok(mutex) => mutex.into_inner().anschlüsse.anschlüsse(),
            Err(_) => (Vec::new(), Vec::new(), Vec::new()),
        }
    }
}

#[allow(single_use_lifetimes)]
impl<Richtung, R> Reserviere<Weiche<Richtung, R>> for WeicheSerialisiert<Richtung, R>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    R: Serialisiere,
{
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Weiche<Richtung, R>> {
        let WeicheSerialisiert {
            name,
            steuerung: WeicheSteuerung { aktuelle_richtung, letzte_richtung, anschlüsse },
        } = self;
        let reserviert = anschlüsse
            .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
            .konvertiere(|anschlüsse| {
                Weiche::neu(name, aktuelle_richtung, letzte_richtung, anschlüsse)
            });
        Ok(reserviert)
    }
}
