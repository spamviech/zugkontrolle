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
    typen::{canvas::Cache, MitRichtung},
};

/// Name einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
/// [Name], aktuelle Richtung und Anschlüsse einer Weiche.
#[derive(Debug, zugkontrolle_macros::Clone)]
pub struct Weiche<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
    richtung: Arc<Mutex<Steuerung<Richtung>>>,
    /// Die Anschlüsse der Weiche.
    anschlüsse: Arc<Mutex<Anschlüsse>>,
}

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

/// Notwendige Information zum ermitteln der nächsten Richtung einer Weiche.
pub trait WeicheSteuerung<R>: MitRichtung<R> {
    /// Notwendige Information zum zurücksetzen bei Schalt-Fehler.
    type Zurücksetzen;

    /// Einstellen einer neuen Richtung.
    fn einstellen(&mut self, neue_richtung: R) -> Self::Zurücksetzen;

    /// Zurücksetzen nach einem Schalt-Fehler.
    fn zurücksetzen(&mut self, zurücksetzen: Self::Zurücksetzen);
}

impl<R: MitRichtung<R>> WeicheSteuerung<R> for R {
    type Zurücksetzen = R;

    fn einstellen(&mut self, neue_richtung: R) -> Self::Zurücksetzen {
        mem::replace(self, neue_richtung)
    }

    fn zurücksetzen(&mut self, zurücksetzen: Self::Zurücksetzen) {
        *self = zurücksetzen
    }
}

impl<T, Anschlüsse> Weiche<T, Anschlüsse> {
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten<Richtung>(
        &mut self,
        richtung: Richtung,
        schalten_zeit: Duration,
    ) -> Result<(), Fehler>
    where
        T: WeicheSteuerung<Richtung>,
        Richtung: Clone,
        Anschlüsse: Nachschlagen<Richtung, OutputAnschluss>,
    {
        Self::schalten_aux(
            &self.richtung,
            &self.anschlüsse,
            richtung,
            schalten_zeit,
            None::<fn()>,
        )?;
        Ok(())
    }

    fn schalten_aux<Richtung>(
        richtung: &Arc<Mutex<Steuerung<T>>>,
        anschlüsse: &Arc<Mutex<Anschlüsse>>,
        neue_richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler>
    where
        T: WeicheSteuerung<Richtung>,
        Richtung: Clone,
        Anschlüsse: Nachschlagen<Richtung, OutputAnschluss>,
    {
        let mut richtung_guard = richtung.lock();
        let weiche_richtung = richtung_guard.as_mut();
        let richtung_zurücksetzen = weiche_richtung.einstellen(neue_richtung.clone());
        // richtung_guard freigeben, damit Zeichnen nicht blockiert wird.
        drop(richtung_guard);
        if let Some(aktualisieren) = aktualisieren {
            aktualisieren()
        }
        macro_rules! bei_fehler_zurücksetzen {
            ($result: expr) => {
                if let Err(fehler) = $result {
                    let mut richtung_guard = richtung.lock();
                    let weiche_richtung = richtung_guard.as_mut();
                    weiche_richtung.zurücksetzen(richtung_zurücksetzen);
                    return Err(fehler);
                }
            };
        }
        // Reserviere die Anschlüsse bis der gesamte Schaltvorgang abgeschlossen ist.
        let mut anschlüsse_guard = anschlüsse.lock();
        bei_fehler_zurücksetzen!(anschlüsse_guard
            .erhalte_mut(&neue_richtung)
            .einstellen(Fließend::Fließend));
        sleep(schalten_zeit);
        bei_fehler_zurücksetzen!(anschlüsse_guard
            .erhalte_mut(&neue_richtung)
            .einstellen(Fließend::Gesperrt));
        Ok(())
    }
}

impl<T, Anschlüsse> Weiche<T, Anschlüsse> {
    /// Schalte eine [Weiche] auf die übergebene `Richtung`.
    pub fn async_schalten<Richtung, Nachricht>(
        &mut self,
        richtung: Richtung,
        schalten_zeit: Duration,
        sender: Sender<Nachricht>,
        erzeuge_aktualisieren_nachricht: Option<
            impl 'static + FnOnce() -> Nachricht + Clone + Send,
        >,
        erzeuge_fehler_nachricht: impl 'static + FnOnce(Fehler) -> Nachricht + Send,
    ) -> JoinHandle<()>
    where
        T: 'static + WeicheSteuerung<Richtung> + Send,
        Richtung: 'static + Clone + Send,
        Anschlüsse: 'static + Nachschlagen<Richtung, OutputAnschluss> + Send,
        Nachricht: 'static + Send,
    {
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

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
    /// Die aktuelle und eventuell weitere Richtungen einer [Weiche].
    pub richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> WeicheSerialisiert<Richtung, Anschlüsse> {
    /// Erstelle eine neue [WeicheSerialisiert].
    pub fn neu(name: Name, richtung: Richtung, anschlüsse: Anschlüsse) -> Self {
        WeicheSerialisiert { name, richtung, anschlüsse }
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
    ) -> de_serialisieren::Ergebnis<Weiche<Richtung, R>> {
        let WeicheSerialisiert { name, richtung, anschlüsse } = self;
        anschlüsse
            .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse, ())
            .konvertiere(|anschlüsse| Weiche::neu(name, richtung, anschlüsse, canvas))
    }
}
