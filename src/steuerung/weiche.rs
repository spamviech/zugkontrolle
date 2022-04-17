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
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Weiche<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    pub name: Name,
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
        Weiche { name, aktuelle_richtung, letzte_richtung, anschlüsse }
    }
}

impl<Richtung, Anschlüsse> AsMut<Weiche<Richtung, Anschlüsse>> for Weiche<Richtung, Anschlüsse> {
    fn as_mut(&mut self) -> &mut Weiche<Richtung, Anschlüsse> {
        self
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Nachschlagen<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: Richtung, schalten_zeit: Duration) -> Result<(), Fehler> {
        Self::schalten_aux(self, richtung, schalten_zeit, None::<fn()>)?;
        Ok(())
    }

    fn schalten_aux<S: AsMut<Weiche<Richtung, Anschlüsse>>>(
        s: S,
        richtung: Richtung,
        schalten_zeit: Duration,
        aktualisieren: Option<impl FnOnce()>,
    ) -> Result<(), Fehler> {
        let mut_ref = s.as_mut();
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
                    let mut_ref = s.as_mut();
                    mut_ref.aktuelle_richtung = aktuelle_richtung;
                    mut_ref.letzte_richtung = letzte_richtung;
                    return Err(fehler);
                }
            };
        }
        bei_fehler_zurücksetzen!(s
            .as_mut()
            .anschlüsse
            .erhalte_mut(&richtung)
            .einstellen(Fließend::Fließend));
        sleep(schalten_zeit);
        bei_fehler_zurücksetzen!(s
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

/// Serialisierbare Repräsentation der Steuerung einer [Weiche].
pub type WeicheSerialisiert<Richtung, Anschlüsse> =
    Weiche<Richtung, <Anschlüsse as Serialisiere>::Serialisiert>;

#[allow(single_use_lifetimes)]
impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = WeicheSerialisiert<Richtung, T>;

    fn serialisiere(&self) -> WeicheSerialisiert<Richtung, T> {
        WeicheSerialisiert {
            name: self.name.clone(),
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self.anschlüsse.serialisiere(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschlüsse.anschlüsse()
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
        let Weiche { name, aktuelle_richtung, letzte_richtung, anschlüsse } = self;
        let reserviert = anschlüsse
            .reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?
            .konvertiere(|anschlüsse| {
                Weiche::neu(name, aktuelle_richtung, letzte_richtung, anschlüsse)
            });
        Ok(reserviert)
    }
}
