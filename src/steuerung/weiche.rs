//! Schaltbare Gleise.

use std::{
    fmt::Debug,
    hash::Hash,
    sync::{mpsc::Sender, Arc, Mutex},
    thread::{self, sleep},
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
    pwm, Anschlüsse, Fehler, Fließend, InputAnschluss, OutputAnschluss,
};
use crate::lookup::Lookup;

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Weiche<Richtung, Anschlüsse> {
    pub aktuelle_richtung: Richtung,
    pub letzte_richtung: Richtung,
    pub anschlüsse: Anschlüsse,
}

#[derive(Debug, Clone)]
pub struct BenannteWeiche<Richtung, Anschlüsse> {
    pub name: Name,
    pub weiche: Arc<Mutex<Weiche<Richtung, Anschlüsse>>>,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Lookup<Richtung, OutputAnschluss> + Send,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    pub fn schalten(&mut self, richtung: &Richtung) -> Result<(), Fehler> {
        let anschluss = self.anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        self.letzte_richtung = self.aktuelle_richtung.clone();
        self.aktuelle_richtung = richtung.clone();
        Ok(())
    }
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone + Send + 'static,
    Anschlüsse: Lookup<Richtung, OutputAnschluss> + Clone + Send + 'static,
{
    /// Schalte eine `Weiche` auf die übergebene `Richtung`.
    /// Ein dabei auftretender Fehler wird konvertiert und über den `Sender` geschickt.
    pub fn thread_schalten<Nachricht: From<Fehler> + Send + 'static>(
        &mut self,
        richtung: Richtung,
        sender: Sender<Nachricht>,
    ) {
        let clone = self.clone();
        thread::spawn(move || {
            if let Err(fehler) = clone.schalten(&richtung) {
                sender.send(fehler.into());
            }
        });
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenannteWeicheSerialisiert<Richtung, Anschlüsse> {
    pub name: Name,
    pub weiche: Weiche<Richtung, Anschlüsse>,
}

impl<Richtung, T> Serialisiere for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = Weiche<Richtung, T::Serialisiert>;

    fn serialisiere(&self) -> Weiche<Richtung, T::Serialisiert> {
        Weiche {
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self.anschlüsse.serialisiere(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschlüsse.anschlüsse()
    }
}
impl<Richtung, T, R> Reserviere<Weiche<Richtung, R>> for Weiche<Richtung, T>
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
            anschluss: Weiche {
                aktuelle_richtung: self.aktuelle_richtung.clone(),
                letzte_richtung: self.letzte_richtung.clone(),
                anschlüsse,
            },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

impl<Richtung, T> Serialisiere for BenannteWeiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: Serialisiere,
{
    type Serialisiert = BenannteWeicheSerialisiert<Richtung, T::Serialisiert>;

    fn serialisiere(&self) -> BenannteWeicheSerialisiert<Richtung, T::Serialisiert> {
        BenannteWeicheSerialisiert { name: self.name.clone(), weiche: self.weiche.serialisiere() }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.weiche.anschlüsse()
    }
}
impl<Richtung, T, R> Reserviere<BenannteWeiche<Richtung, R>>
    for BenannteWeicheSerialisiert<Richtung, T>
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
    ) -> de_serialisieren::Result<BenannteWeiche<Richtung, R>> {
        let Reserviert {
            anschluss: weiche,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.weiche.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: BenannteWeiche {
                name: self.name.clone(),
                weiche: Arc::new(Mutex::new(weiche)),
            },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(400);
