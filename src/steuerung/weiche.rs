//! Schaltbare Gleise.

use std::{fmt::Debug, hash::Hash, thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{
    pwm,
    speichern_laden::{self, Reserviere, Reserviert, ToSave},
    Anschlüsse, Error, Fließend, InputAnschluss, OutputAnschluss,
};
use crate::lookup::Lookup;

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

// inklusive Kreuzung
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Weiche<Richtung, Anschlüsse> {
    pub name: Name,
    pub aktuelle_richtung: Richtung,
    pub letzte_richtung: Richtung,
    pub anschlüsse: Anschlüsse,
}

impl<Richtung, Anschlüsse> Weiche<Richtung, Anschlüsse>
where
    Richtung: Clone,
    Anschlüsse: Lookup<Richtung, OutputAnschluss>,
{
    pub fn schalten(&mut self, richtung: &Richtung) -> Result<(), Error> {
        let anschluss = self.anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        self.letzte_richtung = self.aktuelle_richtung.clone();
        self.aktuelle_richtung = richtung.clone();
        Ok(())
    }
}

impl<Richtung, T> ToSave for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    T: ToSave,
{
    type Save = Weiche<Richtung, T::Save>;

    fn to_save(&self) -> Weiche<Richtung, T::Save> {
        Weiche {
            name: self.name.clone(),
            aktuelle_richtung: self.aktuelle_richtung.clone(),
            letzte_richtung: self.letzte_richtung.clone(),
            anschlüsse: self.anschlüsse.to_save(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschlüsse.anschlüsse()
    }
}
impl<Richtung, T, R> Reserviere<Weiche<Richtung, R>> for Weiche<Richtung, T>
where
    Richtung: Clone + Serialize + for<'de> Deserialize<'de>,
    R: ToSave,
    T: Reserviere<R>,
{
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> speichern_laden::Result<Weiche<Richtung, R>> {
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
                name: self.name,
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

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(400);
