//! Ein Streckenabschnitt regelt die Stromzufuhr.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        Anschlüsse, Error, Fließend, InputAnschluss, OutputAnschluss, OutputSerialisiert,
    },
    farbe::Farbe,
};

pub type StreckenabschnittSerialisiert = Streckenabschnitt<OutputSerialisiert>;
/// Steuerung der Stromzufuhr.
#[derive(Debug, Serialize, Deserialize)]
pub struct Streckenabschnitt<Anschluss = OutputAnschluss> {
    pub farbe: Farbe,
    pub anschluss: Anschluss,
}

impl Streckenabschnitt<OutputAnschluss> {
    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Error> {
        self.anschluss.einstellen(fließend)
    }

    pub fn strom_umschalten(&mut self) -> Result<(), Error> {
        self.anschluss.umstellen()
    }
}

impl Serialisiere for Streckenabschnitt {
    type Serialisiert = StreckenabschnittSerialisiert;

    fn serialisiere(&self) -> Streckenabschnitt<OutputSerialisiert> {
        Streckenabschnitt { farbe: self.farbe, anschluss: self.anschluss.serialisiere() }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        self.anschluss.anschlüsse()
    }
}

impl Reserviere<Streckenabschnitt> for StreckenabschnittSerialisiert {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Streckenabschnitt> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.anschluss.reserviere(anschlüsse, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: Streckenabschnitt { farbe: self.farbe, anschluss },
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}
/// Name eines Streckenabschnittes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschluss = OutputAnschluss> = HashMap<Name, (Streckenabschnitt<Anschluss>, Fließend)>;
