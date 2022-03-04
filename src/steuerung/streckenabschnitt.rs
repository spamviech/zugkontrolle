//! Ein Streckenabschnitt regelt die Stromzufuhr.

use std::sync::Arc;

use parking_lot::{Mutex, MutexGuard};
use serde::{Deserialize, Serialize};

use crate::{
    anschluss::{
        self,
        de_serialisieren::{self, Reserviere, Reserviert, Serialisiere},
        pin::pwm,
        polarität::Fließend,
        Fehler, InputAnschluss, OutputAnschluss, OutputSerialisiert,
    },
    typen::farbe::Farbe,
};

/// Steuerung der Stromzufuhr.
#[derive(Debug, Clone)]
pub struct Streckenabschnitt {
    pub farbe: Farbe,
    anschluss: Arc<Mutex<OutputAnschluss>>,
}
/// Steuerung der Stromzufuhr.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreckenabschnittSerialisiert {
    pub farbe: Farbe,
    pub anschluss: OutputSerialisiert,
}

impl Streckenabschnitt {
    pub fn neu(farbe: Farbe, anschluss: OutputAnschluss) -> Self {
        Streckenabschnitt { farbe, anschluss: Arc::new(Mutex::new(anschluss)) }
    }

    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        self.lock_anschluss().einstellen(fließend)
    }

    pub fn strom_umschalten(&mut self) -> Result<(), Fehler> {
        self.lock_anschluss().umschalten()
    }

    #[inline(always)]
    pub(crate) fn lock_anschluss<'t>(&'t self) -> MutexGuard<'t, OutputAnschluss> {
        self.anschluss.lock()
    }
}

impl Serialisiere for Streckenabschnitt {
    type Serialisiert = StreckenabschnittSerialisiert;

    fn serialisiere(&self) -> StreckenabschnittSerialisiert {
        StreckenabschnittSerialisiert {
            farbe: self.farbe,
            anschluss: self.lock_anschluss().serialisiere(),
        }
    }

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>) {
        match Arc::try_unwrap(self.anschluss) {
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

impl Reserviere<Streckenabschnitt> for StreckenabschnittSerialisiert {
    fn reserviere(
        self,
        lager: &mut anschluss::Lager,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> de_serialisieren::Result<Streckenabschnitt> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self.anschluss.reserviere(lager, pwm_pins, output_anschlüsse, input_anschlüsse)?;
        Ok(Reserviert {
            anschluss: Streckenabschnitt::neu(self.farbe, anschluss),
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        })
    }
}

/// Name eines Streckenabschnittes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
