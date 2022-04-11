//! Ein [Streckenabschnitt] regelt die Stromzufuhr.

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

/// Name eines [Streckenabschnittes](Streckenabschnitt).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);

/// Steuerung der Stromzufuhr.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Streckenabschnitt<Anschluss = Arc<Mutex<OutputAnschluss>>> {
    /// Die Farbe des Streckenabschnittes.
    pub farbe: Farbe,
    /// Die Anschlüsse des Streckenabschnittes.
    anschluss: Anschluss,
}

impl Streckenabschnitt {
    /// Erstelle einen neuen [Streckenabschnitt].
    pub fn neu(farbe: Farbe, anschluss: OutputAnschluss) -> Self {
        Streckenabschnitt { farbe, anschluss: Arc::new(Mutex::new(anschluss)) }
    }

    /// Schalte den Strom für einen [Streckenabschnitt].
    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Fehler> {
        self.lock_anschluss().einstellen(fließend)
    }

    /// Schalte den Strom eines [Streckenabschnittes](Streckenabschnitt).
    /// von [Fließend](Fließend::Fließend) auf [Gesperrt](Fließend::Gesperrt) und umgekehrt.
    pub fn strom_umschalten(&mut self) -> Result<(), Fehler> {
        self.lock_anschluss().umschalten()
    }

    /// Aktuelle Einstellung eines [Streckenabschnittes](Streckenabschnitt).
    pub fn fließend(&self) -> Fließend {
        self.lock_anschluss().fließend()
    }

    #[inline(always)]
    pub(crate) fn lock_anschluss<'t>(&'t self) -> MutexGuard<'t, OutputAnschluss> {
        self.anschluss.lock()
    }
}

/// Serialisierbare Repräsentation der Steuerung der Stromzufuhr.
pub type StreckenabschnittSerialisiert = Streckenabschnitt<OutputSerialisiert>;

impl StreckenabschnittSerialisiert {
    /// Erstelle einen neuen [StreckenabschnittSerialisiert].
    pub fn neu_serialisiert(farbe: Farbe, anschluss: OutputSerialisiert) -> Self {
        Streckenabschnitt { farbe, anschluss }
    }

    /// Der Anschluss des Streckenabschnittes.
    pub fn anschluss(self) -> OutputSerialisiert {
        self.anschluss
    }

    /// Eine Referenz des Anschlusses des Streckenabschnittes.
    pub fn anschluss_ref(&self) -> &OutputSerialisiert {
        &self.anschluss
    }

    /// Eine veränderliche Referenz des Anschlusses des Streckenabschnittes.
    pub fn anschluss_mut(&mut self) -> &mut OutputSerialisiert {
        &mut self.anschluss
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
