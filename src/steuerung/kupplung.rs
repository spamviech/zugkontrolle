//! Kupplungs-Schienen zum trennen von Wagons.

use std::{thread::sleep, time::Duration};

use crate::anschluss::{Error, Fließend, OutputAnschluss, Polarity};

pub struct Kupplung {
    // TODO name ist eigentlich nur für die Anzeige relevant
    pub name: String,
    pub anschluss: OutputAnschluss,
    pub polarität: Polarity,
}

impl Kupplung {
    pub fn kuppeln(&mut self) -> Result<(), Error> {
        self.anschluss.write(Fließend::Fließend.with_polarity(self.polarität))?;
        sleep(KUPPELZEIT);
        self.anschluss.write(Fließend::Gesperrt.with_polarity(self.polarität))
    }
}

// TODO als Teil des Zugtyp-Traits?
const KUPPELZEIT: Duration = Duration::from_millis(500);
