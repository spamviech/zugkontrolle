//! Kupplungs-Schienen zum trennen von Wagons.

use std::{thread::sleep, time::Duration};

use crate::anschluss::{Error, Fließend, OutputAnschluss};

pub struct Kupplung {
    // TODO name ist eigentlich nur für die Anzeige relevant
    pub name: String,
    pub anschluss: OutputAnschluss,
}

impl Kupplung {
    pub fn kuppeln(&mut self) -> Result<(), Error> {
        self.anschluss.einstellen(Fließend::Fließend)?;
        sleep(KUPPELZEIT);
        self.anschluss.einstellen(Fließend::Gesperrt)
    }
}

// TODO als Teil des Zugtyp-Traits?
const KUPPELZEIT: Duration = Duration::from_millis(500);
