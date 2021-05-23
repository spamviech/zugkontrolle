//! Schaltbare Gleise.

use std::{thread::sleep, time::Duration};

use non_empty_vec::NonEmpty;

use crate::anschluss::{Error, Fließend, OutputAnschluss};

// inklusive Kreuzung
pub struct Weiche<Richtung> {
    // TODO name ist eigentlich nur für die Anzeige relevant
    pub name: String,
    pub anschlüsse: NonEmpty<(Richtung, OutputAnschluss)>,
}

impl<Richtung: PartialEq> Weiche<Richtung> {
    pub fn schalten(&mut self, neue_richtung: &Richtung) -> Result<(), Error> {
        for (richtung, anschluss) in self.anschlüsse.as_mut_slice() {
            if &*richtung == neue_richtung {
                anschluss.einstellen(Fließend::Fließend)?;
                sleep(SCHALTZEIT);
                anschluss.einstellen(Fließend::Gesperrt)?;
            }
        }
        Ok(())
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);
