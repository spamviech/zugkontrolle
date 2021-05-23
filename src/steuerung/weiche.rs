//! Schaltbare Gleise.

use std::{thread::sleep, time::Duration};

use crate::anschluss::{Error, Fließend, OutputAnschluss, Polarity};

// inklusive Kreuzung
pub struct Weiche<Richtung> {
    // TODO name ist eigentlich nur für die Anzeige relevant
    pub name: String,
    // TODO non-empty-vec hat Probleme bei iter_mut
    pub anschlüsse: Vec<(Richtung, OutputAnschluss, Polarity)>,
}

impl<Richtung: PartialEq> Weiche<Richtung> {
    pub fn schalten(&mut self, neue_richtung: &Richtung) -> Result<(), Error> {
        for (richtung, anschluss, polarität) in self.anschlüsse.iter_mut() {
            if &*richtung == neue_richtung {
                anschluss.write(Fließend::Fließend.with_polarity(polarität.clone()))?;
                sleep(SCHALTZEIT);
                anschluss.write(Fließend::Gesperrt.with_polarity(polarität.clone()))?;
            }
        }
        Ok(())
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);
