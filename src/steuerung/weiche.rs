//! Schaltbare Gleise.

use std::{collections::HashMap, thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{Error, Fließend, OutputAnschluss};
use crate::lookup::Lookup;

// inklusive Kreuzung
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Weiche<Anschlüsse> {
    pub anschlüsse: Anschlüsse,
}

impl<Anschlüsse> Weiche<Anschlüsse> {
    pub fn schalten<Richtung>(&mut self, richtung: &Richtung) -> Result<(), Error>
    where
        Anschlüsse: Lookup<Richtung, OutputAnschluss>,
    {
        let anschluss = self.anschlüsse.get_mut(richtung);
        anschluss.einstellen(Fließend::Fließend)?;
        sleep(SCHALTZEIT);
        anschluss.einstellen(Fließend::Gesperrt)?;
        Ok(())
    }
}

// TODO als Teil des Zugtyp-Traits?
const SCHALTZEIT: Duration = Duration::from_millis(500);

/// Name einer Weiche.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschlüsse> = HashMap<Name, Weiche<Anschlüsse>>;
