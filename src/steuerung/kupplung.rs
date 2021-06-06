//! Kupplungs-Schienen zum trennen von Wagons.

use std::collections::BTreeMap;
use std::{thread::sleep, time::Duration};

use serde::{Deserialize, Serialize};

use crate::anschluss::{Error, Fließend, OutputAnschluss};

pub struct Kupplung<Anschluss> {
    pub anschluss: Anschluss,
}

impl Kupplung<OutputAnschluss> {
    pub fn kuppeln(&mut self) -> Result<(), Error> {
        self.anschluss.einstellen(Fließend::Fließend)?;
        sleep(KUPPELZEIT);
        self.anschluss.einstellen(Fließend::Gesperrt)
    }
}

// TODO als Teil des Zugtyp-Traits?
const KUPPELZEIT: Duration = Duration::from_millis(500);

/// Name eines Kupplung.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschluss> = BTreeMap<Name, Kupplung<Anschluss>>;
