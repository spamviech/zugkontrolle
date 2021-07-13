//! Ein Streckenabschnitt regelt die Stromzufuhr.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::anschluss::{Error, Fließend, OutputAnschluss, OutputSave};
use crate::farbe::Farbe;

pub type StreckenabschnittSave = Streckenabschnitt<OutputSave>;
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

/// Name eines Streckenabschnittes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(pub String);
pub type Map<Anschluss = OutputAnschluss> = HashMap<Name, (Streckenabschnitt<Anschluss>, Fließend)>;
