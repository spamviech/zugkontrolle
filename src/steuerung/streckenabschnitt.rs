//! Ein Streckenabschnitt regelt die Stromzufuhr.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::anschluss::{Error, Fließend, OutputAnschluss};
use crate::application::typen::canvas::Color;

/// Steuerung der Stromzufuhr.
#[derive(Debug)]
pub struct Streckenabschnitt {
    // TODO name,farbe sind eigentlich nur für die Anzeige relevant
    pub farbe: Color,
    pub anschluss: OutputAnschluss,
}

impl Streckenabschnitt {
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
pub type Map = HashMap<Name, Streckenabschnitt>;
