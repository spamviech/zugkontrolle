//! Ein Streckenabschnitt regelt die Stromzufuhr.

use crate::anschluss::{Error, Fließend, OutputAnschluss};
use crate::application::gleis::canvas::Color;

/// Steuerung der Stromzufuhr.
#[derive(Debug)]
pub struct Streckenabschnitt {
    // TODO name,farbe sind eigentlich nur für die Anzeige relevant
    pub name: String,
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
