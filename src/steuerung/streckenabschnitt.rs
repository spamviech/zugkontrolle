//! Ein Streckenabschnitt regelt die Stromzufuhr.

use crate::anschluss::{Error, Fließend, OutputAnschluss, Polarity};
use crate::gleis::canvas;

/// Steuerung der Stromzufuhr.
#[derive(Debug)]
pub struct Streckenabschnitt {
    // TODO name,farbe sind eigentlich nur für die Anzeige relevant
    pub name: String,
    pub farbe: canvas::Color,
    pub anschluss: OutputAnschluss,
    pub polarität: Polarity,
}

impl Streckenabschnitt {
    pub fn strom(&mut self, fließend: Fließend) -> Result<(), Error> {
        self.anschluss.write(fließend.with_polarity(self.polarität))
    }

    pub fn strom_umschalten(&mut self) -> Result<(), Error> {
        self.anschluss.toggle()
    }
}
