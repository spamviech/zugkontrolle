//! Einstellen der Geschwindigkeit.

use non_empty_vec::NonEmpty;

use crate::anschluss::*;

#[derive(Debug)]
pub struct Geschwindigkeit<Leiter> {
    pub name: String,
    pub leiter: Leiter,
}

#[derive(Debug)]
pub enum Mittelleiter {
    Pwm { anschluss: pwm::Pin },
    KonstanteSpannung { geschwindigkeit: NonEmpty<OutputAnschluss>, umdrehen: OutputAnschluss },
}

impl Geschwindigkeit<Mittelleiter> {
    pub fn geschwindigkeit(&mut self, geschwindigkeit: u8) -> Result<(), Error> {
        todo!()
    }

    pub fn umdrehen(&mut self) -> Result<(), Error> {
        todo!()
    }
}

#[derive(Debug)]
pub enum Zweileiter {
    Pwm { geschwindigkeit: pwm::Pin, fahrtrichtung: OutputAnschluss },
    KonstanteSpannung { geschwindigkeit: NonEmpty<OutputAnschluss>, fahrtrichtung: OutputAnschluss },
}

impl Geschwindigkeit<Zweileiter> {
    pub fn geschwindigkeit(&mut self, geschwindigkeit: u8) -> Result<(), Error> {
        todo!()
    }

    pub fn fahrtrichtung(&mut self, fahrtrichtung: Fahrtrichtung) -> Result<(), Error> {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Fahrtrichtung {
    Vorwärts,
    Rückwärts,
}
