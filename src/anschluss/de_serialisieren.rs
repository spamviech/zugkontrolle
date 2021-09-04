//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, pwm, Anschlüsse, InputAnschluss, OutputAnschluss};

pub trait Serialisiere: Sized {
    type Serialisiert: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    fn serialisiere(&self) -> Self::Serialisiert;

    fn anschlüsse(self) -> (Vec<pwm::Pin>, Vec<OutputAnschluss>, Vec<InputAnschluss>);
}

#[derive(Debug)]
pub struct Reserviert<R> {
    pub anschluss: R,
    pub pwm_nicht_benötigt: Vec<pwm::Pin>,
    pub output_nicht_benötigt: Vec<OutputAnschluss>,
    pub input_nicht_benötigt: Vec<InputAnschluss>,
}

#[derive(Debug)]
pub struct Fehler {
    pub fehler: anschluss::Fehler,
    pub pwm_pins: Vec<pwm::Pin>,
    pub output_anschlüsse: Vec<OutputAnschluss>,
    pub input_anschlüsse: Vec<InputAnschluss>,
}

pub type Result<R> = std::result::Result<Reserviert<R>, Fehler>;

pub trait Reserviere<R> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<R>;
}

#[derive(Debug)]
pub enum AnschlussOderSerialisiert<T: Serialisiere> {
    Anschluss(T),
    Serialisiert(T::Serialisiert),
}

impl<T: Serialisiere> AnschlussOderSerialisiert<T> {
    pub(crate) fn entferne_anschluss(&mut self) -> AnschlussOderSerialisiert<T> {
        let serialisiert = self.serialisiere();
        std::mem::replace(self, T::Serialisiert(serialisiert))
    }

    pub fn serialisiere(&self) -> T::Serialisiert {
        match self {
            AnschlussOderSerialisiert::Anschluss(anschluss) => anschluss.serialisiere(),
            AnschlussOderSerialisiert::Serialisiert(serialisiert) => serialisiert.clone(),
        }
    }
}
