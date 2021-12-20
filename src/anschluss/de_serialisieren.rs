//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, pwm, InputAnschluss, OutputAnschluss};

pub trait Serialisiere: Sized {
    #[allow(single_use_lifetimes)]
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
        lager: &mut anschluss::Lager,
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

impl<T> AnschlussOderSerialisiert<T>
where
    T: Serialisiere,
    <T as Serialisiere>::Serialisiert: Clone,
{
    pub(crate) fn entferne_anschluss(&mut self) -> AnschlussOderSerialisiert<T> {
        let serialisiert = self.serialisiere();
        std::mem::replace(self, AnschlussOderSerialisiert::Serialisiert(serialisiert))
    }

    pub fn serialisiere(&self) -> T::Serialisiert {
        match self {
            AnschlussOderSerialisiert::Anschluss(anschluss) => anschluss.serialisiere(),
            AnschlussOderSerialisiert::Serialisiert(serialisiert) => serialisiert.clone(),
        }
    }
}
