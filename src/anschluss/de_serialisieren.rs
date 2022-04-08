//! Traits zum serialisieren und reservieren der benötigten Anschlüsse.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

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

impl<R> Reserviert<R> {
    /// Konvertiere den `anschluss` anhand der übergebenen Funktion.
    pub fn konvertiere<T>(self, f: impl FnOnce(R) -> T) -> Reserviert<T> {
        let Reserviert {
            anschluss,
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        } = self;
        Reserviert {
            anschluss: f(anschluss),
            pwm_nicht_benötigt,
            output_nicht_benötigt,
            input_nicht_benötigt,
        }
    }
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
