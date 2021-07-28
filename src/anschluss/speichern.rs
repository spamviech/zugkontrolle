//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, pwm, Anschlüsse, InputAnschluss, OutputAnschluss};

pub trait ToSave: Sized {
    type Save: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    fn to_save(&self) -> Self::Save;

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
pub struct Error {
    pub fehler: anschluss::Error,
    pub pwm_pins: Vec<pwm::Pin>,
    pub output_anschlüsse: Vec<OutputAnschluss>,
    pub input_anschlüsse: Vec<InputAnschluss>,
}

pub type Result<R> = std::result::Result<Reserviert<R>, Error>;

pub trait Reserviere<R> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        pwm_pins: Vec<pwm::Pin>,
        output_anschlüsse: Vec<OutputAnschluss>,
        input_anschlüsse: Vec<InputAnschluss>,
    ) -> Result<R>;
}
