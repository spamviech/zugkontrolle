//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, Anschlüsse};

pub trait ToSave<Anschluss>: Sized {
    type Save: Serialize + for<'de> Deserialize<'de> + Reserviere<Self, Anschluss>;

    fn to_save(&self) -> Self::Save;
}

#[derive(Debug)]
pub struct Reserviert<R, Anschluss> {
    anschluss: R,
    nicht_benötigt: Vec<Anschluss>,
}

#[derive(Debug)]
pub struct Error<R> {
    fehler: anschluss::Error,
    bisherige_anschlüsse: Vec<R>,
}

pub type Result<R, Anschluss> = std::result::Result<Reserviert<R, Anschluss>, Error<R>>;

pub trait Reserviere<R, Anschluss> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = R>,
    ) -> Result<R, Anschluss>;
}
