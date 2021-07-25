//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use crate::anschluss::{self, Anschlüsse};

pub trait ToSave: Sized {
    type Save: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    fn to_save(&self) -> Self::Save;
}

#[derive(Debug)]
pub struct Reserviert<R> {
    anschluss: R,
    nicht_benötigt: Vec<R>,
}

#[derive(Debug)]
pub struct Error<R> {
    fehler: anschluss::Error,
    bisherige_anschlüsse: Vec<R>,
}

pub type Result<R> = std::result::Result<Reserviert<R>, Error<R>>;

pub trait Reserviere<R> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        bisherige_anschlüsse: impl Iterator<Item = R>,
    ) -> Result<R>;
}
