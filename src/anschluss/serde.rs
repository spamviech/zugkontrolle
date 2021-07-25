//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use super::{Anschlüsse, Error};

pub trait ToSave: Sized {
    type Save: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    fn to_save(&self) -> Self::Save;
}

pub struct Reserviert<R> {
    anschluss: R,
    ersetzbar: Vec<R>,
}

pub trait Reserviere<R> {
    fn reserviere(
        self,
        anschlüsse: &mut Anschlüsse,
        ersetzbare_anschlüsse: impl Iterator<Item = R>,
    ) -> Result<Reserviert<R>, Error>;
}
