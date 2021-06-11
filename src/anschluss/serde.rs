//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use super::{Anschlüsse, Error};

pub trait ToSave: Sized {
    type Save: Serialize + for<'de> Deserialize<'de> + Reserviere<Self>;

    fn to_save(&self) -> Self::Save;
}

pub trait Reserviere<R> {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<R, Error>;
}
