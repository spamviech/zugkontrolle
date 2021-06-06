//! Konvertierung-Traits zu und von serealisierbaren Typen.

use serde::{Deserialize, Serialize};

use super::{Anschlüsse, Error};

pub trait ToSave<S: Serialize + for<'de> Deserialize<'de>> {
    fn to_save(&self) -> S;
}

pub trait Reserviere<R> {
    fn reserviere(self, anschlüsse: &mut Anschlüsse) -> Result<R, Error>;
}
