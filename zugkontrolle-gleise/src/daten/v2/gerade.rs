//! Serialisierbare Darstellung einer [`Gerade`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::skalar::Skalar;

use crate::daten::{v2::anschluss::KontaktSerialisiert, v3};

/// Serialisierbare Repräsentation einer [`Gerade`](gerade::Gerade).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct GeradeSerialisiert {
    /// Die Länge der Geraden.
    länge: Skalar,
    /// Die Beschreibung der Geraden.
    beschreibung: Option<String>,
    /// Der mit der Geraden assoziierte Kontakt.
    kontakt: Option<KontaktSerialisiert>,
}

impl From<GeradeSerialisiert> for v3::gerade::GeradeSerialisiert {
    fn from(input: GeradeSerialisiert) -> Self {
        let GeradeSerialisiert { länge, beschreibung, kontakt } = input;
        v3::gerade::GeradeSerialisiert { länge, beschreibung, kontakt: kontakt.map(Into::into) }
    }
}
