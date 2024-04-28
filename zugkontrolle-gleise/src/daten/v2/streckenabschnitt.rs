//! Serialisierbare Darstellung eines [`Streckenabschnittes`](Streckenabschnitt) in Version 2.

use std::collections::HashMap;

use serde::Deserialize;

use zugkontrolle_gleis::steuerung::streckenabschnitt as v4;
use zugkontrolle_typen::farbe::Farbe;

use crate::daten::v2::anschluss::OutputSerialisiert;

/// Serialisierbare Repräsentation der Steuerung der Stromzufuhr.
#[derive(Deserialize)]
pub(in crate::daten::v2) struct StreckenabschnittSerialisiert {
    /// Die Farbe des Streckenabschnittes.
    farbe: Farbe,
    /// Die Anschlüsse des Streckenabschnittes.
    anschluss: OutputSerialisiert,
}

impl From<StreckenabschnittSerialisiert> for v4::StreckenabschnittSerialisiert {
    fn from(input: StreckenabschnittSerialisiert) -> Self {
        let StreckenabschnittSerialisiert { farbe, anschluss } = input;
        v4::StreckenabschnittSerialisiert::neu_serialisiert(farbe, anschluss.into())
    }
}

/// Streckenabschnitte mit ihrem Namen.
pub(in crate::daten::v2) type StreckenabschnittMapSerialisiert =
    HashMap<Name, StreckenabschnittSerialisiert>;

/// Name eines [`Streckenabschnittes`](StreckenabschnittSerialisiert).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct Name(pub String);

impl From<Name> for v4::Name {
    fn from(wert: Name) -> Self {
        v4::Name(wert.0)
    }
}
