//! Serialisierbare Darstellung einer [`DreiwegeWeiche`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::daten::{
    v2::{anschluss::OutputSerialisiert, weiche::steuerung::WeicheSteuerungSerialisiert},
    v3,
};

/// Serialisierbare Repräsentation der [`Anschlüsse`](dreiwege::RichtungAnschlüsse)
/// einer [`DreiwegeWeiche`](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct DreiwegeAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die Gerade.
    gerade: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die linke Kurve.
    links: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die rechte Kurve.
    rechts: OutputSerialisiert,
}

impl From<DreiwegeAnschlüsseSerialisiert>
    for v3::weiche::dreiwege::RichtungAnschlüsseSerialisiert
{
    fn from(input: DreiwegeAnschlüsseSerialisiert) -> Self {
        let DreiwegeAnschlüsseSerialisiert { gerade, links, rechts } = input;
        v3::weiche::dreiwege::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            links: links.into(),
            rechts: rechts.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [`DreiwegeWeiche`](dreiwege::DreiwegeWeiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct DreiwegeWeicheSerialisiert {
    /// Die Länge der Gerade der Weiche.
    länge: Skalar,
    /// Der Radius der Kurven der Weiche.
    radius: Skalar,
    /// Der Winkel der Kurven der Weiche
    winkel: Winkel,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::dreiwege::Richtung, DreiwegeAnschlüsseSerialisiert>,
    >,
}

impl From<DreiwegeWeicheSerialisiert> for v3::weiche::dreiwege::DreiwegeWeicheSerialisiert {
    fn from(input: DreiwegeWeicheSerialisiert) -> Self {
        let DreiwegeWeicheSerialisiert { länge, radius, winkel, beschreibung, steuerung } = input;
        v3::weiche::dreiwege::DreiwegeWeicheSerialisiert {
            länge,
            radius,
            winkel,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}
