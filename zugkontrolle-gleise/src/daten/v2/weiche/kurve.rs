//! Serialisierbare Darstellung eines [`KurvenWeiche`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::daten::{
    v2::{
        anschluss::OutputSerialisiert, weiche::orientierung::Orientierung,
        weiche::steuerung::WeicheSteuerungSerialisiert,
    },
    v3,
};

/// Serialisierbare Repräsentation der [`Anschlüsse`](kurven_weiche::RichtungAnschlüsse)
/// einer [`KurvenWeiche`](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct KurvenWeicheAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die innere Kurve.
    innen: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die äußere Kurve.
    außen: OutputSerialisiert,
}

impl From<KurvenWeicheAnschlüsseSerialisiert>
    for v3::weiche::kurve::RichtungAnschlüsseSerialisiert
{
    fn from(input: KurvenWeicheAnschlüsseSerialisiert) -> Self {
        let KurvenWeicheAnschlüsseSerialisiert { innen, außen } = input;
        v3::weiche::kurve::RichtungAnschlüsseSerialisiert {
            innen: innen.into(),
            außen: außen.into(),
        }
    }
}

/// Serialisierbare Repräsentation einer [`KurvenWeiche`](kurven_weiche::KurvenWeiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct KurvenWeicheSerialisiert {
    /// Die Länge der Geraden, bevor die äußere Kurve beginnt.
    länge: Skalar,
    /// Der Radius der Kurven.
    radius: Skalar,
    /// Der Winkel der Kurven.
    winkel: Winkel,
    /// Die Orientierung der Kurven der Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<
            v3::weiche::kurve::Richtung,
            KurvenWeicheAnschlüsseSerialisiert,
        >,
    >,
}

impl From<KurvenWeicheSerialisiert> for v3::weiche::kurve::KurvenWeicheSerialisiert {
    fn from(input: KurvenWeicheSerialisiert) -> Self {
        let KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung,
        } = input;
        v3::weiche::kurve::KurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}
