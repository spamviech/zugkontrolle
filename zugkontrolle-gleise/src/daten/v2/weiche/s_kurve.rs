//! Serialisierbare Darstellung einer [`SKurvenWeiche`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::daten::{
    v2::{
        weiche::orientierung::Orientierung,
        weiche::steuerung::{WeicheAnschlüsseSerialisiert, WeicheSteuerungSerialisiert},
    },
    v3,
};

/// Serialisierbare Repräsentation einer [`SKurvenWeiche`](s_kurve::SKurvenWeiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct SKurvenWeicheSerialisiert {
    /// Die Länge der Gerade der Weiche.
    länge: Skalar,
    /// Der Radius der nach außen gehenden Kurve der Weiche.
    radius: Skalar,
    /// Der Winkel der nach außen gehenden Kurve der Weiche.
    winkel: Winkel,
    /// Der Radius der nach innen gehenden Kurve der Weiche.
    radius_reverse: Skalar,
    /// Der Winkel der nach innen gehenden Kurve der Weiche.
    winkel_reverse: Winkel,
    /// Die Orientierung der Kurve der Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<SKurvenWeicheSerialisiert> for v3::weiche::s_kurve::SKurvenWeicheSerialisiert {
    fn from(input: SKurvenWeicheSerialisiert) -> Self {
        let SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_reverse,
            winkel_reverse,
            orientierung,
            beschreibung,
            steuerung,
        } = input;
        v3::weiche::s_kurve::SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen: radius_reverse,
            winkel_kurve_nach_innen: winkel_reverse,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}
