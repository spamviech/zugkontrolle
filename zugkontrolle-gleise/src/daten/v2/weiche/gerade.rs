//! Serialisierbare Darstellung eines [`Weiche`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::daten::{
    v2::{
        weiche::orientierung::Orientierung,
        weiche::steuerung::{WeicheAnschlüsseSerialisiert, WeicheSteuerungSerialisiert},
    },
    v3,
};

/// Serialisierbare Repräsentation einer [`Weiche`](v3::weiche::gerade::Weiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct WeicheSerialisiert {
    /// Die Länge der Geraden der Weiche.
    länge: Skalar,
    /// Der Radius der Kurve einer Weiche.
    radius: Skalar,
    /// Der Winkel der Kurve einer Weiche.
    winkel: Winkel,
    /// Die Orientierung der Kurve einer Weiche.
    orientierung: Orientierung,
    /// Die Beschreibung der Weiche.
    beschreibung: Option<String>,
    /// Die Steuerung der Weiche.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<WeicheSerialisiert> for v3::weiche::gerade::WeicheSerialisiert {
    fn from(input: WeicheSerialisiert) -> Self {
        let WeicheSerialisiert { länge, radius, winkel, orientierung, beschreibung, steuerung } =
            input;
        v3::weiche::gerade::WeicheSerialisiert {
            länge,
            radius,
            winkel,
            orientierung,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}
