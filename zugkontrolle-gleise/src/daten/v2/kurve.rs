//! Serialisierbare Darstellung einer [`Kurve`] in Version 2.

use serde::Deserialize;

use zugkontrolle_typen::{skalar::Skalar, winkel::Winkel};

use crate::daten::{v2::anschluss::KontaktSerialisiert, v3};

/// Serialisierbare Repräsentation einer [`Kurve`](kurve::Kurve).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct KurveSerialisiert {
    /// Der Radius der Kurve.
    radius: Skalar,
    /// Der Winkel, wie lange die Kurve geht.
    winkel: Winkel,
    /// Die Beschreibung der Kurve.
    beschreibung: Option<String>,
    /// Der Kontakt assoziiert mit der Kurve.
    kontakt: Option<KontaktSerialisiert>,
}

impl From<KurveSerialisiert> for v3::kurve::KurveSerialisiert {
    fn from(input: KurveSerialisiert) -> Self {
        let KurveSerialisiert { radius, winkel, beschreibung, kontakt } = input;
        v3::kurve::KurveSerialisiert {
            radius,
            winkel,
            beschreibung,
            kontakt: kontakt.map(Into::into),
        }
    }
}
