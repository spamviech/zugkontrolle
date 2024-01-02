//! Serialisierbare Darstellung einer [SKurvenWeiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gleise::daten::v3::weiche::{gerade, orientierung::Orientierung, steuerung},
        weiche::s_kurve as v4,
    },
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<gerade::Richtung, gerade::RichtungAnschlüsseSerialisiert>;

/// Definition einer Weiche mit S-Kurve.
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SKurvenWeicheSerialisiert<Anschlüsse = Option<AnschlüsseSerialisiert>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Radius der Kurve nach außen.
    pub radius: Skalar,
    /// Der Winkel der Kurve nach außen.
    pub winkel: Winkel,
    /// Der Radius der Kurve nach innen.
    pub radius_kurve_nach_innen: Skalar,
    /// Der Winkel der Kurve nach innen.
    pub winkel_kurve_nach_innen: Winkel,
    /// Die Orientierung der SKurvenWeiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der SKurvenWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der SKurvenWeiche.
    pub steuerung: Anschlüsse,
}

/// Eine Variante ohne Anschlüsse.
pub type SKurvenWeicheUnit = SKurvenWeicheSerialisiert<()>;

impl<A> From<SKurvenWeicheSerialisiert<A>> for v4::SKurvenWeicheUnit {
    fn from(wert: SKurvenWeicheSerialisiert<A>) -> Self {
        let SKurvenWeicheSerialisiert {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            orientierung,
            beschreibung,
            steuerung: _,
        } = wert;
        v4::SKurvenWeicheUnit {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            orientierung: orientierung.into(),
            beschreibung,
            steuerung: (),
        }
    }
}

impl From<v4::SKurvenWeicheUnit> for SKurvenWeicheUnit {
    fn from(wert: v4::SKurvenWeicheUnit) -> Self {
        let v4::SKurvenWeicheUnit {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            orientierung,
            beschreibung,
            steuerung,
        } = wert;
        SKurvenWeicheUnit {
            länge,
            radius,
            winkel,
            radius_kurve_nach_innen,
            winkel_kurve_nach_innen,
            orientierung: orientierung.into(),
            beschreibung,
            steuerung,
        }
    }
}
