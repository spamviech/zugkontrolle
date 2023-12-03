//! Serialisierbare Darstellung einer [SKurvenWeiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::gleise::daten::v3::weiche::{gerade, orientierung::Orientierung, steuerung},
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<gerade::Richtung, gerade::RichtungAnschlüsseSerialisiert>;

/// Definition einer Weiche mit S-Kurve.
///
/// Bei extremen Winkeln (<0, >90°, angle_reverse>winkel) wird in negativen x,y-Werten gezeichnet!
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SKurvenWeicheSerialisiert {
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
    pub steuerung: AnschlüsseSerialisiert,
}
