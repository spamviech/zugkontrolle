//! Serialisierbare Darstellung eines [KurvenWeiche] in Version 3.

use serde::{Deserialize, Serialize};

use crate::{
    gleis::gleise::daten::v3::weiche::{gerade, orientierung::Orientierung, steuerung},
    typen::{skalar::Skalar, winkel::Winkel},
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<gerade::Richtung, gerade::RichtungAnschlüsseSerialisiert>;

/// Definition einer Kurven-Weiche.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KurvenWeicheSerialisiert {
    /// Die Länge der Geraden vor der äußeren Kurve.
    pub länge: Skalar,
    /// Der Radius der Kurven.
    pub radius: Skalar,
    /// Der Winkel der Kurven.
    pub winkel: Winkel,
    /// Die Orientierung der KurvenWeiche.
    pub orientierung: Orientierung,
    /// Eine allgemeine Beschreibung der KurvenWeiche, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der KurvenWeiche.
    pub steuerung: AnschlüsseSerialisiert,
}
