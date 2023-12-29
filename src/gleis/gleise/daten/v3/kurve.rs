//! Serialisierbare Darstellung einer [Kurve] in Version 3.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    gleis::kurve as v4,
    steuerung::kontakt::KontaktSerialisiert,
    typen::{skalar::Skalar, winkel::Winkel},
};

/// Definition einer Kurve.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct KurveSerialisiert<Anschluss = Option<KontaktSerialisiert>> {
    /// Der Radius auf dem Canvas.
    pub radius: Skalar,
    /// Der Winkel der Kurve.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der Kurve, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [Kontakt] an der Schiene.
    pub kontakt: Anschluss,
}

/// Eine Variante ohne Anschlüsse.
pub type KurveUnit = KurveSerialisiert<()>;

impl<A> From<KurveSerialisiert<A>> for v4::KurveUnit {
    fn from(gerade: KurveSerialisiert<A>) -> Self {
        let KurveSerialisiert { radius, winkel, beschreibung, kontakt: _ } = gerade;
        v4::KurveUnit { radius, winkel, beschreibung, kontakt: () }
    }
}
