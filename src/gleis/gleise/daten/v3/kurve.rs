//! Serialisierbare Darstellung einer [Kurve] in Version 3.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    steuerung::kontakt::KontaktSerialisiert,
    typen::{skalar::Skalar, winkel::Winkel},
};

/// Definition einer Kurve.
///
/// Bei extremen Winkeln (<0, >180°) wird in negativen x-Werten gezeichnet!
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KurveSerialisiert {
    /// Der Radius auf dem Canvas.
    pub radius: Skalar,
    /// Der Winkel der Kurve.
    pub winkel: Winkel,
    /// Eine allgemeine Beschreibung der Kurve, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Der Anschluss für einen [Kontakt] an der Schiene.
    pub kontakt: Option<KontaktSerialisiert>,
}
