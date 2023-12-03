//! Serialisierbare Darstellung einer [Kreuzung] in Version 3.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    gleis::gleise::daten::v3::weiche::{
        gerade::{Richtung, RichtungAnschlüsseSerialisiert},
        steuerung,
    },
    typen::skalar::Skalar,
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Kreuzung.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KreuzungSerialisiert {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Kurvenradius; legt automatisch den Winkel fest.
    pub radius: Skalar,
    /// Werden die Kurven gezeichnet, oder nur die Geraden.
    pub variante: Variante,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der Kreuzung.
    pub steuerung: AnschlüsseSerialisiert,
}

/// Werden die Kurven gezeichnet, oder nur die Geraden.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    /// Zeichne die Kurven und die Geraden.
    MitKurve,
    /// Zeichne nur die Geraden.
    OhneKurve,
}
