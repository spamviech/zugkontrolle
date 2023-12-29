//! Serialisierbare Darstellung einer [Kreuzung] in Version 3.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    gleis::{
        gleise::daten::v3::weiche::{
            gerade::{Richtung, RichtungAnschlüsseSerialisiert},
            steuerung,
        },
        kreuzung as v4,
    },
    typen::skalar::Skalar,
};

type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

/// Definition einer Kreuzung.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct KreuzungSerialisiert<Anschlüsse = Option<AnschlüsseSerialisiert>> {
    /// Die Länge der Geraden.
    pub länge: Skalar,
    /// Der Kurvenradius; legt automatisch den Winkel fest.
    pub radius: Skalar,
    /// Werden die Kurven gezeichnet, oder nur die Geraden.
    pub variante: Variante,
    /// Eine allgemeine Beschreibung der Kreuzung, z.B. die Produktnummer.
    pub beschreibung: Option<String>,
    /// Die Anschlüsse zum Schalten der Kreuzung.
    pub steuerung: Anschlüsse,
}

/// Eine Variante ohne Anschlüsse.
pub type KreuzungUnit = KreuzungSerialisiert<()>;

impl<A> From<KreuzungSerialisiert<A>> for v4::KreuzungUnit {
    fn from(wert: KreuzungSerialisiert<A>) -> Self {
        let KreuzungSerialisiert { länge, radius, variante, beschreibung, steuerung: _ } = wert;
        v4::KreuzungUnit { länge, radius, variante: variante.into(), beschreibung, steuerung: () }
    }
}

/// Werden die Kurven gezeichnet, oder nur die Geraden.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Variante {
    /// Zeichne die Kurven und die Geraden.
    MitKurve,
    /// Zeichne nur die Geraden.
    OhneKurve,
}

impl From<Variante> for v4::Variante {
    fn from(wert: Variante) -> Self {
        match wert {
            Variante::MitKurve => v4::Variante::MitKurve,
            Variante::OhneKurve => v4::Variante::OhneKurve,
        }
    }
}
