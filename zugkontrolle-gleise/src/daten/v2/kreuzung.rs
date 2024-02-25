//! Serialisierbare Darstellung einer [`Kreuzung`] in Version 2.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::kreuzung as v4;
use zugkontrolle_typen::skalar::Skalar;

use crate::daten::{
    v2::weiche::steuerung::{WeicheAnschlüsseSerialisiert, WeicheSteuerungSerialisiert},
    v3,
};

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

impl From<v4::Variante> for Variante {
    fn from(wert: v4::Variante) -> Self {
        match wert {
            v4::Variante::MitKurve => Variante::MitKurve,
            v4::Variante::OhneKurve => Variante::OhneKurve,
        }
    }
}

/// Serialisierbare Repräsentation einer [`Kreuzung`](kreuzung::Kreuzung).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct KreuzungSerialisiert {
    /// Die Länge der Geraden der Kreuzung.
    länge: Skalar,
    /// Der Radius der Kurven der Kreuzung.
    radius: Skalar,
    /// Sind die Kurven Teil der Kreuzung.
    variante: Variante,
    /// Die Beschreibung der Kreuzung.
    beschreibung: Option<String>,
    /// Die Steuerung der Kreuzung.
    steuerung: Option<
        WeicheSteuerungSerialisiert<v3::weiche::gerade::Richtung, WeicheAnschlüsseSerialisiert>,
    >,
}

impl From<KreuzungSerialisiert> for v3::kreuzung::KreuzungSerialisiert {
    fn from(input: KreuzungSerialisiert) -> Self {
        let KreuzungSerialisiert { länge, radius, variante, beschreibung, steuerung } = input;
        v3::kreuzung::KreuzungSerialisiert {
            länge,
            radius,
            variante,
            beschreibung,
            steuerung: steuerung.map(WeicheSteuerungSerialisiert::konvertiere),
        }
    }
}
