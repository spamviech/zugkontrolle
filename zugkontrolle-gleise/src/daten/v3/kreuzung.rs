//! Serialisierbare Darstellung einer [`Kreuzung`] in Version 3.

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use zugkontrolle_gleis::kreuzung as v4;
use zugkontrolle_typen::skalar::Skalar;

use crate::daten::{
    v2::kreuzung::Variante,
    v3::weiche::{
        gerade::{Richtung, RichtungAnschlüsseSerialisiert},
        steuerung,
    },
};

/// Serialisierbare Darstellung der Steuerung einer [`Kreuzung`].
type AnschlüsseSerialisiert =
    steuerung::WeicheSerialisiert<Richtung, RichtungAnschlüsseSerialisiert>;

// Folge Konvention TypName -> TypNameSerialisiert
#[allow(clippy::module_name_repetitions)]
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

// Folge Konvention TypName -> TypNameUnit
#[allow(clippy::module_name_repetitions)]
/// Eine Variante ohne Anschlüsse.
pub type KreuzungUnit = KreuzungSerialisiert<()>;

impl<A> From<KreuzungSerialisiert<A>> for v4::KreuzungUnit {
    fn from(wert: KreuzungSerialisiert<A>) -> Self {
        let KreuzungSerialisiert { länge, radius, variante, beschreibung, steuerung: _ } = wert;
        v4::KreuzungUnit { länge, radius, variante: variante.into(), beschreibung, steuerung: () }
    }
}

impl From<v4::KreuzungUnit> for KreuzungUnit {
    fn from(wert: v4::KreuzungUnit) -> Self {
        let v4::KreuzungUnit { länge, radius, variante, beschreibung, steuerung } = wert;
        KreuzungUnit { länge, radius, variante: variante.into(), beschreibung, steuerung }
    }
}
