//! Serialisierbare Darstellung einer Weichen-Steuerung in Version 3.

use serde::Deserialize;

use zugkontrolle_gleis::steuerung;

use crate::daten::{v2::anschluss::OutputSerialisiert, v3};

use zugkontrolle_gleis::steuerung::weiche as v4;

/// Die serialisierbare Darstellung der Steuerung einer Weiche.
#[derive(Deserialize)]
pub(in crate::daten::v2) struct WeicheSteuerungSerialisiert<Richtung, Anschlüsse> {
    /// Der Name der Weiche.
    name: steuerung::weiche::Name,
    /// Die aktuelle Richtung der Weiche.
    aktuelle_richtung: Richtung,
    /// Die Richtung vor der aktuellen Richtung.
    letzte_richtung: Richtung,
    /// Die Anschlüsse der Weiche.
    anschlüsse: Anschlüsse,
}

/// Die aktuelle und vorherige Richtung.
pub(in crate::daten::v2) struct AktuellUndBisher<R> {
    #[allow(clippy::missing_docs_in_private_items)]
    aktuelle_richtung: R,
    #[allow(clippy::missing_docs_in_private_items)]
    letzte_richtung: R,
}

/// Hilfs-Type für alternative [`From`]-Implementierungen.
pub(in crate::daten::v2) struct Wrapper<T>(T);

impl<R> From<AktuellUndBisher<R>> for Wrapper<R> {
    fn from(input: AktuellUndBisher<R>) -> Self {
        Wrapper(input.aktuelle_richtung)
    }
}

impl From<AktuellUndBisher<v3::weiche::dreiwege::Richtung>>
    for Wrapper<v3::weiche::dreiwege::RichtungInformation>
{
    fn from(input: AktuellUndBisher<v3::weiche::dreiwege::Richtung>) -> Self {
        let AktuellUndBisher { aktuelle_richtung, letzte_richtung } = input;
        Wrapper(v3::weiche::dreiwege::RichtungInformation { aktuelle_richtung, letzte_richtung })
    }
}

impl<R1, A1> WeicheSteuerungSerialisiert<R1, A1> {
    /// Konvertiere in die Darstellung, wie sie in Version 3 verwendet wird.
    pub(in crate::daten::v2) fn konvertiere<R2, A2>(
        self,
    ) -> v3::weiche::steuerung::WeicheSerialisiert<R2, A2>
    where
        Wrapper<R2>: From<AktuellUndBisher<R1>>,
        A2: From<A1>,
    {
        let WeicheSteuerungSerialisiert { name, aktuelle_richtung, letzte_richtung, anschlüsse } =
            self;
        v3::weiche::steuerung::WeicheSerialisiert::neu(
            Name(name.0),
            Wrapper::from(AktuellUndBisher { aktuelle_richtung, letzte_richtung }).0,
            anschlüsse.into(),
        )
    }
}

/// Serialisierbare Repräsentation der [`Anschlüsse`](gerade_weiche::RichtungAnschlüsse)
/// einer [`Weiche`](gerade_weiche::Weiche).
#[derive(Deserialize)]
pub(in crate::daten::v2) struct WeicheAnschlüsseSerialisiert {
    /// Der Anschluss zum Schalten auf die Gerade.
    gerade: OutputSerialisiert,
    /// Der Anschluss zum Schalten auf die Kurve.
    kurve: OutputSerialisiert,
}

impl From<WeicheAnschlüsseSerialisiert> for v3::weiche::gerade::RichtungAnschlüsseSerialisiert {
    fn from(input: WeicheAnschlüsseSerialisiert) -> Self {
        let WeicheAnschlüsseSerialisiert { gerade, kurve } = input;
        v3::weiche::gerade::RichtungAnschlüsseSerialisiert {
            gerade: gerade.into(),
            kurve: kurve.into(),
        }
    }
}

/// Name einer [`Weiche`](WeicheSerialisiert).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
pub struct Name(pub String);

impl From<Name> for v4::Name {
    fn from(wert: Name) -> Self {
        v4::Name(wert.0)
    }
}
