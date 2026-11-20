//! Serialisierbare Darstellung einer [`Geschwindigkeit`] in Version 4.X.

#![expect(clippy::pub_use, reason = "Erlaube einfache Anpassung der Typsignaturen.")]

use std::{fmt::Debug, marker::PhantomData, time::Duration};

use serde::{Deserialize, Serialize};
use zugkontrolle_gleis::steuerung::geschwindigkeit::{
    self as v6, Fahrtrichtung, Mittelleiter, Zweileiter,
};
pub use zugkontrolle_gleis::steuerung::geschwindigkeit::{
    MittelleiterSerialisiert, Name, ZweileiterSerialisiert,
};
use zugkontrolle_util::{eingeschränkt::NullBisEins, void::Void};

/// Ein [`Leiter`] ermöglicht ein Einstellen der Geschwindigkeit und Umdrehen der Fahrtrichtung.
pub trait Leiter {
    /// Wie lange ist die Überspannung beim Umdrehen [`Fließend`](Fließend::Fließend).
    type UmdrehenZeit: Clone;

    /// Was ist das Verhältnis von Fahrspannung zur Überspannung zum Umdrehen.
    type VerhältnisFahrspannungÜberspannung: Clone;

    /// Einstellmöglichkeiten der aktuellen Fahrtrichtung.
    type Fahrtrichtung;
}

/// Ein unterstützter Leiter, mit bekannten Namen.
pub trait BekannterLeiter: Leiter + Sized {
    /// Der Name des Leiters.
    const NAME: &'static str;
}

impl Leiter for Mittelleiter {
    type Fahrtrichtung = Void;
    type UmdrehenZeit = Duration;
    type VerhältnisFahrspannungÜberspannung = NullBisEins;
}

impl BekannterLeiter for Mittelleiter {
    const NAME: &'static str = "Mittelleiter";
}

impl BekannterLeiter for Zweileiter {
    const NAME: &'static str = "Zweileiter";
}

impl Leiter for Zweileiter {
    type Fahrtrichtung = Fahrtrichtung;
    type UmdrehenZeit = PhantomData<Duration>;
    type VerhältnisFahrspannungÜberspannung = PhantomData<NullBisEins>;
}

/// Serialisierbare Repräsentation einer [`Geschwindigkeit`].
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct GeschwindigkeitSerialisiert<LeiterSerialisiert> {
    /// Der Leiter der Geschwindigkeit.
    pub leiter: LeiterSerialisiert,
}

impl<S: Into<v6::LeiterSerialisiert>> From<GeschwindigkeitSerialisiert<S>>
    for v6::GeschwindigkeitSerialisiert
{
    fn from(value: GeschwindigkeitSerialisiert<S>) -> Self {
        v6::GeschwindigkeitSerialisiert { leiter: value.leiter.into() }
    }
}
