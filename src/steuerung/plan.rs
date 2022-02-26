//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use serde::{Deserialize, Serialize};

// FIXME Benötigt eigenen Serialisiert-Typ
/// Plan für einen automatischen Fahrplan.
#[derive(Debug, Serialize, Deserialize)]
#[allow(missing_copy_implementations)]
pub struct Plan {
    pub aktionen: Vec<Aktion>,
    pub endlosschleife: bool,
}

/// Eine Aktionen in einem Fahrplan.
///
/// Erstellen erster Aktionen führt nicht zwingend zu einem erhöhen der major version.
/// Daher ist das enum als non_exhaustive markiert.
#[non_exhaustive]
#[derive(Debug, Serialize, Deserialize)]
#[allow(missing_copy_implementations)]
pub enum Aktion {}
