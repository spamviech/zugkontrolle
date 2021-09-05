//! Eine Sammlung an Aktionen, die in vorgegebener Reihenfolge ausgeführt werden können.

use serde::{Deserialize, Serialize};

/// TODO Dummy-Typ; wenn fertig wird er einen automatischen Fahrplan darstellen.
/// Die erste Anpassung in einen sinnvollen Wert (wahrscheinlich ein Record struct)
/// wird die SemVer nicht zwingend berücksichtigen (kann ohne erhöhen der major version erfolgen).
#[derive(Serialize, Deserialize)]
pub struct Plan;
