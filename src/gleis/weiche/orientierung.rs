//! Die Orientierung einer [`Weiche`], in welche Richtung geht die Kurve.

use serde::{Deserialize, Serialize};

/// Die Orientierung einer [`Weiche`], in welche Richtung geht die Kurve.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Orientierung {
    /// Die Kurve geht nach links.
    Links,
    /// Die Kurve geht nach rechts.
    Rechts,
}
