//! Farbe im RGB-Schema (inklusive Serialize/Deserialize-Implementierungen).

use iced_core::Color;
use serde::{Deserialize, Serialize};

/// Rote [`Farbe`].
pub const ROT: Farbe = Farbe { rot: 1., grün: 0., blau: 0. };

/// Grüne [`Farbe`].
pub const GRÜN: Farbe = Farbe { rot: 0., grün: 1., blau: 0. };

/// Blaue [`Farbe`].
pub const BLAU: Farbe = Farbe { rot: 0., grün: 0., blau: 1. };

/// Weiße [`Farbe`].
pub const WEIẞ: Farbe = Farbe { rot: 1., grün: 1., blau: 1. };

/// Schwarze [`Farbe`].
pub const SCHWARZ: Farbe = Farbe { rot: 0., grün: 0., blau: 0. };

/// Eine Farbe im RGB-Schema.
#[derive(Debug, Clone, Copy, Default, PartialEq, Serialize, Deserialize)]
pub struct Farbe {
    /// Rot
    pub rot: f32,
    /// Grün
    pub grün: f32,
    /// Blau
    pub blau: f32,
}

impl From<Farbe> for Color {
    fn from(Farbe { rot, grün, blau }: Farbe) -> Self {
        Color::from_rgb(rot, grün, blau)
    }
}
