//! Farbe im RGB-Schema (inklusive Serialize/Deserialize-Implementierungen).

use iced::Color;
use serde::{Deserialize, Serialize};

/// Eine Farbe im RGB-Schema.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
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
