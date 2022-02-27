//! Farbe im RGB-Schema (inklusive Serialize/Deserialize-Implementierungen).

use iced::Color;
use serde::{Deserialize, Serialize};

/// Eine Farbe im RGB-Schema.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Farbe {
    pub r: f32,
    pub g: f32,
    pub b: f32,
}

impl From<Farbe> for Color {
    fn from(Farbe { r, g, b }: Farbe) -> Self {
        Color::from_rgb(r, g, b)
    }
}
