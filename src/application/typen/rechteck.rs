//! Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.

use rstar::primitives::Rectangle;

use crate::application::typen::{vektor::Vektor, winkel::Winkel};

/// Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.
#[derive(Debug, Clone)]
pub struct Rechteck {
    pub position: Vektor,
    pub größe: Vektor,
}

impl Rechteck {
    /// Erzeuge ein Rechteck der angegebenen Größe beginnend bei (0, 0).
    pub fn mit_größe(größe: Vektor) -> Self {
        Rechteck { position: Vektor::null_vektor(), größe }
    }

    /// Verschiebe das Rechteck um Vektor
    pub fn verschiebe(&mut self, bewegung: &Vektor) {
        self.position += bewegung
    }

    /// Dehne das Rechteck aus, so dass es um `winkel` rotiert in die (nicht rotierte) neue Größe passt.
    pub fn respektiere_rotation(&mut self, winkel: &Winkel) {
        todo!()
    }
}

impl From<Rechteck> for Rectangle<Vektor> {
    fn from(input: Rechteck) -> Self {
        Rectangle::from_corners(input.position, input.größe)
    }
}
