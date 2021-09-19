//! Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.

use rstar::primitives::Rectangle;

use crate::application::typen::Vektor;

/// Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.
#[derive(Debug, Clone)]
pub struct Rechteck {
    pub position: Vektor,
    pub ausdehnung: Vektor,
}

impl Rechteck {
    pub fn verschiebe(&mut self, bewegung: &Vektor) {
        self.position += bewegung
    }
}

impl From<Rechteck> for Rectangle<Vektor> {
    fn from(input: Rechteck) -> Self {
        Rectangle::from_corners(input.position, input.ausdehnung)
    }
}
