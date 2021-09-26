//! Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.

use rstar::primitives::Rectangle;

use crate::application::typen::{skalar::Skalar, vektor::Vektor, winkel::Winkel};

/// Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.
#[derive(Debug, Clone)]
pub struct Rechteck {
    pub ecke_a: Vektor,
    pub ecke_b: Vektor,
}

impl Rechteck {
    /// Erzeuge ein Rechteck der angegebenen Größe beginnend bei `(0, 0)`.
    pub fn mit_größe(größe: Vektor) -> Self {
        Rechteck { ecke_a: Vektor::null_vektor(), ecke_b: größe }
    }

    /// Verschiebe das Rechteck um Vektor.
    #[zugkontrolle_derive::chain]
    pub fn verschiebe(&mut self, bewegung: &Vektor) {
        self.ecke_a += bewegung;
        self.ecke_b += bewegung;
    }

    /// Dehne das Rechteck aus, so dass es um `winkel`-Rotation um `(0, 0)` (im Uhrzeigersinn)
    /// in das angepasste (nicht rotierte) Rechteck passt.
    #[zugkontrolle_derive::chain]
    pub fn respektiere_rotation(&mut self, winkel: &Winkel) {
        let Rechteck { mut ecke_a, mut ecke_b } = *self;
        let mut ecke_c = Vektor { x: ecke_a.x, y: ecke_b.y };
        let mut ecke_d = Vektor { x: ecke_b.x, y: ecke_a.y };
        // rotiere alle Ecken
        ecke_a.rotiere(*winkel);
        ecke_b.rotiere(*winkel);
        ecke_c.rotiere(*winkel);
        ecke_d.rotiere(*winkel);
        // finde maximale x-, y-Werte
        let xs = [ecke_a.x, ecke_b.x, ecke_c.x, ecke_d.x];
        let min_x = xs.iter().fold(xs[0], find(Skalar::min));
        let max_x = xs.iter().fold(xs[0], find(Skalar::max));
        let ys = [ecke_a.y, ecke_b.y, ecke_c.y, ecke_d.y];
        let min_y = ys.iter().fold(ys[0], find(Skalar::min));
        let max_y = ys.iter().fold(ys[0], find(Skalar::min));
        self.ecke_a = Vektor { x: min_x, y: min_y };
        self.ecke_b = Vektor { x: max_x, y: max_y };
    }

    /// Position der linken oberen Ecke des Rechtecks.
    pub fn position(&self) -> Vektor {
        Vektor { x: self.ecke_a.x.min(&self.ecke_b.x), y: self.ecke_a.y.min(&self.ecke_b.y) }
    }

    /// Größe des Rechtecks.
    pub fn größe(&self) -> Vektor {
        Vektor {
            x: (self.ecke_a.x - self.ecke_b.x).abs(),
            y: (self.ecke_a.y - self.ecke_b.y).abs(),
        }
    }
}

fn find<'t>(
    cmp: impl 't + Fn(&Skalar, &Skalar) -> Skalar,
) -> impl 't + Fn(Skalar, &Skalar) -> Skalar {
    move |a: Skalar, b: &Skalar| cmp(&a, b)
}

impl From<Rechteck> for Rectangle<Vektor> {
    fn from(input: Rechteck) -> Self {
        Rectangle::from_corners(input.ecke_a, input.ecke_b)
    }
}
