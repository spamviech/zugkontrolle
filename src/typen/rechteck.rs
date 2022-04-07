//! Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.

use rstar::primitives::Rectangle;

use crate::typen::{vektor::Vektor, winkel::Winkel};

/// Ein Rechteck auf dem Canvas. Hauptsächlich zur Verwendung als Bounding Box.
#[allow(missing_copy_implementations)]
#[derive(Debug, Clone)]
pub struct Rechteck {
    /// Eine Ecke des Rechtecks.
    pub ecke_a: Vektor,
    /// Die gegenüberliegende Ecke des Rechtecks.
    pub ecke_b: Vektor,
}

impl Rechteck {
    /// Erzeuge ein Rechteck der angegebenen Größe beginnend bei `(0, 0)`.
    pub fn mit_größe(größe: Vektor) -> Self {
        Rechteck { ecke_a: Vektor::null_vektor(), ecke_b: größe }
    }

    /// Erzeuge das kleinstmögliche Rechteck das alle Vektoren enthält.
    /// Schlägt bei einem leeren Iterator (erstes `next` gibt `None` zurück) fehl.
    pub fn aus_vektoren(mut vektoren: impl Iterator<Item = Vektor>) -> Option<Self> {
        let anfang = vektoren.next()?;
        let (min, max) = vektoren.fold((anfang, anfang), min_max);
        Some(Rechteck { ecke_a: min, ecke_b: max })
    }

    /// Verschiebe das Rechteck um [Vektor].
    #[zugkontrolle_macros::chain]
    pub fn verschiebe(&mut self, bewegung: &Vektor) {
        self.ecke_a += bewegung;
        self.ecke_b += bewegung;
    }

    /// Erzeuge ein Rechteck, in dem `self` und `other` enthalten sind.
    pub fn einschließend(self, other: Self) -> Self {
        Rechteck::aus_vektoren([self.ecke_a, self.ecke_b, other.ecke_a, other.ecke_b].into_iter())
            .expect("Iterator besteht aus 4 Elementen.")
    }

    /// Dehne das Rechteck aus, so dass es um `winkel`-Rotation um `(0, 0)` (im Uhrzeigersinn)
    /// in das angepasste (nicht rotierte) Rechteck passt.
    #[zugkontrolle_macros::chain]
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
        *self = Rechteck::aus_vektoren([ecke_a, ecke_b, ecke_c, ecke_d].into_iter())
            .expect("Iterator besteht aus 4 Elementen.");
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

    /// Ecke mit den minimalen Koordinaten.
    pub fn ecke_min(&self) -> Vektor {
        Vektor { x: self.ecke_a.x.min(&self.ecke_b.x), y: self.ecke_a.y.min(&self.ecke_b.y) }
    }

    /// Ecke mit den maximalen Koordinaten.
    pub fn ecke_max(&self) -> Vektor {
        Vektor { x: self.ecke_a.x.max(&self.ecke_b.x), y: self.ecke_a.y.max(&self.ecke_b.y) }
    }
}

fn min_max((min, max): (Vektor, Vektor), wert: Vektor) -> (Vektor, Vektor) {
    (
        Vektor { x: min.x.min(&wert.x), y: min.y.min(&wert.y) },
        Vektor { x: max.x.max(&wert.x), y: max.y.max(&wert.y) },
    )
}

impl From<Rechteck> for Rectangle<Vektor> {
    fn from(input: Rechteck) -> Self {
        Rectangle::from_corners(input.ecke_a, input.ecke_b)
    }
}
