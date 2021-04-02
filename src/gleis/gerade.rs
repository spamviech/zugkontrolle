//! Definition und zeichnen einer Gerade

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::hash::Hash;
use std::marker::PhantomData;

use super::anchor;
use super::types::*;

/// Definition einer Gerade
#[derive(Debug, Clone)]
pub struct Gerade<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, anchor::Lookup)]
pub enum AnchorName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Gerade<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn width(&self) -> u64 {
        CanvasAbstand::from(self.length).pixel()
    }

    fn height(&self) -> u64 {
        beschraenkung::<Z>().pixel()
    }

    fn zeichne(&self, cairo: &mut Cairo) {
        // Beschränkungen
        cairo.move_to(self.gleis_links(), self.beschraenkung_oben());
        cairo.line_to(self.gleis_links(), self.beschraenkung_unten());
        cairo.move_to(self.gleis_rechts(), self.beschraenkung_oben());
        cairo.line_to(self.gleis_rechts(), self.beschraenkung_unten());
        // Gleis
        cairo.move_to(self.gleis_links(), self.gleis_oben());
        cairo.line_to(self.gleis_rechts(), self.gleis_oben());
        cairo.move_to(self.gleis_links(), self.gleis_unten());
        cairo.line_to(self.gleis_rechts(), self.gleis_unten())
    }

    fn fuelle(&self, cairo: &mut Cairo) {
        cairo.move_to(self.gleis_links(), self.gleis_oben());
        cairo.line_to(self.gleis_links(), self.gleis_unten());
        cairo.line_to(self.gleis_rechts(), self.gleis_unten());
        cairo.line_to(self.gleis_rechts(), self.gleis_oben());
        cairo.line_to(self.gleis_links(), self.gleis_oben());
    }

    fn anchor_points(&self) -> Self::AnchorPoints {
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position { x: self.gleis_links(), y: self.beschraenkung_mitte() },
                direction: anchor::Direction { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende: anchor::Point {
                position: anchor::Position {
                    x: self.gleis_rechts(),
                    y: self.beschraenkung_mitte(),
                },
                direction: anchor::Direction { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        }
    }
}

// Utility functions
impl<Z: Zugtyp> Gerade<Z> {
    fn gleis_links(&self) -> CanvasX {
        CanvasX(0.)
    }

    fn gleis_rechts(&self) -> CanvasX {
        self.gleis_links() + CanvasAbstand::from(self.length)
    }

    fn beschraenkung_mitte(&self) -> CanvasY {
        self.beschraenkung_oben() + 0.5 * beschraenkung::<Z>()
    }

    fn beschraenkung_oben(&self) -> CanvasY {
        CanvasY(0.)
    }

    fn beschraenkung_unten(&self) -> CanvasY {
        self.beschraenkung_oben() + beschraenkung::<Z>()
    }

    fn gleis_oben(&self) -> CanvasY {
        CanvasY(0.) + abstand::<Z>()
    }

    fn gleis_unten(&self) -> CanvasY {
        self.gleis_oben() + beschraenkung::<Z>() - 2. * abstand::<Z>()
    }
}
