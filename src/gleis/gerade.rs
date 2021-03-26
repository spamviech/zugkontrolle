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
use super::widget::Zeichnen;
use crate as zugkontrolle;
use zugkontrolle_derive::AnchorLookup;

/// Definition einer Gerade
#[derive(Debug, Clone)]
pub struct Gerade<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, AnchorLookup)]
pub enum AnchorName {
    Anfang,
    Ende,
}

impl<Z: Zugtyp> Zeichnen for Gerade<Z> {
    type AnchorName = AnchorName;
    type AnchorPoints = AnchorPoints;

    fn width(&self) -> u64 {
        CanvasAbstand::new(self.length.0).pixel()
    }

    fn height(&self) -> u64 {
        Z::beschraenkung().pixel()
    }

    fn zeichne(&self, cairo: &Cairo) {
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
        CanvasX(0.) + CanvasAbstand::new(self.length.0)
    }

    fn beschraenkung_mitte(&self) -> CanvasY {
        CanvasY(0.) + 0.5 * Z::beschraenkung()
    }

    fn beschraenkung_oben(&self) -> CanvasY {
        CanvasY(0.)
    }

    fn beschraenkung_unten(&self) -> CanvasY {
        CanvasY(0.) + Z::beschraenkung()
    }

    fn gleis_oben(&self) -> CanvasY {
        CanvasY(0.) + Z::abstand
    }

    fn gleis_unten(&self) -> CanvasY {
        CanvasY(0.) + Z::beschraenkung()
    }
}
