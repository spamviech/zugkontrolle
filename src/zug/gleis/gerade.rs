//! Definition und zeichnen einer Gerade

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::marker::PhantomData;

use cairo::Context;

use super::anchor::*;
use super::types::*;

/// Definition einer Gerade
#[derive(Debug, Clone)]
pub struct Gerade<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
}

// TODO convert to Trait?
impl<Z: Zugtyp> Gerade<Z> {
    pub fn width(&self) -> u64 {
        CanvasAbstand::new(self.length.0).pixel()
    }

    pub fn height(&self) -> u64 {
        Z::beschraenkung.pixel()
    }

    pub fn zeichne(&self, c: Context) {
        // Beschränkungen
        c.move_to(self.gleis_links().0, self.beschraenkung_oben().0);
        c.line_to(self.gleis_links().0, self.beschraenkung_unten().0);
        c.move_to(self.gleis_rechts().0, self.beschraenkung_oben().0);
        c.line_to(self.gleis_rechts().0, self.beschraenkung_unten().0);
        // Gleis
        c.move_to(self.gleis_links().0, self.gleis_oben().0);
        c.line_to(self.gleis_rechts().0, self.gleis_oben().0);
        c.move_to(self.gleis_links().0, self.gleis_unten().0);
        c.line_to(self.gleis_rechts().0, self.gleis_unten().0)
    }

    pub fn anchor_points(&self) -> AnchorPointMap {
        with_anchor_name("Gerade", [
            AnchorPoint {
                position: AnchorPosition { x: self.gleis_links(), y: self.beschraenkung_mitte() },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            AnchorPoint {
                position: AnchorPosition { x: self.gleis_rechts(), y: self.beschraenkung_mitte() },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        ])
    }

    // Utility functions
    fn gleis_links(&self) -> CanvasX {
        CanvasX::default()
    }

    fn gleis_rechts(&self) -> CanvasX {
        CanvasX::default() + CanvasAbstand::new(self.length.0)
    }

    fn beschraenkung_mitte(&self) -> CanvasY {
        CanvasY::default() + 0.5 * Z::beschraenkung
    }

    fn beschraenkung_oben(&self) -> CanvasY {
        CanvasY::default()
    }

    fn beschraenkung_unten(&self) -> CanvasY {
        CanvasY::default() + Z::beschraenkung
    }

    fn gleis_oben(&self) -> CanvasY {
        CanvasY::default() + Z::abstand
    }

    fn gleis_unten(&self) -> CanvasY {
        CanvasY::default() + Z::beschraenkung
    }
}
