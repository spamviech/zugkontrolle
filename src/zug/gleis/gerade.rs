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
        self.length.0.ceil() as u64
    }

    pub fn height(&self) -> u64 {
        Z::beschraenkung.0.ceil() as u64
    }

    pub fn zeichne(&self, c: Context) {
        // Beschränkungen
        c.move_to(0., 0.);
        c.line_to(0., Z::beschraenkung.0);
        c.move_to(self.length.0, 0.);
        c.line_to(self.length.0, Z::beschraenkung.0);
        // Gleis
        let gleis_oben = Z::abstand.0;
        let gleis_unten = Z::beschraenkung.0;
        c.move_to(0., gleis_oben);
        c.line_to(self.length.0, gleis_oben);
        c.move_to(0., gleis_unten);
        c.line_to(self.length.0, gleis_unten)
    }

    pub fn anchor_points(&self) -> AnchorPointMap {
        with_anchor_name("Gerade", [
            AnchorPoint {
                position: AnchorPosition { x: CanvasX(0.), y: CanvasY(0.5 * Z::beschraenkung.0) },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            AnchorPoint {
                position: AnchorPosition {
                    x: CanvasX(self.length.0),
                    y: CanvasY(0.5 * Z::beschraenkung.0),
                },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        ])
    }
}
