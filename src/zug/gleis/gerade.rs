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
        let gleis_links: CanvasX = CanvasX::default();
        let gleis_rechts: CanvasX = CanvasX::default() + CanvasAbstand::new(self.length.0);
        let beschraenkung_oben: CanvasY = CanvasY::default();
        let beschraenkung_unten: CanvasY = CanvasY::default() + Z::beschraenkung;
        // Beschränkungen
        c.move_to(gleis_links.0, beschraenkung_oben.0);
        c.line_to(gleis_links.0, beschraenkung_unten.0);
        c.move_to(gleis_rechts.0, beschraenkung_oben.0);
        c.line_to(gleis_rechts.0, beschraenkung_unten.0);
        // Gleis
        let gleis_oben: CanvasY = CanvasY::default() + Z::abstand;
        let gleis_unten: CanvasY = CanvasY::default() + Z::beschraenkung;
        c.move_to(gleis_links.0, gleis_oben.0);
        c.line_to(gleis_rechts.0, gleis_oben.0);
        c.move_to(gleis_links.0, gleis_unten.0);
        c.line_to(gleis_rechts.0, gleis_unten.0)
    }

    pub fn anchor_points(&self) -> AnchorPointMap {
        let gleis_links: CanvasX = CanvasX::default();
        let gleis_rechts: CanvasX = CanvasX::default() + CanvasAbstand::new(self.length.0);
        let beschraenkung_mitte: CanvasY = CanvasY::default() + 0.5 * Z::beschraenkung;
        with_anchor_name("Gerade", [
            AnchorPoint {
                position: AnchorPosition { x: gleis_links, y: beschraenkung_mitte },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            AnchorPoint {
                position: AnchorPosition { x: gleis_rechts, y: beschraenkung_mitte },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        ])
    }
}
