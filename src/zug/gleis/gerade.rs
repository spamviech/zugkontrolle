//! Definition und zeichnen einer Gerade

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;

use super::anchor::*;
use super::types::*;
use super::widget::{AnchorLookup, Zeichnen};

/// Definition einer Gerade
#[derive(Debug, Clone)]
pub struct Gerade<Z> {
    pub zugtyp: PhantomData<*const Z>,
    pub length: Length,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum GeradeAnchorName {
    Anfang,
    Ende,
}

#[derive(Debug)]
pub struct GeradeAnchors {
    anfang: AnchorPoint,
    ende: AnchorPoint,
}

impl<Z: Zugtyp> Zeichnen for Gerade<Z> {
    type AnchorName = GeradeAnchorName;
    type AnchorPoints = GeradeAnchors;

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
        GeradeAnchors {
            anfang: AnchorPoint {
                position: AnchorPosition { x: self.gleis_links(), y: self.beschraenkung_mitte() },
                direction: AnchorDirection { dx: CanvasX(-1.), dy: CanvasY(0.) },
            },
            ende: AnchorPoint {
                position: AnchorPosition { x: self.gleis_rechts(), y: self.beschraenkung_mitte() },
                direction: AnchorDirection { dx: CanvasX(1.), dy: CanvasY(0.) },
            },
        }
    }
}

impl AnchorLookup<GeradeAnchorName> for GeradeAnchors {
    fn get(&self, key: GeradeAnchorName) -> &AnchorPoint {
        match key {
            GeradeAnchorName::Anfang => &self.anfang,
            GeradeAnchorName::Ende => &self.ende,
        }
    }
    fn get_mut(&mut self, key: GeradeAnchorName) -> &mut AnchorPoint {
        match key {
            GeradeAnchorName::Anfang => &mut self.anfang,
            GeradeAnchorName::Ende => &mut self.ende,
        }
    }
    fn map<F: Fn(&AnchorPoint)>(&self, action: F) {
        action(&self.anfang);
        action(&self.ende);
    }
}

// Utility functions
impl<Z: Zugtyp> Gerade<Z> {
    fn gleis_links(&self) -> CanvasX {
        CanvasX::default()
    }

    fn gleis_rechts(&self) -> CanvasX {
        CanvasX::default() + CanvasAbstand::new(self.length.0)
    }

    fn beschraenkung_mitte(&self) -> CanvasY {
        CanvasY::default() + 0.5 * Z::beschraenkung()
    }

    fn beschraenkung_oben(&self) -> CanvasY {
        CanvasY::default()
    }

    fn beschraenkung_unten(&self) -> CanvasY {
        CanvasY::default() + Z::beschraenkung()
    }

    fn gleis_oben(&self) -> CanvasY {
        CanvasY::default() + Z::abstand
    }

    fn gleis_unten(&self) -> CanvasY {
        CanvasY::default() + Z::beschraenkung()
    }
}
