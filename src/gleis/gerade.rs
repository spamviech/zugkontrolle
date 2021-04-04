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

    fn size(&self) -> canvas::Size {
        canvas::Size::new(
            canvas::X(0.) + self.length.to_abstand(),
            canvas::Y(0.) + beschraenkung::<Z>(),
        )
    }

    fn zeichne(&self, path_builder: &mut canvas::PathBuilder) {
        zeichne::<Z>(&mut path_builder, canvas::X(0.), canvas::Y(0.), self.length);
    }

    /*
    fn fuelle(&self, cairo: &mut Cairo) {
        cairo.move_to(self.gleis_links(), self.gleis_oben());
        cairo.line_to(self.gleis_links(), self.gleis_unten());
        cairo.line_to(self.gleis_rechts(), self.gleis_unten());
        cairo.line_to(self.gleis_rechts(), self.gleis_oben());
        cairo.line_to(self.gleis_links(), self.gleis_oben());
    }
    */

    fn anchor_points(&self) -> Self::AnchorPoints {
        let gleis_links: canvas::X = canvas::X(0.);
        let gleis_rechts: canvas::X = gleis_links + self.length.to_abstand();
        let beschraenkung_mitte: canvas::Y = canvas::Y(0.) + 0.5 * beschraenkung::<Z>();
        AnchorPoints {
            anfang: anchor::Point {
                position: anchor::Position { x: gleis_links, y: beschraenkung_mitte },
                direction: anchor::Direction { dx: canvas::X(-1.), dy: canvas::Y(0.) },
            },
            ende: anchor::Point {
                position: anchor::Position { x: gleis_rechts, y: beschraenkung_mitte },
                direction: anchor::Direction { dx: canvas::X(1.), dy: canvas::Y(0.) },
            },
        }
    }
}

pub(crate) fn zeichne<Z: Zugtyp>(
    path_builder: &mut canvas::PathBuilder,
    start_x: canvas::X,
    start_y: canvas::Y,
    laenge: Length,
) {
    let gleis_links: canvas::X = start_x;
    let gleis_rechts: canvas::X = gleis_links + laenge.to_abstand();
    let beschraenkung_oben: canvas::Y = start_y;
    let beschraenkung_unten: canvas::Y = beschraenkung_oben + beschraenkung::<Z>();
    let gleis_oben: canvas::Y = beschraenkung_oben + abstand::<Z>();
    let gleis_unten: canvas::Y = gleis_oben + Z::SPURWEITE.to_abstand();
    // Beschränkungen
    path_builder.move_to(canvas::Point::new(gleis_links, beschraenkung_oben));
    path_builder.line_to(canvas::Point::new(gleis_links, beschraenkung_unten));
    path_builder.move_to(canvas::Point::new(gleis_rechts, beschraenkung_oben));
    path_builder.line_to(canvas::Point::new(gleis_rechts, beschraenkung_unten));
    // Gleis
    path_builder.move_to(canvas::Point::new(gleis_links, gleis_oben));
    path_builder.line_to(canvas::Point::new(gleis_rechts, gleis_oben));
    path_builder.move_to(canvas::Point::new(gleis_links, gleis_unten));
    path_builder.line_to(canvas::Point::new(gleis_rechts, gleis_unten));
}
