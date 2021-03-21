pub mod gerade;
pub mod kreuzung;
pub mod kurve;
pub mod weiche;

use gerade::*;
use kreuzung::*;
use kurve::*;
use weiche::*;

/// Definition eines Gleises
pub enum GleisDefinition {
    Gerade(Gerade),
    Kurve(Kurve),
    Weiche(Weiche),
    Kreuzung(Kreuzung),
}

// TODO: Spurweite
// erneut Zugtyp-phantom-type?
