pub mod gerade;
pub mod kreuzung;
pub mod kurve;
pub mod weiche;

use gerade::*;
use kreuzung::*;
use kurve::*;
use weiche::*;

/// Definition eines Gleises
pub enum GleisDefinition<T> {
    Gerade(Gerade<T>),
    Kurve(Kurve<T>),
    Weiche(Weiche<T>),
    Kreuzung(Kreuzung<T>),
}
