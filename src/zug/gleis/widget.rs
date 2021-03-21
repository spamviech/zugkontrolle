use super::gerade::*;
use super::kreuzung::*;
use super::kurve::*;
use super::weiche::*;

/// Definition eines Gleises
pub enum GleisDefinition<T> {
    Gerade(Gerade<T>),
    Kurve(Kurve<T>),
    Weiche(Weiche<T>),
    Kreuzung(Kreuzung<T>),
}
