//! Zugtyp Trait + Phantom-Typen + Spurweite

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp {
    const SPURWEITE: Spurweite;
}
