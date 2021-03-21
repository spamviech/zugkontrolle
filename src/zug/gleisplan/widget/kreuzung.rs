use crate::zug::gleisplan::types::*;

/// Definition einer Kreuzung
pub struct Kreuzung {
    pub length: Length,
    pub radius: Radius,
    // TODO: winkel kann aus radius und länge berechnet werden?
    pub angle: Angle,
}
pub enum KreuzungsArt {
    MitKurve,
    OhneKurve,
}
