use crate::zug::gleisplan::types::*;

/// Definition einer Weiche
pub struct Weiche {
    pub length: Length,
    pub radius: Radius,
    pub angle: Angle,
    pub direction: WeichenRichtung,
}
pub enum WeichenRichtung {
    Gerade(WeichenRichtungGerade),
    Gebogen(WeichenRichtungGebogen),
    SKurve(WeichenRichtungSKurve),
}
pub enum WeichenRichtungGerade {
    Links,
    Rechts,
    Dreiwege,
}
pub enum WeichenRichtungGebogen {
    Links,
    Rechts,
}
pub enum WeichenRichtungSKurve {
    Links(Angle),
    Rechts(Angle),
}
