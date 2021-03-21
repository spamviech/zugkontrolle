//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// non_ascii_idents might be stabilized soon
// use german names then (currently blocked by umlauts)
// https://github.com/rust-lang/rust/issues/55467
use std::convert::From;

/// Längenmaß [mm]
pub struct Length(f64);

/// Radius [mm]
pub struct Radius(f64);

/// Winkel [Bogenmaß]
pub struct Angle(f64);

// automatically implements Trait Into
impl From<AngleDegrees> for Angle {
    fn from(AngleDegrees(f): AngleDegrees) -> Angle {
        Angle(f.to_degrees())
    }
}

/// Winkel [Grad]
pub struct AngleDegrees(f64);

impl From<Angle> for AngleDegrees {
    fn from(Angle(f): Angle) -> AngleDegrees {
        AngleDegrees(f.to_degrees())
    }
}
