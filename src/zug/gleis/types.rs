//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::convert::From;

use crate::zug::zugtyp::*;

/// Spurweite [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f64);

/// Abstands-Anzeige an Gleis-Enden [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Abstand(pub f64);

/// Gleis-Breite inklusive Abstands-Anzeige [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Beschraenkung(pub f64);

pub trait Zugtyp {
    #[allow(non_upper_case_globals)]
    const spurweite: Spurweite;

    #[allow(non_upper_case_globals)]
    const abstand: Abstand = Abstand(Self::spurweite.0 / 3.);
    #[allow(non_upper_case_globals)]
    const beschraenkung: Beschraenkung = Beschraenkung(Self::spurweite.0 + 2. * Self::abstand.0);

    fn radius_begrenzung(radius: Radius) -> Radius {
        Radius(radius.0 + 0.5 * Self::spurweite.0 + Self::abstand.0)
    }
}

impl Zugtyp for Maerklin {
    #[allow(non_upper_case_globals)]
    const spurweite: Spurweite = Spurweite(16.5);
}

impl Zugtyp for Lego {
    #[allow(non_upper_case_globals)]
    const spurweite: Spurweite = Spurweite(38.);
}

/// Längenmaß [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(pub f64);

/// Radius [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(pub f64);

/// Winkel [Bogenmaß]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Angle(pub f64);

// automatically implements Trait Into
impl From<AngleDegrees> for Angle {
    fn from(AngleDegrees(f): AngleDegrees) -> Angle {
        Angle(f.to_degrees())
    }
}

/// Winkel [Grad]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AngleDegrees(pub f64);

impl From<Angle> for AngleDegrees {
    fn from(Angle(f): Angle) -> AngleDegrees {
        AngleDegrees(f.to_degrees())
    }
}

/// Horizontale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CanvasX(pub f64);
/// Vertikale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CanvasY(pub f64);
