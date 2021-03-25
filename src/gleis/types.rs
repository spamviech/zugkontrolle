//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

pub mod angle;
pub mod canvas;
pub use angle::*;
pub use canvas::*;

use crate::zugtyp::*;

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f64);

pub trait Zugtyp {
    #[allow(non_upper_case_globals)]
    const spurweite: Spurweite;

    // abgeleitete Größe unter der Umrechnung 1mm
    #[allow(non_upper_case_globals)]
    const abstand: CanvasAbstand = CanvasAbstand::new(Self::spurweite.0 / 3.);
    fn beschraenkung() -> CanvasAbstand {
        CanvasAbstand::new(Self::spurweite.0) + 2. * Self::abstand
    }
    fn radius_begrenzung(radius: Radius) -> CanvasAbstand {
        CanvasAbstand::new(radius.0) + 0.5 * CanvasAbstand::new(Self::spurweite.0) + Self::abstand
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

/// Längenmaß \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Length(pub f64);

/// Radius \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Radius(pub f64);
