//! newtypes for f64, to avoid mixing of length, radius, angle (radians/degree), etc.

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467

use std::convert::From;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Sub, SubAssign};

use crate::zug::zugtyp::*;

/// Spurweite [mm]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f64);

pub trait Zugtyp {
    #[allow(non_upper_case_globals)]
    const spurweite: Spurweite;

    // abgeleitete Größe unter der Umrechnung 1mm
    #[allow(non_upper_case_globals)]
    const abstand: CanvasAbstand = CanvasAbstand::new(Self::spurweite.0 / 3.);
    #[allow(non_upper_case_globals)]
    const beschraenkung: CanvasAbstand =
        CanvasAbstand::new(Self::spurweite.0 + 2. * Self::abstand.0);
    fn radius_begrenzung(radius: Radius) -> CanvasAbstand {
        CanvasAbstand::new(radius.0 + 0.5 * Self::spurweite.0 + Self::abstand.0)
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
impl Default for CanvasX {
    fn default() -> Self {
        CanvasX(0.)
    }
}
impl Add<CanvasAbstand> for CanvasX {
    type Output = CanvasX;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> CanvasX {
        CanvasX(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasX {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasX {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasX(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasX {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
/// Vertikale Koordinate auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CanvasY(pub f64);
impl Default for CanvasY {
    fn default() -> Self {
        CanvasY(0.)
    }
}
impl Add<CanvasAbstand> for CanvasY {
    type Output = Self;

    fn add(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasY(self.0 + rhs)
    }
}
impl AddAssign<CanvasAbstand> for CanvasY {
    fn add_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 += rhs
    }
}
impl Sub<CanvasAbstand> for CanvasY {
    type Output = Self;

    fn sub(self, CanvasAbstand(rhs): CanvasAbstand) -> Self {
        CanvasY(self.0 - rhs)
    }
}
impl SubAssign<CanvasAbstand> for CanvasY {
    fn sub_assign(&mut self, CanvasAbstand(rhs): CanvasAbstand) {
        self.0 -= rhs
    }
}
/// Abstand/Länge auf einem Cairo-Canvas
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct CanvasAbstand(f64);
impl CanvasAbstand {
    /// Umrechnung von mm-Größen auf Canvas-Koordinaten
    /// Verwenden dieser Funktion um evtl. in der Zukunft einen Faktor zu erlauben
    pub const fn new(abstand_mm: f64) -> CanvasAbstand {
        CanvasAbstand(abstand_mm)
    }

    /// Anzahl benötigter Pixel um den CanvasAbstand darstellen zu können
    pub fn pixel(&self) -> u64 {
        self.0.ceil() as u64
    }
}
impl Add<CanvasX> for CanvasAbstand {
    type Output = CanvasX;

    fn add(self, CanvasX(rhs): CanvasX) -> CanvasX {
        CanvasX(self.0 + rhs)
    }
}
impl Add<CanvasY> for CanvasAbstand {
    type Output = CanvasY;

    fn add(self, CanvasY(rhs): CanvasY) -> CanvasY {
        CanvasY(self.0 + rhs)
    }
}
impl Mul<f64> for CanvasAbstand {
    type Output = CanvasAbstand;

    fn mul(self, rhs: f64) -> CanvasAbstand {
        CanvasAbstand(self.0 * rhs)
    }
}
impl Mul<CanvasAbstand> for f64 {
    type Output = CanvasAbstand;

    fn mul(self, CanvasAbstand(rhs): CanvasAbstand) -> CanvasAbstand {
        CanvasAbstand(self * rhs)
    }
}
impl MulAssign<f64> for CanvasAbstand {
    fn mul_assign(&mut self, rhs: f64) {
        self.0 *= rhs
    }
}
impl Div<f64> for CanvasAbstand {
    type Output = CanvasAbstand;

    fn div(self, rhs: f64) -> CanvasAbstand {
        CanvasAbstand(self.0 / rhs)
    }
}
impl DivAssign<f64> for CanvasAbstand {
    fn div_assign(&mut self, rhs: f64) {
        self.0 /= rhs
    }
}
