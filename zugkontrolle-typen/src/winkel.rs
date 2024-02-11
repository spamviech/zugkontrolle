//! Winkel in Bogen- und Gradmaß.

use std::cmp::Ordering;
use std::convert::From;
use std::f32::consts;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use kommandozeilen_argumente::{Beschreibung, ParseArgument, Vergleich};
use serde::{Deserialize, Serialize};

use crate::skalar::Skalar;

/// Trigonometrische Funktionen (+ abs) für Winkel.
pub trait Trigonometrie {
    /// Absoluter Wert.
    #[must_use]
    fn abs(&self) -> Self;
    /// Normalisiert in den äquivalenten Bereich zu [-π,π).
    #[must_use]
    fn normalisiert(self) -> Self;
    /// Kosinus
    #[must_use]
    fn cos(&self) -> Skalar;
    /// Sinus
    #[must_use]
    fn sin(&self) -> Skalar;
    /// Tangens
    #[must_use]
    fn tan(&self) -> Skalar;
    /// Inverser Kosinus
    #[must_use]
    fn acos(input: Skalar) -> Self;
    /// Inverser Sinus
    #[must_use]
    fn asin(input: Skalar) -> Self;
    /// Inverser Tangens
    #[must_use]
    fn atan(input: Skalar) -> Self;
}

/// τ = 2. * π, eine ganze Umdrehung.
pub const TAU: Winkel = Winkel(consts::TAU);

/// π, eine halbe Umdrehung.
pub const PI: Winkel = Winkel(consts::PI);

/// π / 2., eine viertel Umdrehung.
pub const FRAC_PI_2: Winkel = Winkel(consts::FRAC_PI_2);

/// 0
pub const ZERO: Winkel = Winkel(0.);

/// Winkel \[`Bogenmaß\`]
///
/// Die [`PartialEq`]- und [`PartialOrd`]-Instanzen sind abgeleitet und normalisieren die Winkel NICHT,
/// bevor sie verglichen werden.
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd, Serialize, Deserialize)]
pub struct Winkel(pub f32);

impl From<WinkelGradmaß> for Winkel {
    fn from(WinkelGradmaß(gradmaß): WinkelGradmaß) -> Winkel {
        Winkel(gradmaß.to_radians())
    }
}

impl PartialEq<WinkelGradmaß> for Winkel {
    fn eq(&self, other: &WinkelGradmaß) -> bool {
        self.eq(&Winkel::from(*other))
    }
}

impl PartialOrd<WinkelGradmaß> for Winkel {
    fn partial_cmp(&self, other: &WinkelGradmaß) -> Option<Ordering> {
        self.partial_cmp(&Winkel::from(*other))
    }
}

impl AddAssign<&Winkel> for Winkel {
    fn add_assign(&mut self, Winkel(other): &Winkel) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 += other;
        }
    }
}

impl AddAssign<&WinkelGradmaß> for Winkel {
    fn add_assign(&mut self, WinkelGradmaß(other): &WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 += other.to_radians();
        }
    }
}

impl AddAssign<Winkel> for Winkel {
    fn add_assign(&mut self, rhs: Winkel) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self += &rhs;
        }
    }
}

impl AddAssign<WinkelGradmaß> for Winkel {
    fn add_assign(&mut self, rhs: WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self += &rhs;
        }
    }
}

impl<T> AddAssign<&mut T> for Winkel
where
    Winkel: for<'s> AddAssign<&'s T>,
{
    fn add_assign(&mut self, rhs: &mut T) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self += &*rhs;
        }
    }
}

impl<T> Add<T> for Winkel
where
    Winkel: AddAssign<T>,
{
    type Output = Self;

    fn add(mut self, other: T) -> Winkel {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self += other;
        }
        self
    }
}

impl SubAssign<&Winkel> for Winkel {
    fn sub_assign(&mut self, Winkel(other): &Winkel) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 -= other;
        }
    }
}

impl SubAssign<&WinkelGradmaß> for Winkel {
    fn sub_assign(&mut self, WinkelGradmaß(other): &WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 -= other.to_radians();
        }
    }
}

impl SubAssign<Winkel> for Winkel {
    fn sub_assign(&mut self, rhs: Winkel) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self -= &rhs;
        }
    }
}

impl SubAssign<WinkelGradmaß> for Winkel {
    fn sub_assign(&mut self, rhs: WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self -= &rhs;
        }
    }
}

impl<T> SubAssign<&mut T> for Winkel
where
    Winkel: for<'s> SubAssign<&'s T>,
{
    fn sub_assign(&mut self, rhs: &mut T) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self -= &*rhs;
        }
    }
}

impl<T> Sub<T> for Winkel
where
    Winkel: SubAssign<T>,
{
    type Output = Self;

    fn sub(mut self, other: T) -> Self::Output {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self -= other;
        }
        self
    }
}

impl Neg for Winkel {
    type Output = Self;

    fn neg(self) -> Self {
        Winkel(-self.0)
    }
}

impl MulAssign<f32> for Winkel {
    fn mul_assign(&mut self, rhs: f32) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 *= rhs;
        }
    }
}

impl Mul<f32> for Winkel {
    type Output = Self;

    fn mul(mut self, other: f32) -> Self {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self *= other;
        }
        self
    }
}

impl Mul<Winkel> for f32 {
    type Output = Winkel;

    fn mul(self, other: Winkel) -> Winkel {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            other * self
        }
    }
}

impl DivAssign<f32> for Winkel {
    fn div_assign(&mut self, rhs: f32) {
        // Wie f32: Schlimmstenfalls wird eine NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 /= rhs;
        }
    }
}

impl Div<f32> for Winkel {
    type Output = Self;

    fn div(mut self, other: f32) -> Self {
        // Wie f32: Schlimmstenfalls wird eine NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self /= other;
        }
        self
    }
}

impl Div<Winkel> for f32 {
    type Output = Winkel;

    fn div(self, other: Winkel) -> Winkel {
        // Wie f32: Schlimmstenfalls wird eine NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            other / self
        }
    }
}

impl Trigonometrie for Winkel {
    fn abs(&self) -> Winkel {
        Winkel(self.0.abs())
    }

    fn normalisiert(mut self) -> Self {
        while self < -PI {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                self += TAU;
            }
        }
        while self >= PI {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                self -= TAU;
            }
        }
        self
    }

    fn cos(&self) -> Skalar {
        Skalar(self.0.cos())
    }

    fn sin(&self) -> Skalar {
        Skalar(self.0.sin())
    }

    fn tan(&self) -> Skalar {
        Skalar(self.0.tan())
    }

    fn acos(input: Skalar) -> Self {
        Winkel(input.0.acos())
    }

    fn asin(input: Skalar) -> Self {
        Winkel(input.0.asin())
    }

    fn atan(input: Skalar) -> Self {
        Winkel(input.0.atan())
    }
}

impl ParseArgument for Winkel {
    fn argumente<'t>(
        beschreibung: Beschreibung<'t, Self>,
        invertiere_präfix: impl Into<Vergleich<'t>>,
        invertiere_infix: impl Into<Vergleich<'t>>,
        wert_infix: impl Into<Vergleich<'t>>,
        meta_var: &'t str,
    ) -> kommandozeilen_argumente::Argumente<'t, Self, String> {
        kommandozeilen_argumente::Argumente::konvertiere(
            Winkel,
            f32::argumente(
                beschreibung.konvertiere(|winkel| winkel.0),
                invertiere_präfix,
                invertiere_infix,
                wert_infix,
                meta_var,
            ),
        )
    }

    fn standard() -> Option<Self> {
        Some(Winkel(0.))
    }
}

// TODO behandeln erfordert Anpassung des public API.
#[allow(clippy::module_name_repetitions)]
/// Winkel \[`Gradmaß\`].
///
/// Die [`PartialEq`]- und [`PartialOrd`]-Instanzen sind abgeleitet und normalisieren die Winkel NICHT,
/// bevor sie verglichen werden.
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub struct WinkelGradmaß(f32);

impl WinkelGradmaß {
    /// Konstruktor
    #[must_use]
    pub const fn neu(grad: f32) -> Self {
        WinkelGradmaß(grad)
    }
}

impl From<Winkel> for WinkelGradmaß {
    fn from(Winkel(bogenmaß): Winkel) -> WinkelGradmaß {
        WinkelGradmaß(bogenmaß.to_degrees())
    }
}

impl PartialEq<Winkel> for WinkelGradmaß {
    fn eq(&self, other: &Winkel) -> bool {
        Winkel::from(*self).eq(other)
    }
}

impl PartialOrd<Winkel> for WinkelGradmaß {
    fn partial_cmp(&self, other: &Winkel) -> Option<Ordering> {
        Winkel::from(*self).partial_cmp(other)
    }
}

impl AddAssign<WinkelGradmaß> for WinkelGradmaß {
    fn add_assign(&mut self, rhs: WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 += rhs.0;
        }
    }
}

impl Add<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn add(mut self, other: WinkelGradmaß) -> WinkelGradmaß {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self += other;
        }
        self
    }
}

impl Add<Winkel> for WinkelGradmaß {
    type Output = Winkel;

    fn add(self, other: Winkel) -> Winkel {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            other + self
        }
    }
}

impl SubAssign<WinkelGradmaß> for WinkelGradmaß {
    fn sub_assign(&mut self, rhs: WinkelGradmaß) {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self.0 -= rhs.0;
        }
    }
}

impl Sub<WinkelGradmaß> for WinkelGradmaß {
    type Output = Self;

    fn sub(mut self, other: WinkelGradmaß) -> WinkelGradmaß {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self -= other;
        }
        self
    }
}

impl Sub<Winkel> for WinkelGradmaß {
    type Output = Winkel;

    fn sub(self, other: Winkel) -> Winkel {
        // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
        #[allow(clippy::arithmetic_side_effects)]
        {
            other - self
        }
    }
}

impl Neg for WinkelGradmaß {
    type Output = Self;

    fn neg(self) -> Self {
        WinkelGradmaß(-self.0)
    }
}

impl Mul<f32> for WinkelGradmaß {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        WinkelGradmaß(other * self.0)
    }
}

impl Mul<WinkelGradmaß> for f32 {
    type Output = WinkelGradmaß;

    fn mul(self, WinkelGradmaß(other): WinkelGradmaß) -> WinkelGradmaß {
        WinkelGradmaß(self * other)
    }
}

impl Trigonometrie for WinkelGradmaß {
    fn abs(&self) -> WinkelGradmaß {
        WinkelGradmaß(self.0.abs())
    }

    fn normalisiert(mut self) -> Self {
        while self < WinkelGradmaß(-180.) {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                self += WinkelGradmaß(360.);
            }
        }
        while self >= WinkelGradmaß(180.) {
            // Wie f32: Schlimmstenfalls kommt es zu Genauigkeits-Problemen.
            #[allow(clippy::arithmetic_side_effects)]
            {
                self -= WinkelGradmaß(360.);
            }
        }
        self
    }

    fn cos(&self) -> Skalar {
        Skalar(self.0.to_radians().cos())
    }

    fn sin(&self) -> Skalar {
        Skalar(self.0.to_radians().sin())
    }

    fn tan(&self) -> Skalar {
        Skalar(self.0.to_radians().tan())
    }

    fn acos(input: Skalar) -> Self {
        WinkelGradmaß(input.0.acos().to_degrees())
    }

    fn asin(input: Skalar) -> Self {
        WinkelGradmaß(input.0.asin().to_degrees())
    }

    fn atan(input: Skalar) -> Self {
        WinkelGradmaß(input.0.atan().to_degrees())
    }
}
