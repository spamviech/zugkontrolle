//! Skalar-Werte für Größen auf dem Canvas.

use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use kommandozeilen_argumente::{Beschreibung, ParseArgument, Vergleich};
use serde::{Deserialize, Serialize};

/// Skalar-Werte für Größen auf dem Canvas.
///
/// Methoden bilden den bekannten arithmetischen Körper.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct Skalar(pub f32);

impl Skalar {
    /// Doppelter Wert.
    #[must_use]
    pub fn doppelt(&self) -> Self {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            Skalar(2.) * self
        }
    }

    /// Halber Wert.
    #[must_use]
    pub fn halbiert(&self) -> Self {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            Skalar(0.5) * self
        }
    }

    /// Absoluter Wert.
    #[must_use]
    pub fn abs(&self) -> Self {
        Skalar(self.0.abs())
    }

    /// Kopie des größeren Elements.
    #[must_use]
    pub fn max(&self, other: &Self) -> Self {
        if self > other {
            *self
        } else {
            *other
        }
    }

    /// Kopie des kleineren Elements.
    #[must_use]
    pub fn min(&self, other: &Self) -> Self {
        if self < other {
            *self
        } else {
            *other
        }
    }

    /// Returns a number that represents the sign of self.
    ///
    /// - `1.0` if the number is positive or `INFINITY`
    /// - `0.0` if the number is `+0.0` or `-0.0`
    /// - `-1.0` if the number is negative or `NEG_INFINITY`
    /// - `NaN` if the number is `NaN`
    #[must_use]
    pub fn signum(&self) -> Self {
        if *self == Skalar(0.) {
            Skalar(0.)
        } else {
            Skalar(self.0.signum())
        }
    }
}

// Ein Körper ist eine (additive) abelsche Gruppe
// Halbgruppe
impl AddAssign<&Self> for Skalar {
    fn add_assign(&mut self, rhs: &Self) {
        self.0 += rhs.0;
    }
}

impl AddAssign<&mut Self> for Skalar {
    fn add_assign(&mut self, rhs: &mut Self) {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self += &*rhs;
        }
    }
}

impl AddAssign<Self> for Skalar {
    fn add_assign(&mut self, rhs: Self) {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self += &rhs;
        }
    }
}

impl<T> Add<T> for Skalar
where
    Skalar: AddAssign<T>,
{
    type Output = Self;

    fn add(mut self, rhs: T) -> Self::Output {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self += rhs;
        }
        self
    }
}

// Monoid
impl Skalar {
    /// Additiv neutrales Element `Skalar(0.)`.
    #[must_use]
    pub const fn additiv_neutral() -> Self {
        Skalar(0.)
    }
}

// inverses Element
impl Neg for Skalar {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.0 = -self.0;
        self
    }
}

impl SubAssign<Self> for Skalar {
    fn sub_assign(&mut self, rhs: Self) {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        // Der Wert wird vor der Addition negiert.
        #[allow(clippy::suspicious_op_assign_impl)]
        {
            *self += rhs.neg();
        }
    }
}

impl SubAssign<&Self> for Skalar {
    fn sub_assign(&mut self, rhs: &Self) {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self -= *rhs;
        }
    }
}

impl SubAssign<&mut Self> for Skalar {
    fn sub_assign(&mut self, rhs: &mut Self) {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self -= &*rhs;
        }
    }
}

impl<T> Sub<T> for Skalar
where
    Skalar: SubAssign<T>,
{
    type Output = Self;

    fn sub(mut self, rhs: T) -> Self::Output {
        // Wie bei f32: Schlimmstenfalls kommt es zu Genauigkeits-Fehlern.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self -= rhs;
        }
        self
    }
}

// Körper ohne additiv-neutrales Element (0) ist eine (multiplikative) abelsche Gruppe Halbgruppe.
impl MulAssign<&Self> for Skalar {
    fn mul_assign(&mut self, rhs: &Self) {
        self.0 *= rhs.0;
    }
}

impl MulAssign<&mut Self> for Skalar {
    fn mul_assign(&mut self, rhs: &mut Self) {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self *= &*rhs;
        }
    }
}

impl MulAssign<Self> for Skalar {
    fn mul_assign(&mut self, rhs: Self) {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self *= &rhs;
        }
    }
}

impl<T> Mul<T> for Skalar
where
    Skalar: MulAssign<T>,
{
    type Output = Self;

    fn mul(mut self, rhs: T) -> Self::Output {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self *= rhs;
        }
        self
    }
}

// Monoid
impl Skalar {
    /// Multiplikativ neutrales Element `Skalar(1.)`.
    #[must_use]
    pub const fn multiplikativ_neutral() -> Self {
        Skalar(1.)
    }
}

// Inverses Element (via division)
impl DivAssign<&Self> for Skalar {
    fn div_assign(&mut self, rhs: &Self) {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self *= Skalar(1. / rhs.0);
        }
    }
}

impl DivAssign<&mut Self> for Skalar {
    fn div_assign(&mut self, rhs: &mut Self) {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self /= &*rhs;
        }
    }
}

impl DivAssign<Self> for Skalar {
    fn div_assign(&mut self, rhs: Self) {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            *self /= &rhs;
        }
    }
}

impl<T> Div<T> for Skalar
where
    Skalar: DivAssign<T>,
{
    type Output = Self;

    fn div(mut self, rhs: T) -> Self::Output {
        // Wie bei f32: Schlimmstenfalls wird ein NaN-Wert erzeugt.
        #[allow(clippy::arithmetic_side_effects)]
        {
            self /= rhs;
        }
        self
    }
}

impl ParseArgument for Skalar {
    fn argumente<'t>(
        beschreibung: Beschreibung<'t, Self>,
        invertiere_präfix: impl Into<Vergleich<'t>>,
        invertiere_infix: impl Into<Vergleich<'t>>,
        wert_infix: impl Into<Vergleich<'t>>,
        meta_var: &'t str,
    ) -> kommandozeilen_argumente::Argumente<'t, Self, String> {
        kommandozeilen_argumente::Argumente::konvertiere(
            Skalar,
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
        None
    }
}
