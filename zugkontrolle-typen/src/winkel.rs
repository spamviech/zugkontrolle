//! Winkel in Bogen- und Gradmaß.

use std::f32::consts;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};

use kommandozeilen_argumente::{Beschreibung, ParseArgument, Vergleich};
use serde::{Deserialize, Serialize};

use crate::skalar::Skalar;

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

impl Winkel {
    /// Winkel \[`Gradmaß\`].
    #[must_use]
    pub fn gradmaß(gradmaß: f32) -> Winkel {
        Winkel(gradmaß.to_radians())
    }

    /// Absoluter Wert.
    #[must_use]
    pub fn abs(&self) -> Winkel {
        Winkel(self.0.abs())
    }

    /// Normalisiert in den äquivalenten Bereich zu `[-π,π)`.
    #[must_use]
    pub fn normalisiert(mut self) -> Self {
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

    /// Kosinus
    #[must_use]
    pub fn cos(&self) -> Skalar {
        Skalar(self.0.cos())
    }

    /// Sinus
    #[must_use]
    pub fn sin(&self) -> Skalar {
        Skalar(self.0.sin())
    }

    /// Tangens
    #[must_use]
    pub fn tan(&self) -> Skalar {
        Skalar(self.0.tan())
    }

    /// Inverser Kosinus
    #[must_use]
    pub fn acos(input: Skalar) -> Self {
        Winkel(input.0.acos())
    }

    /// Inverser Sinus
    #[must_use]
    pub fn asin(input: Skalar) -> Self {
        Winkel(input.0.asin())
    }

    /// Inverser Tangens
    #[must_use]
    pub fn atan(input: Skalar) -> Self {
        Winkel(input.0.atan())
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

impl AddAssign<Winkel> for Winkel {
    fn add_assign(&mut self, rhs: Winkel) {
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

impl SubAssign<Winkel> for Winkel {
    fn sub_assign(&mut self, rhs: Winkel) {
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
