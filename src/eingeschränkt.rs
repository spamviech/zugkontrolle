//! Zahlen, die auf einen bestimmten Bereich eingeschränkt sind.

use std::{
    fmt::{self, Display, Formatter},
    ops::{Add, Mul, Sub},
};

use serde::{Deserialize, Serialize};

/// Fehler beim konvertieren in einen eingeschränkten Datentyp.
#[derive(Debug, Clone, Copy)]
pub struct InvaliderWert<T>(pub T);

macro_rules! definiere_u8_typ {
    ($ident: ident, $max: expr, $docstring: tt) => {
        #[doc = $docstring]
        /// [Add]-Implementierung verwendet [saturating_add].
        #[derive(
            Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
        )]
        #[allow(non_camel_case_types)]
        pub struct $ident(u8);

        impl From<$ident> for u8 {
            fn from(value: $ident) -> Self {
                value.0
            }
        }

        impl From<$ident> for u16 {
            fn from(value: $ident) -> Self {
                Self::from(value.0)
            }
        }

        impl From<$ident> for u32 {
            fn from(value: $ident) -> Self {
                Self::from(value.0)
            }
        }

        impl From<$ident> for usize {
            fn from(value: $ident) -> Self {
                Self::from(value.0)
            }
        }

        impl TryFrom<u8> for $ident {
            type Error = InvaliderWert<u8>;

            fn try_from(wert: u8) -> Result<Self, Self::Error> {
                Self::neu(wert)
            }
        }

        impl Display for $ident {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl Add for $ident {
            type Output = Self;

            fn add(self, other: Self) -> Self::Output {
                let u8 = self.0.saturating_add(other.0);
                if u8 > Self::MAX.0 {
                    Self::MAX
                } else {
                    Self(u8)
                }
            }
        }

        impl $ident {
            /// Kleinster Wert (0).
            pub const MIN: Self = $ident(0);

            /// Größter darstellbarer Wert.
            pub const MAX: Self = $ident($max);

            /// Versuche einen neuen
            #[doc = stringify!([$ident])]
            /// zu erstellen.
            /// Schlägt bei zu großen Werten fehl.
            pub const fn neu(wert: u8) -> Result<Self, InvaliderWert<u8>> {
                if wert > $ident::MAX.0 {
                    Err(InvaliderWert(wert))
                } else {
                    Ok($ident(wert))
                }
            }

            /// Iterator über alle Werte.
            pub fn alle_werte() -> impl Iterator<Item = $ident> {
                (0..=$max).map($ident)
            }
        }
    };
}

definiere_u8_typ! {kleiner_8, 7, "Datentyp mit maximal 3 Bytes ohne Vorzeichen (0-7)."}
definiere_u8_typ! {kleiner_128, 127, "Datentyp mit maximal 7 Bytes ohne Vorzeichen (0-127)."}

macro_rules! definiere_f64_typ {
    ($ident: ident, $min: expr, $max: expr, $($docstring: tt),+) => {
        $(#[doc = $docstring])+
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize)]
        pub struct $ident(f64);

        impl $ident {
            /// Kleinster erlaubter Wert.
            pub const MIN: Self = Self($min);

            /// Größter erlaubter Wert.
            pub const MAX: Self = Self($max);

            /// Erstelle einen neuen
            #[doc = stringify!([$ident])]
            /// -Wert, ohne die Grenzen zu überprüfen.
            pub(crate) const fn neu_unchecked(wert: f64) -> Self {
                Self(wert)
            }
        }

        impl From<$ident> for f64 {
            fn from(input: $ident) -> Self {
                input.0
            }
        }

        impl TryFrom<f64> for $ident {
            type Error = InvaliderWert<f64>;

            fn try_from(wert: f64) -> Result<Self, Self::Error> {
                if wert >= Self::MIN.0 && wert <= Self::MAX.0 {
                    Ok(Self(wert))
                } else {
                    Err(InvaliderWert(wert))
                }
            }
        }

        impl Display for $ident {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl Add for $ident {
            type Output = Self;

            fn add(self, other: Self) -> Self::Output {
                let unbeschränkt = self.0 + other.0;
                if unbeschränkt >= Self::MIN.0 && unbeschränkt <= Self::MAX.0 {
                    Self(unbeschränkt)
                } else if unbeschränkt > Self::MAX.0 {
                    Self::MAX
                } else if unbeschränkt < Self::MIN.0 {
                    Self::MIN
                } else {
                    // NaN
                    let self_pos = self.0 > 0.;
                    let self_greater_abs = self.0.abs() > other.0.abs();
                    let other_pos = other.0 > 0.;
                    if (self_pos && other_pos)
                        || (self_pos && self_greater_abs)
                        || (other_pos && !self_greater_abs)
                    {
                        Self::MAX
                    } else {
                        Self::MIN
                    }
                }
            }
        }

        impl Sub for $ident {
            type Output = Self;

            fn sub(self, other: Self) -> Self::Output {
                let unbeschränkt = self.0 - other.0;
                if unbeschränkt >= Self::MIN.0 && unbeschränkt <= Self::MAX.0 {
                    Self(unbeschränkt)
                } else if unbeschränkt > Self::MAX.0 {
                    Self::MAX
                } else if unbeschränkt < Self::MIN.0 {
                    Self::MIN
                } else {
                    // NaN
                    let self_pos = self.0 > 0.;
                    let self_greater_abs = self.0.abs() > other.0.abs();
                    let other_neg = other.0 < 0.;
                    if (self_pos && other_neg)
                        || (self_pos && self_greater_abs)
                        || (other_neg && !self_greater_abs)
                    {
                        Self::MAX
                    } else {
                        Self::MIN
                    }
                }
            }
        }
    };
}

definiere_f64_typ! {
    NullBisEins,
    0.,
    1.,
    "Ein Wert mit der Eigenschaft `0 <= x <= 1`.",
    "",
    "[Add]- und [Sub]-Implementierungen sind saturating, z.B. `0.7+0.5=1`."
}

impl Mul for NullBisEins {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        // Beide Werte sind im Bereich 0 <= x <= 1.
        // Das Ergebnis ist definitiv im selben Bereich:
        // wird nur kleiner, minimal 0, oder bleibt gleich.
        NullBisEins(self.0 * other.0)
    }
}

definiere_f64_typ! {
    NichtNegativ,
    0.,
    f64::MAX,
    "Ein Wert mit der Eigenschaft 0 <= x",
    "",
    "[Add]- und [Sub]-Implementierungen sind saturating, z.B. `0.5-0.7=0`."
}
