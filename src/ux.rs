//! Ganzzahlen ohne Vorzeichen, die weniger als ein Byte benötigen.
//! [Add]-Implementierung verwendet [saturating_add].

use std::{
    fmt::{self, Display, Formatter},
    ops::Add,
};

use serde::{Deserialize, Serialize};

macro_rules! definiere_typ {
    ($ident: ident, $max:expr, $docstring: tt) => {
        #[doc = $docstring]
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
            type Error = ZuGroß;

            fn try_from(value: u8) -> Result<Self, Self::Error> {
                if value > $ident::MAX.0 {
                    Err(ZuGroß(value))
                } else {
                    Ok($ident(value))
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
            /// Iterator über alle Werte.
            pub fn alle_werte() -> impl Iterator<Item = $ident> {
                (0..=$max).map($ident)
            }
        }
    };
}

definiere_typ! {u3, 7, "Datentyp mit maximal 3 Bytes ohne Vorzeichen (0-7)."}
definiere_typ! {u7, 127, "Datentyp mit maximal 7 Bytes ohne Vorzeichen (0-127)."}

/// Fehler beim konvertieren von [u8] in einen kleineren Datentyp.
#[derive(Debug, Clone, Copy)]
pub struct ZuGroß(pub u8);
