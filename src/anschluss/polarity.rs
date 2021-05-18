//! Level eines Anschluss

#[cfg(not(raspi))]
use std::fmt::{Display, Formatter, Result};

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(raspi)] {
        pub use rppal::pwm::Polarity;
    } else {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Polarity {
            Normal,
            Inverse,
        }
        impl Display for Polarity {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result {
                write!(f, "{}", match self {
                    Polarity::Normal => "Normal",
                    Polarity::Inverse => "Inverse",
                })
            }
        }
    }
}
