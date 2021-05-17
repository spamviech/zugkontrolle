//! Level eines Anschluss

#[cfg(not(raspi))]
use std::ops::Not;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(raspi)] {
        pub use rppal::gpio::Level;
    } else {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum Level {
            Low,
            High,
        }
        impl Not for Level {
            type Output = Self;

            fn not(self) -> Self::Output {
                match self {
                    Level::Low => Level::High,
                    Level::High => Level::Low,
                }
            }
        }
    }
}
