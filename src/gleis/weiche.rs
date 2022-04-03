//! Definition und zeichnen einer Weiche.

// HACK cargo check takes very long, this should reduce it until the lint is addressed
#![allow(missing_docs)]

pub mod dreiwege;
pub mod gerade;
pub mod kurve;
pub mod s_kurve;

pub use dreiwege::{DreiwegeWeiche, DreiwegeWeicheSerialisiert, DreiwegeWeicheUnit};
pub use gerade::{Orientierung, Weiche, WeicheSerialisiert, WeicheUnit};
pub use kurve::{KurvenWeiche, KurvenWeicheSerialisiert, KurvenWeicheUnit};
pub use s_kurve::{SKurvenWeiche, SKurvenWeicheSerialisiert, SKurvenWeicheUnit};
