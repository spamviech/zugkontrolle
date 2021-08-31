//! Definition und zeichnen einer Weiche

pub mod dreiwege;
pub mod gerade;
pub mod kurve;
pub mod s_kurve;

pub use dreiwege::{DreiwegeWeiche, DreiwegeWeicheSerialisiert, DreiwegeWeicheUnit};
pub use gerade::{Orientierung, Weiche, WeicheSerialisiert, WeicheUnit};
pub use kurve::{KurvenWeiche, KurvenWeicheSerialisiert, KurvenWeicheUnit};
pub use s_kurve::{SKurvenWeiche, SKurvenWeicheSerialisiert, SKurvenWeicheUnit};
