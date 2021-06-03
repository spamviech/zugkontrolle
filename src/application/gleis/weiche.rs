//! Definition und zeichnen einer Weiche

pub mod dreiwege;
pub mod gerade;
pub mod kurve;
pub mod s_kurve;

pub use dreiwege::{DreiwegeWeiche, DreiwegeWeicheSave};
pub use gerade::{Orientierung, Weiche, WeicheSave};
pub use kurve::KurvenWeiche;
pub use s_kurve::SKurvenWeiche;
