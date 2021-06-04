//! Definition und zeichnen einer Weiche

pub mod dreiwege;
pub mod gerade;
pub mod kurve;
pub mod s_kurve;

// pub use *, weil rust-analyzer bei explizitem import von durch Macros erzeugten Typen meckert
pub use dreiwege::*;
// pub use dreiwege::{DreiwegeWeiche, DreiwegeWeicheSave, DreiwegeWeicheUnit};
pub use gerade::*;
// pub use gerade::{Orientierung, Weiche, WeicheSave, WeicheUnit};
pub use kurve::KurvenWeiche;
pub use s_kurve::SKurvenWeiche;
