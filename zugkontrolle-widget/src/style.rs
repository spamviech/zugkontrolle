//! Style Definitionen.

#![expect(
    clippy::pub_use,
    reason = "style::Container fühlt sich viel natürlicher an als style::container::Container"
)]

pub mod button;
pub mod container;
pub mod linie;
pub mod sammlung;
pub mod streckenabschnitt;
pub mod tab_bar;
pub mod thema;

pub use self::{
    button::Button, container::Container, linie::Linie, sammlung::Sammlung, tab_bar::TabBar,
    thema::Thema,
};
