//! Steuerung einer Model-Eisenbahn über einen raspberry pi

pub mod anschluss;
pub mod application;
pub mod args;
pub mod farbe;
pub mod lookup;
pub mod non_empty;
pub mod steuerung;
pub mod zugtyp;

// include std in doc generated by `cargo doc`
// https://github.com/rust-lang/rfcs/issues/2324#issuecomment-502437904
#[cfg(doc)]
#[doc(inline)]
pub use std;

pub use application::{gleis::Gleise, Zugkontrolle};
pub use zugtyp::{Lego, Märklin};
