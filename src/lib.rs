//! Steuerung einer Model-Eisenbahn über einen raspberry pi

pub mod gleis;
pub mod zugtyp;
pub mod anschluss;

// include std in doc generated by `cargo doc`
// https://github.com/rust-lang/rfcs/issues/2324#issuecomment-502437904
#[cfg(doc)]
#[doc(inline)]
pub use std;
