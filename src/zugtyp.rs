//! Zugtyp Trait + Phantom-Typen + Spurweite

/// Spurweite \[mm\]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Spurweite(pub f32);
pub trait Zugtyp {
    const SPURWEITE: Spurweite;
}

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Maerklin;
impl Zugtyp for Maerklin {
    #[allow(non_upper_case_globals)]
    const SPURWEITE: Spurweite = Spurweite(16.5);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Lego;
impl Zugtyp for Lego {
    #[allow(non_upper_case_globals)]
    const SPURWEITE: Spurweite = Spurweite(38.);
}
