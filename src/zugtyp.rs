//! zugtyp phantom types

// TODO
// non_ascii_idents might be stabilized soon
// use english names until then :(
// (nightly crashes atm on Sized-check)
// https://github.com/rust-lang/rust/issues/55467
#[derive(Debug, Clone, Copy)]
pub struct Maerklin;

#[derive(Debug, Clone, Copy)]
pub struct Lego;
