//! Steuerung einer Model-Eisenbahn über einen Raspberry Pi.

// Aktiviere alle Warnungen/Lints, außer:
// box_pointers, non_ascii_idents, unstable_features
#![warn(
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    keyword_idents,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    noop_method_call,
    pointer_structural_match,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]

pub mod anschluss;
pub mod application;
pub mod argumente;
#[path = "eingeschränkt.rs"]
pub mod eingeschränkt;
pub mod gleis;
pub mod maybe_empty;
pub mod nachschlagen;
pub mod rppal;
pub mod steuerung;
pub mod typen;
pub mod void;
pub mod zugtyp;

pub use crate::{
    application::{ausführen, ausführen_aus_env, Zugkontrolle},
    argumente::Argumente,
    gleis::gleise::Gleise,
    zugtyp::Zugtyp,
};
