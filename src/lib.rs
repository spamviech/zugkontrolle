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
// Erlaube mehr rekursive Aufrufe von Macros.
#![recursion_limit = "256"]

pub mod anschluss;
pub mod application;
pub mod argumente;
#[path = "eingeschränkt.rs"]
pub mod eingeschränkt;
pub mod gleis;
pub mod nachschlagen;
pub mod rppal;
pub mod steuerung;
pub mod typen;
pub mod unicase_ord;
pub mod void;
pub mod zugtyp;

pub use crate::{
    application::{ausführen, ausführen_aus_env, Fehler, Zugkontrolle},
    argumente::Argumente,
    gleis::gleise::Gleise,
    zugtyp::Zugtyp,
};

#[cfg(test)]
use self::test_util::init_test_logging;

#[cfg(test)]
mod test_util {
    use flexi_logger::{LogSpecBuilder, Logger, LoggerHandle};
    use log::LevelFilter;
    use parking_lot::{const_mutex, Mutex};

    static LOGGER_HANDLE: Mutex<Option<LoggerHandle>> = const_mutex(None);

    /// Initialisiere FlexiLogger einmalig, speichere den Handle in einer globalen Variable.
    ///
    /// Notwendig, da `cargo test` mehrere Tests parallel ausführt, aber nur ein Logger aktiv sein kann.
    pub(crate) fn init_test_logging() {
        let handle = &mut *LOGGER_HANDLE.lock();
        if handle.is_none() {
            let mut log_spec_builder = LogSpecBuilder::new();
            let _ = log_spec_builder
                .default(LevelFilter::Error)
                .module("zugkontrolle", LevelFilter::Warn);
            let log_spec = log_spec_builder.finalize();
            *handle = Some(
                Logger::with(log_spec)
                    .log_to_stderr()
                    .start()
                    .expect("Logging initialisieren fehlgeschlagen!"),
            )
        }
    }
}
