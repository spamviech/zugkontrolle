//! Utility-Funktionen für Tests.

use std::fmt::Debug;

use flexi_logger::{LogSpecBuilder, Logger, LoggerHandle};
use log::LevelFilter;
use parking_lot::{const_mutex, Mutex};
use thiserror::Error;

/// Hilfs-Variable für [`init_test_logging`].
/// Sorgt dafür, dass nur der erste Aufruf einen Logger registriert.
static LOGGER_HANDLE: Mutex<Option<LoggerHandle>> = const_mutex(None);

/// Initialisiere `FlexiLogger` einmalig, speichere den Handle in einer globalen Variable.
///
/// Notwendig, da `cargo test` mehrere Tests parallel ausführt, aber nur ein Logger aktiv sein kann.
///
/// ## Panics
///
/// Wenn der Logger nicht initialisiert werden kann, z.B. bereits einer existiert.
pub fn init_test_logging() {
    let handle = &mut *LOGGER_HANDLE.lock();
    if handle.is_none() {
        let mut log_spec_builder = LogSpecBuilder::new();
        let _ =
            log_spec_builder.default(LevelFilter::Error).module("zugkontrolle", LevelFilter::Warn);
        let log_spec = log_spec_builder.finalize();
        *handle = Some(
            Logger::with(log_spec)
                .log_to_stderr()
                .start()
                .expect("Logging initialisieren fehlgeschlagen!"),
        );
    }
}

/// Eine fehlgeschlagene Annahme in einem Test.
#[derive(Debug, zugkontrolle_macros::From)]
pub enum Expectation {
    /// Es wurde `true` erwartet.
    True(ExpectTrue),
    /// Es wurde die Gleichheit zweier Werte erwartet.
    Eq(ExpectEq),
    /// Es wurde ein Unterschied zweier Werte erwartet.
    Ne(ExpectNe),
    /// Es wurde er erwartet, dass der erste Wert größer ist.
    Gt(ExpectGt),
}

#[derive(Debug, Clone, Copy)]
/// Es wurde `true` erwartet.
pub struct ExpectTrue;

#[allow(clippy::missing_errors_doc)]
/// Gebe [Ok] zurück wenn der wert [true] ist, ansonsten [`Err`].
pub fn expect_true(wert: bool) -> Result<(), ExpectTrue> {
    if wert {
        Ok(())
    } else {
        Err(ExpectTrue)
    }
}

#[derive(Debug, Error)]
/// Es wurde die Gleichheit zweier Werte erwartet.
#[error("Expected {0:?} and {1:?} to be equal!")]
pub struct ExpectEq(Box<dyn Debug>, Box<dyn Debug>);

#[allow(clippy::missing_errors_doc)]
/// Gebe [Ok] zurück wenn beide Werte gleich sind, ansonsten [`Err`].
#[allow(clippy::min_ident_chars)]
pub fn expect_eq<T: 'static + Debug + PartialEq>(a: T, b: T) -> Result<(), ExpectEq> {
    expect_true(a == b).map_err(|_expect_true| ExpectEq(Box::new(a), Box::new(b)))
}

#[derive(Debug, Error)]
/// Es wurde ein Unterschied zweier Werte erwartet.
#[error("Expected {0:?} and {1:?} to be different!")]
pub struct ExpectNe(Box<dyn Debug>, Box<dyn Debug>);

#[allow(clippy::missing_errors_doc)]
/// Gebe [Ok] zurück wenn beide Werte unterschiedlich sind, ansonsten [`Err`].
#[allow(clippy::min_ident_chars)]
pub fn expect_ne<T: 'static + Debug + PartialEq>(a: T, b: T) -> Result<(), ExpectNe> {
    expect_true(a != b).map_err(|_expect_true| ExpectNe(Box::new(a), Box::new(b)))
}

#[derive(Debug, Error)]
/// Es wurde er erwartet, dass der erste Wert größer ist.
#[error("Expected {0:?} to be greater than {1:?}!")]
pub struct ExpectGt(Box<dyn Debug>, Box<dyn Debug>);

#[allow(clippy::missing_errors_doc)]
/// Gebe [Ok] zurück wenn beide Werte unterschiedlich sind, ansonsten [`Err`].
#[allow(clippy::min_ident_chars)]
pub fn expect_gt<T: 'static + Debug + PartialEq>(a: T, b: T) -> Result<(), ExpectGt> {
    expect_true(a != b).map_err(|_expect_true| ExpectGt(Box::new(a), Box::new(b)))
}
