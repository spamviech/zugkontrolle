//! Steuerung einer Modell-Eisenbahn über einen Raspberry Pi.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use std::sync::Arc;

use flexi_logger::{Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger, LoggerHandle};
use log::LevelFilter;
use parking_lot::RwLock;

use zugkontrolle_anschluss::Lager;
use zugkontrolle_application::{Fehler, Zugkontrolle};
use zugkontrolle_argumente::{Argumente, ZugtypArgument};
use zugkontrolle_gleis::zugtyp::Zugtyp;

/// Parse die Kommandozeilen-Argumente und führe die Anwendung aus.
///
/// ## Errors
///
/// Fehler beim Initialisieren der Anwendung.
fn main() -> Result<(), Fehler> {
    let args = Argumente::parse_aus_env_einzelnes_als_pfad();
    ausführen(args)
}

/// Parse die übergebenen Kommandozeilen-Argumente und führe die Anwendung aus.
///
/// ## Errors
///
/// Fehler beim Initialisieren der Anwendung.
pub fn ausführen(argumente: Argumente) -> Result<(), Fehler> {
    /// Initialisiere die Logger-Instanz.
    fn start_logger(verbose: bool, log_datei: bool) -> Result<LoggerHandle, FlexiLoggerError> {
        let log_level = if verbose { LevelFilter::Debug } else { LevelFilter::Warn };
        let mut log_spec_builder = LogSpecBuilder::new();
        let _ = log_spec_builder.default(LevelFilter::Error).module("zugkontrolle", log_level);
        let log_spec = log_spec_builder.finalize();
        let logger_base = Logger::with(log_spec);
        let logger = if log_datei {
            logger_base
                .log_to_file(FileSpec::default().directory("log"))
                .duplicate_to_stderr(Duplicate::All)
        } else {
            logger_base.log_to_stderr()
        };
        logger.start()
    }

    let Argumente { i2c_settings, zugtyp, verbose, log_datei, .. } = argumente;
    let lager = Arc::new(RwLock::new(Lager::neu(i2c_settings)?));

    let logger_handle = start_logger(verbose, log_datei)?;

    match zugtyp {
        ZugtypArgument::Märklin => {
            Zugkontrolle::application(argumente, lager, Zugtyp::märklin()).run()?;
        },
        ZugtypArgument::Lego => {
            Zugkontrolle::application(argumente, lager, Zugtyp::lego()).run()?;
        },
    }

    // explizit drop aufrufen, damit logger_handle auf jeden Fall lang genau in scope bleibt.
    drop(logger_handle);

    Ok(())
}
