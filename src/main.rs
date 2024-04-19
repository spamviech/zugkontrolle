//! Steuerung einer Modell-Eisenbahn über einen Raspberry Pi.

// Zu viele/große dependencies, um das wirklich zu vermeiden.
#![allow(clippy::multiple_crate_versions)]

use flexi_logger::{Duplicate, FileSpec, FlexiLoggerError, LogSpecBuilder, Logger, LoggerHandle};
use iced::{window, Application, Settings, Size};
use log::LevelFilter;

use zugkontrolle_anschluss::Lager;
use zugkontrolle_application::{icon::icon, Fehler, Flags, Zugkontrolle};
use zugkontrolle_argumente::{Argumente, ZugtypArgument};
use zugkontrolle_gleis::{steuerung::geschwindigkeit::Leiter, zugtyp::Zugtyp};
use zugkontrolle_widget::fonts;

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

    /// Erstelle die Settings-Struktur für [`Zugkontrolle::run`].
    fn erstelle_settings<L: Leiter>(
        argumente: Argumente,
        lager: Lager,
        zugtyp: &'static Zugtyp<L>,
    ) -> Settings<Flags<L>> {
        Settings {
            window: window::Settings {
                size: Size { width: 800., height: 480. },
                icon: icon(),
                ..window::Settings::default()
            },
            default_font: fonts::REGULAR,
            fonts: fonts::benötigte_font_bytes(),
            ..Settings::with_flags((argumente, lager, zugtyp))
        }
    }

    let Argumente { i2c_settings, zugtyp, verbose, log_datei, .. } = argumente;
    let lager = Lager::neu(i2c_settings)?;

    let logger_handle = start_logger(verbose, log_datei)?;

    match zugtyp {
        ZugtypArgument::Märklin => {
            Zugkontrolle::run(erstelle_settings(argumente, lager, Zugtyp::märklin()))
        },
        ZugtypArgument::Lego => {
            Zugkontrolle::run(erstelle_settings(argumente, lager, Zugtyp::lego()))
        },
    }?;

    // explizit drop aufrufen, damit logger_handle auf jeden Fall lang genau in scope bleibt.
    drop(logger_handle);

    Ok(())
}
