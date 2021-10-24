//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use iced::{Application, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::{
    anschluss::anschlüsse::{self, Anschlüsse},
    application::{fonts, icon::icon},
    args::{self, Args},
    Lego, Märklin, Zugkontrolle,
};

fn main() -> Result<(), Fehler> {
    let args = Args::from_env();
    let verbose = args.verbose;
    let zugtyp = args.zugtyp;

    let log_level = if verbose { log::LevelFilter::Debug } else { log::LevelFilter::Warn };
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log_level)
        .init()
        .expect("failed to initialize error logging");

    let anschlüsse = Anschlüsse::neu()?;

    let settings = Settings {
        window: iced::window::Settings {
            size: (1024, 768),
            icon: Some(icon()),
            ..Default::default()
        },
        default_font: Some(&fonts::REGULAR),
        ..Settings::with_flags((anschlüsse, args))
    };

    match zugtyp {
        args::Zugtyp::Märklin => Zugkontrolle::<Märklin>::run(settings)?,
        args::Zugtyp::Lego => Zugkontrolle::<Lego>::run(settings)?,
    }

    Ok(())
}

#[derive(Debug)]
enum Fehler {
    Iced(iced::Error),
    Anschlüsse(anschlüsse::Fehler),
}
impl From<iced::Error> for Fehler {
    fn from(error: iced::Error) -> Self {
        Fehler::Iced(error)
    }
}
impl From<anschlüsse::Fehler> for Fehler {
    fn from(error: anschlüsse::Fehler) -> Self {
        Fehler::Anschlüsse(error)
    }
}
