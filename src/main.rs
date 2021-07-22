//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use iced::{Application, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::{
    anschluss::anschlüsse::{self, Anschlüsse},
    application::icon::icon,
    args::{self, Args},
    Lego, Märklin, Zugkontrolle,
};

fn main() -> Result<(), Error> {
    let args = Args::from_env();
    let verbose = args.verbose;
    let zugtyp = args.zugtyp;

    let log_level = if verbose { log::LevelFilter::Debug } else { log::LevelFilter::Warn };
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log_level)
        .init()
        .expect("failed to initialize error logging");

    Anschlüsse::neu().map_err(Error::from).and_then(|anschlüsse| {
        let settings = Settings {
            window: iced::window::Settings {
                size: (1024, 768),
                icon: Some(icon()),
                ..Default::default()
            },
            ..Settings::with_flags((anschlüsse, args))
        };
        match zugtyp {
            args::Zugtyp::Märklin => Zugkontrolle::<Märklin>::run(settings),
            args::Zugtyp::Lego => Zugkontrolle::<Lego>::run(settings),
        }
        .map_err(Error::from)
    })
}

#[derive(Debug)]
enum Error {
    Iced(iced::Error),
    Anschlüsse(anschlüsse::Error),
}
impl From<iced::Error> for Error {
    fn from(error: iced::Error) -> Self {
        Error::Iced(error)
    }
}
impl From<anschlüsse::Error> for Error {
    fn from(error: anschlüsse::Error) -> Self {
        Error::Anschlüsse(error)
    }
}
