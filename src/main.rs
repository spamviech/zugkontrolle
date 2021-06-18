//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use iced::{Application, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::{
    anschluss::anschlüsse::{self, Anschlüsse},
    application::icon::icon,
    Lego, Märklin, Zugkontrolle,
};

pub mod args;
use self::args::Args;

fn main() -> Result<(), Error> {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    let Args { zugtyp, pfad, modus, .. } = Args::from_env();
    Anschlüsse::neu().map_err(Error::from).and_then(|anschlüsse| {
        let settings = Settings {
            window: iced::window::Settings {
                size: (1024, 768),
                icon: Some(icon()),
                ..Default::default()
            },
            ..Settings::with_flags((anschlüsse, pfad, modus))
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
