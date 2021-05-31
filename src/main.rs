//! Steuerung einer Model-Eisenbahn über einen raspberry pi

use iced::{Application, Settings};
use simple_logger::SimpleLogger;
use zugkontrolle::{Gleise, Lego, Märklin, Zugkontrolle};

pub mod args;

fn main() -> iced::Result {
    SimpleLogger::new()
        .with_level(log::LevelFilter::Error)
        .with_module_level("zugkontrolle", log::LevelFilter::Debug)
        .init()
        .expect("failed to initialize error logging");

    let args::Args { zugtyp, pfad, .. } = args::Args::from_env();
    match zugtyp {
        args::Zugtyp::Märklin => {
            // Märklin-Gleise
            let mut gleise_märklin: Gleise<Märklin> = Gleise::neu();
            if let Some(pfad) = pfad {
                if let Err(error) = gleise_märklin.laden(pfad) {
                    // TODO setzte Start-MessageBox über flags
                    eprintln!("{:?}", error)
                }
            }

            Zugkontrolle::run(Settings {
                window: iced::window::Settings {
                    size: (1024, 768),
                    icon: Some(Zugkontrolle::<Märklin>::icon()),
                    ..Default::default()
                },
                ..Settings::with_flags(gleise_märklin)
            })
        },
        args::Zugtyp::Lego => {
            // Lego-Gleise
            let mut gleise_lego: Gleise<Lego> = Gleise::neu();
            if let Some(pfad) = pfad {
                if let Err(error) = gleise_lego.laden(pfad) {
                    // TODO setzte Start-MessageBox über flags
                    eprintln!("{:?}", error)
                }
            }

            Zugkontrolle::run(Settings {
                window: iced::window::Settings {
                    size: (1024, 768),
                    icon: Some(Zugkontrolle::<Lego>::icon()),
                    ..Default::default()
                },
                ..Settings::with_flags(gleise_lego)
            })
        },
    }
}
