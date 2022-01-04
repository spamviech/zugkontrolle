//! Kommandozeilen-Argumente.

use std::{
    env,
    fmt::{Debug, Display},
    process,
};

use kommandozeilen_argumente::Parse;

use crate::application::gleis::gleise::Modus;

pub use kommandozeilen_argumente::ArgEnum;

#[derive(Debug, Clone, Parse)]
// subcommand umd direkte Verwendung (impl TopLevelCommand) von `argh::from_env` zu verhindern.
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
#[kommandozeilen_argumente(deutsch, version, hilfe)]
pub struct Args {
    /// Verwendeter Zugtyp
    #[kommandozeilen_argumente(standard(Zugtyp::Märklin))]
    pub zugtyp: Zugtyp,

    /// Lade bei Programmstand die angegebene Datei
    pub pfad: Option<String>,

    /// Modus bei Programmstart
    #[kommandozeilen_argumente(standard(Modus::Bauen))]
    pub modus: Modus,

    /// Zoom bei Programmstart
    #[kommandozeilen_argumente(standard(1.))]
    pub zoom: f32,

    /// X-position bei Programmstart
    #[kommandozeilen_argumente(standard(0.))]
    pub x: f32,

    /// Y-position bei Programmstart
    #[kommandozeilen_argumente(standard(0.))]
    pub y: f32,

    /// Winkel bei Programmstart
    #[kommandozeilen_argumente(standard(0.))]
    pub winkel: f32,

    /// I2C channel auf pins 2 und 3 (bus 0 oder 1)
    #[kommandozeilen_argumente(standard(true))]
    pub i2c0_1: bool,

    // /// I2C channel auf pins 2? und ? (bus 2)
    // pub i2c2: bool,
    //
    /// I2C channel auf pins 4 und 5 (bus 3)
    pub i2c3: bool,

    /// I2C channel auf pins 8 und 9 (bus 4)
    pub i2c4: bool,

    /// I2C channel auf pins 12 und 13 (bus 5)
    pub i2c5: bool,

    /// I2C channel auf pins 22 und 23 (bus 6)
    pub i2c6: bool,

    /// Zeige zusätzliche Informationen in der Konsole an
    pub verbose: bool,

    /// Speichere Log-Nachrichten zusätzlich in einer Datei
    pub log_to_file: bool,
}

impl Args {
    /// Parse Kommandozeilen-Argumente.
    /// Ein einzelnes Argument (das nicht mit "-" beginnt) wird als Pfad interpretiert.
    pub fn parse_aus_env() -> Self {
        let mut args: Vec<_> = env::args_os().skip(1).collect();
        if args.len() == 1 {
            if !args
                .first()
                .and_then(|os_string| os_string.to_str())
                .map(|string| string.starts_with('-'))
                .unwrap_or(false)
            {
                // Einzelnes Argument, dass nicht mit '-' beginnt.
                args.insert(0, "--pfad".to_owned().into());
            }
        }
        let (ergebnis, nicht_benötigt) = Args::parse(args.into_iter());
        match ergebnis {
            kommandozeilen_argumente::ParseErgebnis::Wert(wert) if nicht_benötigt.is_empty() => {
                wert
            }
            kommandozeilen_argumente::ParseErgebnis::Wert(_wert) => {
                eprintln!("Nicht verwendete Kommandozeilen-Argumente:");
                for arg in nicht_benötigt {
                    match arg.into_string() {
                        Ok(string) => eprintln!("{}", string),
                        Err(os_string) => eprintln!("{:?}", os_string),
                    }
                }
                process::exit(1)
            }
            kommandozeilen_argumente::ParseErgebnis::FrühesBeenden(nachrichten) => {
                for nachricht in nachrichten {
                    println!("{}", nachricht);
                }
                process::exit(0)
            }
            kommandozeilen_argumente::ParseErgebnis::Fehler(fehler_sammlung) => {
                for fehler in fehler_sammlung {
                    match fehler.als_string() {
                        Ok(string_fehler) => eprintln!("{}", string_fehler.fehlermeldung()),
                        Err(os_string_fehler) => eprintln!("{:?}", os_string_fehler),
                    }
                }
                process::exit(2)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, ArgEnum)]
pub enum Zugtyp {
    Märklin,
    Lego,
}

impl Display for Zugtyp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
