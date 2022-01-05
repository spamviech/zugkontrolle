//! Kommandozeilen-Argumente.

use std::{
    env,
    ffi::OsString,
    fmt::{Debug, Display},
    process,
};

use kommandozeilen_argumente::{parse::ArgumentArt, Arg, Beschreibung, Parse};

use crate::application::{
    gleis::gleise::Modus,
    typen::{skalar::Skalar, vektor::Vektor, winkel::Winkel},
};

pub use kommandozeilen_argumente::ArgEnum;

// TODO Doppelte Kurzformen anpassen!
#[derive(Debug, Clone, Parse)]
// subcommand umd direkte Verwendung (impl TopLevelCommand) von `argh::from_env` zu verhindern.
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
#[kommandozeilen_argumente(deutsch, version, hilfe)]
pub struct Args {
    /// Verwendeter Zugtyp
    #[kommandozeilen_argumente(standard(Zugtyp::Märklin), kurz)]
    pub zugtyp: Zugtyp,

    /// Lade bei Programmstart die angegebene Datei
    #[kommandozeilen_argumente(kurz)]
    pub pfad: Option<String>,

    /// Modus bei Programmstart
    #[kommandozeilen_argumente(standard(Modus::Bauen), kurz)]
    pub modus: Modus,

    /// Zoom bei Programmstart
    #[kommandozeilen_argumente(standard(Skalar(1.)))]
    pub zoom: Skalar,

    /// Position bei Programmstart
    #[kommandozeilen_argumente(standard(Vektor::null_vektor()))]
    pub position: Vektor,

    /// Winkel bei Programmstart
    #[kommandozeilen_argumente(standard(Winkel(0.)))]
    pub winkel: Winkel,

    #[kommandozeilen_argumente(glätten)]
    pub i2c_settings: I2cSettings,

    /// Zeige zusätzliche Informationen in der Konsole an
    pub verbose: bool,

    /// Speichere Log-Nachrichten zusätzlich in einer Datei
    #[kommandozeilen_argumente(kurz = "l")]
    pub erstelle_log_datei: bool,
}

/// Einstellung über aktivierte I2c-Channel
#[derive(Debug, Clone, Copy, Parse)]
#[kommandozeilen_argumente(deutsch)]
pub struct I2cSettings {
    /// I2C channel auf pins 2 und 3 (bus 0 oder 1)
    #[kommandozeilen_argumente(standard(true))]
    pub i2c0_1: bool,
    // /// I2C channel auf pins 2? und ? (bus 2)
    // pub i2c2: bool,
    /// I2C channel auf pins 4 und 5 (bus 3)
    pub i2c3: bool,
    /// I2C channel auf pins 8 und 9 (bus 4)
    pub i2c4: bool,
    /// I2C channel auf pins 12 und 13 (bus 5)
    pub i2c5: bool,
    /// I2C channel auf pins 22 und 23 (bus 6)
    pub i2c6: bool,
}

impl ArgumentArt for Vektor {
    fn erstelle_arg(
        beschreibung: Beschreibung<Self>,
        _invertiere_prefix: &'static str,
        meta_var: &str,
    ) -> Arg<Self, OsString> {
        Arg::wert_allgemein(
            beschreibung,
            meta_var.to_owned(),
            None,
            |arg| {
                if let Some((x, y)) = arg
                    .to_str()
                    .and_then(|s| s.strip_prefix('('))
                    .and_then(|s| s.strip_suffix(')'))
                    .and_then(|s| s.split_once(','))
                    .and_then(|(x_str, y_str)| Some((x_str.parse().ok()?, y_str.parse().ok()?)))
                {
                    return Ok(Vektor { x: Skalar(x), y: Skalar(y) });
                }
                Err(arg.to_owned())
            },
            |Vektor { x, y }| format!("({:.2}, {:.2})", x.0, y.0),
        )
    }

    fn standard() -> Option<Self> {
        Some(Vektor::null_vektor())
    }
}

impl ArgumentArt for Winkel {
    fn erstelle_arg(
        beschreibung: Beschreibung<Self>,
        _invertiere_prefix: &'static str,
        meta_var: &str,
    ) -> Arg<Self, OsString> {
        Arg::wert_allgemein(
            beschreibung,
            meta_var.to_owned(),
            None,
            |arg| {
                if let Some(winkel) = arg.to_str().and_then(|s| s.parse().ok()) {
                    Ok(Winkel(winkel))
                } else {
                    Err(arg.to_owned())
                }
            },
            |winkel| format!("{}", winkel.0),
        )
    }

    fn standard() -> Option<Self> {
        Some(Winkel(0.))
    }
}

impl ArgumentArt for Skalar {
    fn erstelle_arg(
        beschreibung: Beschreibung<Self>,
        _invertiere_prefix: &'static str,
        meta_var: &str,
    ) -> Arg<Self, OsString> {
        Arg::wert_allgemein(
            beschreibung,
            meta_var.to_owned(),
            None,
            |arg| {
                if let Some(skalar) = arg.to_str().and_then(|s| s.parse().ok()) {
                    Ok(Skalar(skalar))
                } else {
                    Err(arg.to_owned())
                }
            },
            |skalar| format!("{}", skalar.0),
        )
    }

    fn standard() -> Option<Self> {
        None
    }
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
