//! Kommandozeilen-Argumente.

use std::{ffi::OsString, str::FromStr};

use argh::{EarlyExit, FromArgs, TopLevelCommand};
use version::version;

use crate::application::gleis::gleise::Modus;

#[derive(Debug)]
struct Wrapper(Args);
impl TopLevelCommand for Wrapper {}
impl FromArgs for Wrapper {
    fn from_args(command_name: &[&str], args: &[&str]) -> Result<Self, EarlyExit> {
        if args.contains(&"--version") {
            Err(EarlyExit { output: format!("Zugkontrolle {}", version!()), status: Ok(()) })
        } else {
            Ok(Wrapper(if let Some((pfad, [])) = args.split_first() {
                if pfad.starts_with("-") {
                    Args::from_args(command_name, args)?
                } else {
                    Args::from_args(command_name, &["--pfad", pfad])?
                }
            } else {
                Args::from_args(command_name, args)?
            }))
        }
    }
}

#[derive(Debug, Clone, FromArgs)]
// subcommand umd direkte Verwendung (impl TopLevelCommand) von `argh::from_env` zu verhindern.
#[argh(subcommand, name = "Args")]
/// Steuerung einer Modelleisenbahn über einen Raspberry Pi.
pub struct Args {
    #[argh(option, default = "Zugtyp::Märklin")]
    /// verwendeter Zugtyp
    pub zugtyp: Zugtyp,

    #[argh(option, short = 'p')]
    /// dateiname
    pub pfad: Option<String>,

    #[argh(option, short = 'm', default = "Modus::Bauen")]
    /// modus bei Programmstart
    pub modus: Modus,

    #[argh(option, short = 'z', default = "1.")]
    /// zoom bei Programmstart
    pub zoom: f32,

    #[argh(option, short = 'x', default = "0.")]
    /// x-position bei Programmstart
    pub x: f32,

    #[argh(option, short = 'y', default = "0.")]
    /// y-position bei Programmstart
    pub y: f32,

    #[argh(option, short = 'w', default = "0.")]
    /// winkel bei Programmstart
    pub winkel: f32,

    #[argh(option, default = "true")]
    /// i2c channel auf pins 2 und 3 (bus 0 oder 1), standard an
    pub i2c0_1: bool,

    // #[argh(option, default = "false")]
    // /// i2c channel auf pins 2? und ? (bus 2), standard aus
    // pub i2c2: bool,
    //
    #[argh(option, default = "false")]
    /// i2c channel auf pins 4 und 5 (bus 3), standard aus
    pub i2c3: bool,

    #[argh(option, default = "false")]
    /// i2c channel auf pins 8 und 9 (bus 4), standard aus
    pub i2c4: bool,

    #[argh(option, default = "false")]
    /// i2c channel auf pins 12 und 13 (bus 5), standard aus
    pub i2c5: bool,

    #[argh(option, default = "false")]
    /// i2c channel auf pins 22 und 23 (bus 6), standard aus
    pub i2c6: bool,

    #[argh(switch)]
    /// zeige zusätzliche Informationen in der Konsole an
    pub verbose: bool,

    #[argh(switch)]
    /// speichere Log-Nachrichten zusätzlich in einer Datei
    pub log_to_file: bool,

    #[argh(switch)]
    /// zeige die aktuelle Version
    #[allow(dead_code)]
    version: bool,
}

impl Args {
    /// Parse Kommandozeilen-Argumente durch das `argh`-crate.
    ///
    /// Diese Methode berücksichtigt ein `--version` flag, dass zu einem `EarlyExit` führt.
    /// Ein einzelnes Argument (das nicht mit "-" beginnt) wird als Pfad interpretiert.
    pub fn from_env() -> Self {
        let Wrapper(args) = argh::from_env();
        args
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Zugtyp {
    Märklin,
    Lego,
}

impl FromStr for Zugtyp {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Märklin" => Ok(Zugtyp::Märklin),
            "Lego" => Ok(Zugtyp::Lego),
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Debug)]
pub struct ArgName {
    pub long: String,
    pub short: Option<char>,
}

pub struct Parser<T, E> {
    pub parse: Box<dyn Fn(&OsString) -> Result<T, E>>,
}

impl<T, E> std::fmt::Debug for Parser<T, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parser").field("parse", &"<function>").finish()
    }
}

impl<T, E> Parser<T, E> {
    pub fn neu(parse: impl 'static + Fn(&OsString) -> Result<T, E>) -> Self {
        Parser { parse: Box::new(parse) }
    }
}

#[derive(Debug)]
pub enum ParseStringFehler<E> {
    KonvertiereOsString(OsString),
    ParseFehler(E),
}

impl<E> From<E> for ParseStringFehler<E> {
    fn from(fehler: E) -> Self {
        ParseStringFehler::ParseFehler(fehler)
    }
}

impl<T: FromStr> Parser<T, ParseStringFehler<T::Err>> {
    pub fn try_from_str() -> Self {
        Parser {
            parse: Box::new(|os_string| {
                T::from_str(
                    os_string
                        .to_str()
                        .ok_or_else(|| ParseStringFehler::KonvertiereOsString(os_string.clone()))?,
                )
                .map_err(ParseStringFehler::from)
            }),
        }
    }
}
