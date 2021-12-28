//! Kommandozeilen-Argumente.

use std::{
    env,
    ffi::OsString,
    fmt::{Debug, Display},
    str::FromStr,
};

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

#[derive(Debug)]
#[allow(variant_size_differences)]
pub enum ParsedArgName {
    Short(char),
    Long(String),
}

#[derive(Debug)]
pub enum ParsedArg {
    Flag { name: ParsedArgName, aktiviert: bool },
    Wert { name: ParsedArgName, wert: OsString },
}

#[derive(Debug)]
pub struct ArgName<'t> {
    pub lang: &'t String,
    pub kurz: &'t Option<char>,
}

#[derive(Debug)]
pub enum ArgKonfiguration<'t> {
    Flag(ArgName<'t>),
    Wert(ArgName<'t>),
}

#[derive(Debug)]
pub enum ParseArgFehler {
    KonvertiereOsString(OsString),
}

impl ParsedArg {
    pub fn from_env(
        konfiguriert: Vec<ArgKonfiguration<'_>>,
    ) -> Result<Vec<ParsedArg>, Vec<ParseArgFehler>> {
        ParsedArg::parse(konfiguriert, env::args_os())
    }

    pub fn parse(
        konfiguriert: Vec<ArgKonfiguration<'_>>,
        args: impl Iterator<Item = OsString>,
    ) -> Result<Vec<ParsedArg>, Vec<ParseArgFehler>> {
        todo!()
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
pub struct ArgBeschreibung<T> {
    pub lang: String,
    pub kurz: Option<char>,
    pub hilfe: Option<String>,
    pub standard: Option<T>,
}

impl<T> ArgBeschreibung<T> {
    pub fn als_arg_name(&self) -> ArgName<'_> {
        let ArgBeschreibung { lang, kurz, .. } = self;
        ArgName { lang, kurz }
    }
}

pub enum Arg<T> {
    Flag {
        beschreibung: ArgBeschreibung<T>,
        aus_bool: Box<dyn Fn(bool) -> T>,
    },
    Wert {
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: Box<dyn Fn(OsString) -> Result<T, OsString>>,
    },
}

impl<T: Debug> Debug for Arg<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Flag { beschreibung, aus_bool: _ } => f
                .debug_struct("Flag")
                .field("arg", beschreibung)
                .field("aus_bool", &"<function>")
                .finish(),
            Self::Wert { beschreibung, meta_var, parse: _ } => f
                .debug_struct("Wert")
                .field("arg", beschreibung)
                .field("meta_var", meta_var)
                .field("parse", &"<function>")
                .finish(),
        }
    }
}

impl<T: Display> Arg<T> {
    pub fn als_arg_konfiguration(&self) -> ArgKonfiguration<'_> {
        match self {
            Arg::Flag { beschreibung, .. } => ArgKonfiguration::Flag(beschreibung.als_arg_name()),
            Arg::Wert { beschreibung, .. } => ArgKonfiguration::Wert(beschreibung.als_arg_name()),
        }
    }
}

pub struct Parser<T, E> {
    pub parse: Box<dyn Fn(&OsString) -> Result<T, E>>,
}

impl<T, E> Debug for Parser<T, E> {
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
    KonvertiereOsString,
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
                os_string
                    .to_str()
                    .ok_or(ParseStringFehler::KonvertiereOsString)
                    .and_then(|s| T::from_str(s).map_err(ParseStringFehler::from))
            }),
        }
    }
}
