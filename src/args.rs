//! Kommandozeilen-Argumente.

use std::{
    convert::identity,
    env,
    ffi::OsString,
    fmt::{Debug, Display, Write},
    iter,
    str::FromStr,
};

use argh::{EarlyExit, FromArgs, TopLevelCommand};
use itertools::Itertools;
use log::error;
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

#[derive(Debug, Clone)]
#[allow(variant_size_differences)]
pub enum ParsedArgName {
    Kurz(char),
    Lang(String),
}

#[derive(Debug)]
pub enum ParsedArg<'t> {
    Flag { name: ArgName<'t>, aktiviert: bool },
    Wert { name: ArgName<'t>, wert: OsString },
}

#[derive(Debug, Clone, Copy)]
pub struct ArgName<'t> {
    pub lang: &'t String,
    pub kurz: &'t Option<char>,
}

impl ArgName<'_> {
    pub fn passend(self, parsed: &ParsedArgName) -> bool {
        match (self, parsed) {
            (ArgName { kurz: Some(konfiguriert), .. }, ParsedArgName::Kurz(geparsed)) => {
                konfiguriert == geparsed
            }
            (ArgName { lang: konfiguriert, .. }, ParsedArgName::Lang(geparsed)) => {
                konfiguriert == geparsed
            }
            (_, _) => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ArgKonfiguration<'t> {
    Flag(ArgName<'t>),
    Wert(ArgName<'t>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Konfiguriert {
    Flag(bool),
    Wert,
}

impl<'t> ArgKonfiguration<'t> {
    pub fn ist_konfiguriert(self, geparsed: &ParsedArgName) -> Option<(Konfiguriert, ArgName<'t>)> {
        match self {
            ArgKonfiguration::Flag(name) => {
                if name.passend(geparsed) {
                    Some((Konfiguriert::Flag(true), name))
                } else {
                    match geparsed {
                        ParsedArgName::Lang(lang) if lang.starts_with("no-") => {
                            if name.passend(&ParsedArgName::Lang(lang[3..].to_owned())) {
                                Some((Konfiguriert::Flag(false), name))
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }
            }
            ArgKonfiguration::Wert(name) if name.passend(geparsed) => {
                Some((Konfiguriert::Wert, name))
            }
            _ => None,
        }
    }

    pub fn ist_konfiguriert_als(
        self,
        geparsed: &ParsedArgName,
        soll: Konfiguriert,
    ) -> Option<ArgName<'t>> {
        self.ist_konfiguriert(geparsed).and_then(|(konfiguriert, konfigurierter_name)| {
            if konfiguriert == soll {
                Some(konfigurierter_name)
            } else {
                None
            }
        })
    }
}

#[derive(Debug)]
pub enum ParseArgFehler {
    KonvertiereName(OsString),
    InvaliderName(String),
    NichtKonfigurierterName(ParsedArgName),
    NichtKonfigurierteFlagKurzform(char),
    FehlenderWert(ParsedArgName),
    WertFürFlag { name: ParsedArgName, wert: String },
}

impl ParsedArg<'_> {
    pub fn from_env<'t>(
        konfiguriert: &'t [ArgKonfiguration<'t>],
    ) -> Result<Vec<ParsedArg<'t>>, Vec<ParseArgFehler>> {
        ParsedArg::parse(konfiguriert, env::args_os())
    }

    pub fn parse<'t>(
        konfiguriert: &'t [ArgKonfiguration<'t>],
        args: impl Iterator<Item = OsString>,
    ) -> Result<Vec<ParsedArg<'t>>, Vec<ParseArgFehler>> {
        let konfiguriert_als = |name, konfiguration| {
            konfiguriert
                .iter()
                .filter_map(move |config| config.ist_konfiguriert_als(&name, konfiguration))
        };
        let mut parsed_args = Vec::new();
        let mut errors = Vec::new();
        let mut wert_name = None;
        for arg in args {
            if let Some((_parsed_name, name)) = wert_name.take() {
                parsed_args.push(ParsedArg::Wert { name, wert: arg })
            } else {
                match arg.into_string() {
                    Ok(string) => {
                        // Parse Name und Wert
                        let mut parsed_name = None;
                        let parsed_wert;
                        if let Some(lang) = string.strip_prefix("--") {
                            if let Some((name, wert)) = lang.split_once('=') {
                                parsed_name = Some((ParsedArgName::Lang(name.to_owned()), None));
                                parsed_wert = Some(wert.to_string());
                            } else {
                                parsed_name = Some((ParsedArgName::Lang(lang.to_owned()), None));
                                parsed_wert = None;
                            }
                        } else if let Some(kurz) = string.strip_prefix('-') {
                            // TODO sauberer mit graphemes
                            // https://crates.io/crates/unicode-segmentation
                            // Für reine ascii-Characters nicht notwendig
                            let mut chars = kurz.chars();
                            let first = chars.next();
                            if let Some(c) = first {
                                let name = ParsedArgName::Kurz(c);
                                let second = chars.next();
                                let bekannt_wert =
                                    konfiguriert_als(name.clone(), Konfiguriert::Wert).next();
                                if let Some('=') = second {
                                    // "-k=<Wert>"
                                    parsed_name = Some((name, None));
                                    parsed_wert = Some(chars.collect());
                                } else if let Some(arg_name) = bekannt_wert {
                                    parsed_name =
                                        Some((name, Some((Konfiguriert::Wert, arg_name))));
                                    if let Some(c_wert) = second {
                                        // "-k<Wert>"
                                        parsed_wert =
                                            Some(iter::once(c_wert).chain(chars).collect());
                                    } else {
                                        // "-k <Wert>"
                                        parsed_wert = None;
                                    }
                                } else {
                                    // "-abc"
                                    // zusammenfassen mehrerer Flag-Kurzformen (oder eine einzelne)
                                    // TODO ist erneuter Aufruf von `kurz.chars()` hier zu bevorzugen?
                                    for c in iter::once(c).chain(second.into_iter()).chain(chars) {
                                        let name = ParsedArgName::Kurz(c);
                                        let mut bekannt =
                                            konfiguriert_als(name, Konfiguriert::Flag(true));
                                        if let Some(name) = bekannt.next() {
                                            parsed_args
                                                .push(ParsedArg::Flag { name, aktiviert: true })
                                        } else {
                                            errors.push(
                                                ParseArgFehler::NichtKonfigurierteFlagKurzform(c),
                                            )
                                        }
                                    }
                                    parsed_wert = None;
                                }
                            } else {
                                // "-"
                                parsed_wert = None;
                                errors.push(ParseArgFehler::InvaliderName(string));
                            }
                        } else {
                            // Name, der nicht mit "-" beginnt
                            // TODO Positions-Argumente nicht unterstützt
                            parsed_wert = None;
                            errors.push(ParseArgFehler::InvaliderName(string));
                        }
                        // Auswerten von geparstem Namen und Wert
                        if let Some((parsed_name, arg_konfiguration_name)) = parsed_name {
                            let konfigurationen: Vec<_> = iter::once(arg_konfiguration_name)
                                .chain(
                                    konfiguriert
                                        .iter()
                                        .map(|config| config.ist_konfiguriert(&parsed_name)),
                                )
                                .filter_map(identity)
                                .collect();
                            fn ist_wert_konfiguration(
                                (konfiguriert, _name): &(Konfiguriert, ArgName<'_>),
                            ) -> bool {
                                *konfiguriert == Konfiguriert::Wert
                            }
                            if konfigurationen.is_empty() {
                                errors.push(ParseArgFehler::NichtKonfigurierterName(parsed_name))
                            } else if let Some(wert) = parsed_wert {
                                if let Some((_konfiguriert, name)) =
                                    konfigurationen.into_iter().find(ist_wert_konfiguration)
                                {
                                    parsed_args.push(ParsedArg::Wert { name, wert: wert.into() })
                                } else {
                                    errors.push(ParseArgFehler::WertFürFlag {
                                        name: parsed_name,
                                        wert,
                                    })
                                }
                            } else {
                                if let Some(ix) =
                                    konfigurationen.iter().position(ist_wert_konfiguration)
                                {
                                    wert_name = Some((parsed_name, konfigurationen[ix].1))
                                } else if let Some((aktiviert, name)) = konfigurationen
                                    .into_iter()
                                    .flat_map(|(konfiguriert, name)| {
                                        if let Konfiguriert::Flag(aktiviert) = konfiguriert {
                                            Some((aktiviert, name))
                                        } else {
                                            None
                                        }
                                    })
                                    .next()
                                {
                                    parsed_args.push(ParsedArg::Flag { name, aktiviert })
                                } else {
                                    // Sollte nicht eintreten, aber besser als ein Crash
                                    errors
                                        .push(ParseArgFehler::NichtKonfigurierterName(parsed_name))
                                }
                            }
                        }
                    }
                    // TODO "--lang=<some_os_string>", "-k[=]<some_os_string>" wird nicht unterstützt
                    Err(os_string) => errors.push(ParseArgFehler::KonvertiereName(os_string)),
                }
            }
        }

        if let Some((parsed_name, _name)) = wert_name.take() {
            errors.push(ParseArgFehler::FehlenderWert(parsed_name))
        }

        if errors.is_empty() {
            Ok(parsed_args)
        } else {
            Err(errors)
        }
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
            _ => Err(s.to_owned()),
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

impl<T: Display> ArgBeschreibung<T> {
    pub fn als_string_beschreibung(self) -> (ArgBeschreibung<String>, Option<T>) {
        let ArgBeschreibung { lang, kurz, hilfe, standard } = self;
        let standard_string = standard.as_ref().map(ToString::to_string);
        (ArgBeschreibung { lang, kurz, hilfe, standard: standard_string }, standard)
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
        parse: Box<dyn Fn(&OsString) -> Result<T, &OsString>>,
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

impl<T> Arg<T> {
    pub fn als_arg_konfiguration(&self) -> ArgKonfiguration<'_> {
        match self {
            Arg::Flag { beschreibung, .. } => ArgKonfiguration::Flag(beschreibung.als_arg_name()),
            Arg::Wert { beschreibung, .. } => ArgKonfiguration::Wert(beschreibung.als_arg_name()),
        }
    }
}

#[derive(Debug)]
pub enum ArgString {
    Flag { beschreibung: ArgBeschreibung<String> },
    Wert { beschreibung: ArgBeschreibung<String>, meta_var: String },
}

pub struct ArgKombination<T> {
    pub beschreibung: Vec<ArgString>,
    pub parse: Box<dyn Fn(Vec<&OsString>) -> Result<(T, Vec<&OsString>), Vec<String>>>,
    // Fn(Vec<&OsString>) -> Result<(T, Vec<&OsString>), Vec<&OsString>>
    // with combine2(impl Fn(A, B) -> C, Arg<A>, Arg<B>) -> Arg<C>
}

impl<T> Debug for ArgKombination<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ArgKombination")
            .field("beschreibung", &self.beschreibung)
            .field("parse", &"<function>")
            .finish()
    }
}

impl<T: 'static + Display + Clone> ArgKombination<T> {
    pub fn flag(
        beschreibung: ArgBeschreibung<T>,
        konvertiere: impl 'static + Fn(bool) -> T,
    ) -> ArgKombination<T> {
        // TODO Kombination aus mehreren Flags, z.B. "-abc"
        // bei `kombiniereN` berücksichtigen?
        let name_kurz = beschreibung.kurz;
        let name_lang = beschreibung.lang.clone();
        let (beschreibung, standard) = beschreibung.als_string_beschreibung();
        ArgKombination {
            beschreibung: vec![ArgString::Flag { beschreibung }],
            parse: Box::new(move |args| {
                let mut ergebnis = None;
                let mut nicht_verwendet = Vec::new();
                for arg in args.iter() {
                    if let Some(string) = arg.to_str() {
                        if let Some(lang) = string.strip_prefix("--") {
                            if lang == name_lang {
                                ergebnis = Some(konvertiere(true));
                                break;
                            } else if let Some(negiert) = lang.strip_prefix("no-") {
                                if negiert == name_lang {
                                    ergebnis = Some(konvertiere(false));
                                    break;
                                }
                            }
                        } else if let Some(kurz) = string.strip_prefix("-") {
                            if kurz.chars().exactly_one().ok() == name_kurz {
                                ergebnis = Some(konvertiere(true));
                                break;
                            }
                        }
                    }
                    nicht_verwendet.push(*arg);
                }
                if let Some(wert) = ergebnis {
                    Ok((wert, nicht_verwendet))
                } else if let Some(wert) = &standard {
                    Ok((wert.clone(), args))
                } else {
                    let mut fehlermeldung = format!("Fehlende Flag: `--[no-]{}`", name_lang);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str("| -");
                        fehlermeldung.push(*kurz);
                    }
                    Err(vec![fehlermeldung.clone()])
                }
            }),
        }
    }

    pub fn aus_arg(arg: Arg<T>) -> ArgKombination<T> {
        match arg {
            Arg::Flag { beschreibung, aus_bool } => ArgKombination::flag(beschreibung, aus_bool),
            Arg::Wert { beschreibung, meta_var, parse } => {
                ArgKombination::wert(beschreibung, meta_var, parse)
            }
        }
    }
}

impl<T: Display> ArgKombination<T> {
    pub fn wert(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl Fn(&OsString) -> Result<T, &OsString>,
    ) -> ArgKombination<T> {
        let (beschreibung, standard) = beschreibung.als_string_beschreibung();
        ArgKombination {
            beschreibung: vec![ArgString::Wert { beschreibung, meta_var }],
            parse: Box::new(|args| todo!()),
        }
    }
}

impl<T> ArgKombination<T> {
    pub fn kombiniere2<A, B>(f: impl Fn(A, B) -> T, a: Arg<A>, b: Arg<B>) -> ArgKombination<T> {
        todo!()
    }
}
