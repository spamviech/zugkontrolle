//! Parsen von Kommandozeilen-Argumenten, inklusiver automatisch generierter (deutscher) Hilfe.

use std::{
    convert::identity,
    env,
    ffi::{OsStr, OsString},
    fmt::{Debug, Display},
    iter,
};

use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;

use crate::non_empty::NonEmpty;

#[derive(Debug, Clone)]
pub enum ParsedArgName {
    Kurz(String),
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
    pub kurz: &'t Option<String>,
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
    NichtKonfigurierteFlagKurzform(String),
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
                            let mut chars = kurz.graphemes(true);
                            let first = chars.next();
                            if let Some(c) = first {
                                let name = ParsedArgName::Kurz(c.to_owned());
                                let second = chars.next();
                                let bekannt_wert =
                                    konfiguriert_als(name.clone(), Konfiguriert::Wert).next();
                                if let Some("=") = second {
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
                                    for c in iter::once(c).chain(second.into_iter()).chain(chars) {
                                        let name = ParsedArgName::Kurz(c.to_owned());
                                        let mut bekannt =
                                            konfiguriert_als(name, Konfiguriert::Flag(true));
                                        if let Some(name) = bekannt.next() {
                                            parsed_args
                                                .push(ParsedArg::Flag { name, aktiviert: true })
                                        } else {
                                            errors.push(
                                                ParseArgFehler::NichtKonfigurierteFlagKurzform(
                                                    c.to_owned(),
                                                ),
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

#[derive(Debug)]
pub struct ArgBeschreibung<T> {
    pub lang: String,
    pub kurz: Option<String>,
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

#[derive(Debug)]
pub enum ArgString {
    Flag { beschreibung: ArgBeschreibung<String> },
    Wert { beschreibung: ArgBeschreibung<String>, meta_var: String },
}

pub struct Arg<T> {
    pub beschreibungen: Vec<ArgString>,
    pub flag_kurzformen: Vec<String>,
    pub parse: Box<dyn Fn(Vec<&OsStr>) -> (Result<T, NonEmpty<String>>, Vec<&OsStr>)>,
}

impl<T> Debug for Arg<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Arg")
            .field("beschreibungen", &self.beschreibungen)
            .field("parse", &"<function>")
            .finish()
    }
}

impl<T: 'static + Display + Clone> Arg<T> {
    #[inline(always)]
    pub fn flag_deutsch(
        beschreibung: ArgBeschreibung<T>,
        konvertiere: impl 'static + Fn(bool) -> T,
    ) -> Arg<T> {
        Arg::flag(beschreibung, konvertiere, "Fehlende Flag", "kein")
    }

    #[inline(always)]
    pub fn flag_english(
        beschreibung: ArgBeschreibung<T>,
        konvertiere: impl 'static + Fn(bool) -> T,
    ) -> Arg<T> {
        Arg::flag(beschreibung, konvertiere, "Missing Flag", "no")
    }

    pub fn flag(
        beschreibung: ArgBeschreibung<T>,
        konvertiere: impl 'static + Fn(bool) -> T,
        fehlende_flag: &'static str,
        invertiere_prefix: &str,
    ) -> Arg<T> {
        let name_kurz = beschreibung.kurz.clone();
        let name_lang = beschreibung.lang.clone();
        let invertiere_prefix_minus = format!("{}-", invertiere_prefix);
        let (beschreibung, standard) = beschreibung.als_string_beschreibung();
        Arg {
            beschreibungen: vec![ArgString::Flag { beschreibung }],
            flag_kurzformen: name_kurz.iter().cloned().collect(),
            parse: Box::new(move |args| {
                let name_kurz_str = name_kurz.as_ref().map(String::as_str);
                let mut ergebnis = None;
                let mut nicht_verwendet = Vec::new();
                for arg in args.iter() {
                    if let Some(string) = arg.to_str() {
                        if let Some(lang) = string.strip_prefix("--") {
                            if lang == name_lang {
                                ergebnis = Some(konvertiere(true));
                                continue;
                            } else if let Some(negiert) =
                                lang.strip_prefix(&invertiere_prefix_minus)
                            {
                                if negiert == name_lang {
                                    ergebnis = Some(konvertiere(false));
                                    continue;
                                }
                            }
                        } else if let Some(kurz) = string.strip_prefix("-") {
                            if kurz.graphemes(true).exactly_one().ok() == name_kurz_str {
                                ergebnis = Some(konvertiere(true));
                                continue;
                            }
                        }
                    }
                    nicht_verwendet.push(*arg);
                }
                if let Some(wert) = ergebnis {
                    (Ok(wert), nicht_verwendet)
                } else if let Some(wert) = &standard {
                    (Ok(wert.clone()), args)
                } else {
                    let mut fehlermeldung =
                        format!("{}: --[{}]{}", fehlende_flag, invertiere_prefix_minus, name_lang);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str(" | -");
                        fehlermeldung.push_str(kurz);
                    }
                    (Err(NonEmpty::singleton(fehlermeldung)), nicht_verwendet)
                }
            }),
        }
    }
}

impl<T: 'static + Display + Clone> Arg<T> {
    #[inline(always)]
    pub fn wert_deutsch(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl Fn(&OsString) -> Result<T, String>,
    ) -> Arg<T> {
        todo!()
    }

    #[inline(always)]
    pub fn value_english(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl Fn(&OsString) -> Result<T, String>,
    ) -> Arg<T> {
        todo!()
    }

    pub fn wert(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl 'static + Fn(&OsStr) -> Result<T, String>,
        fehlender_wert: &'static str,
    ) -> Arg<T> {
        let name_kurz = beschreibung.kurz.clone();
        let name_lang = beschreibung.lang.clone();
        let meta_var_clone = meta_var.clone();
        let (beschreibung, standard) = beschreibung.als_string_beschreibung();
        Arg {
            beschreibungen: vec![ArgString::Wert { beschreibung, meta_var }],
            flag_kurzformen: Vec::new(),
            parse: Box::new(move |args| {
                let name_kurz_str = name_kurz.as_ref().map(String::as_str);
                let mut ergebnis = None;
                let mut fehler = Vec::new();
                let mut name_ohne_wert = false;
                let mut nicht_verwendet = Vec::new();
                for arg in args.iter() {
                    if name_ohne_wert {
                        match parse(arg) {
                            Ok(wert) => ergebnis = Some(wert),
                            Err(parse_fehler) => fehler.push(parse_fehler),
                        }
                        name_ohne_wert = false;
                        continue;
                    } else if let Some(string) = arg.to_str() {
                        if let Some(lang) = string.strip_prefix("--") {
                            if let Some((name, wert_string)) = lang.split_once('=') {
                                if name == name_lang {
                                    match parse(wert_string.as_ref()) {
                                        Ok(wert) => ergebnis = Some(wert),
                                        Err(parse_fehler) => fehler.push(parse_fehler),
                                    }
                                    continue;
                                }
                            } else if lang == name_lang {
                                name_ohne_wert = true;
                                continue;
                            }
                        } else if let Some(kurz) = string.strip_prefix("-") {
                            let mut graphemes = kurz.graphemes(true);
                            if graphemes.next() == name_kurz_str {
                                todo!();
                                continue;
                            }
                        }
                    }
                    nicht_verwendet.push(*arg);
                }
                if let Some(fehler) = NonEmpty::from_vec(fehler) {
                    (Err(fehler), nicht_verwendet)
                } else if let Some(wert) = ergebnis {
                    (Ok(wert), nicht_verwendet)
                } else if let Some(wert) = &standard {
                    (Ok(wert.clone()), args)
                } else {
                    let mut fehlermeldung =
                        format!("{}: --{} {}", fehlender_wert, name_lang, meta_var_clone);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str(" | -");
                        fehlermeldung.push_str(kurz);
                        fehlermeldung.push_str("[=| ]");
                        fehlermeldung.push_str(&meta_var_clone);
                    }
                    (Err(NonEmpty::singleton(fehlermeldung)), nicht_verwendet)
                }
            }),
        }
    }
}

macro_rules! kombiniere {
    ($funktion: expr => $($args: ident),*) => {{
        #[allow(unused_mut)]
        let mut beschreibungen = Vec::new();
        $(beschreibungen.extend($args.beschreibungen);)*
        #[allow(unused_mut)]
        let mut flag_kurzformen = Vec::new();
        $(flag_kurzformen.extend($args.flag_kurzformen);)*
        Arg {
            beschreibungen,
            flag_kurzformen,
            parse: Box::new(move |args| {
                #[allow(unused_mut)]
                let mut fehler = Vec::new();
                $(
                    let (ergebnis, args) = ($args.parse)(args);
                    let $args = match ergebnis {
                        Ok(wert) => Some(wert),
                        Err(parse_fehler) => {
                            fehler.extend(parse_fehler);
                            None
                        }
                    };
                )*
                if let Some(fehler) = NonEmpty::from_vec(fehler) {
                    (Err(fehler), args)
                } else {
                    (Ok($funktion($($args.unwrap()),*)), args)
                }
            }),
        }
    }};
}
pub(crate) use kombiniere;

macro_rules! impl_kombiniere_n {
    ($name: ident ($($var: ident: $ty_var: ident),*)) => {
        pub fn $name<$($ty_var: 'static),*>(
            f: impl 'static + Fn($($ty_var),*) -> T,
            $($var: Arg<$ty_var>),*
        ) -> Arg<T> {
            kombiniere!(f=>$($var),*)
        }

    };
}

impl<T> Arg<T> {
    impl_kombiniere_n! {konstant()}
    impl_kombiniere_n! {konvertiere(a: A)}
    impl_kombiniere_n! {kombiniere2(a: A, b: B)}
    impl_kombiniere_n! {kombiniere3(a: A, b: B, c: C)}
    impl_kombiniere_n! {kombiniere4(a: A, b: B, c: C, d: D)}
    impl_kombiniere_n! {kombiniere5(a: A, b: B, c: C, d: D, e: E)}
    impl_kombiniere_n! {kombiniere6(a: A, b: B, c: C, d: D, e: E, f: F)}
    impl_kombiniere_n! {kombiniere7(a: A, b: B, c: C, d: D, e: E, f: F, g: G)}
    impl_kombiniere_n! {kombiniere8(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H)}
    impl_kombiniere_n! {kombiniere9(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I)}
}
