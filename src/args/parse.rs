//! Parsen von Kommandozeilen-Argumenten, inklusiver automatisch generierter (deutscher) Hilfe.

use std::{
    env,
    ffi::OsStr,
    fmt::{Debug, Display},
    iter,
    path::{Path, PathBuf},
};

use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;
use void::Void;

use crate::non_empty::NonEmpty;

#[derive(Debug, Clone)]
pub struct ArgBeschreibung<T> {
    pub lang: String,
    pub kurz: Option<String>,
    pub hilfe: Option<String>,
    pub standard: Option<T>,
}

impl<T: Display> ArgBeschreibung<T> {
    fn als_string_beschreibung(self) -> (ArgBeschreibung<String>, Option<T>) {
        let ArgBeschreibung { lang, kurz, hilfe, standard } = self;
        let standard_string = standard.as_ref().map(ToString::to_string);
        (ArgBeschreibung { lang, kurz, hilfe, standard: standard_string }, standard)
    }
}

#[derive(Debug)]
enum ArgString {
    Flag { beschreibung: ArgBeschreibung<String>, invertiere_prefix: Option<String> },
    Wert { beschreibung: ArgBeschreibung<String>, meta_var: String },
}

// TODO Eigener Fehler-Typ mit Display-Implementierung, parametrisiert über ParseFehler-Typ
#[derive(Debug)]
pub enum ParseErgebnis<T> {
    Wert(T),
    FrühesBeenden(NonEmpty<String>),
    Fehler(NonEmpty<String>),
}

// TODO parse-methode, die flag_kurzformen berücksichtigt
// TODO shortcut zum direkten Verwenden von std::env::args_os
pub struct Arg<T> {
    beschreibungen: Vec<ArgString>,
    flag_kurzformen: Vec<String>,
    parse: Box<dyn Fn(Vec<Option<&OsStr>>) -> (ParseErgebnis<T>, Vec<Option<&OsStr>>)>,
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
            beschreibungen: vec![ArgString::Flag {
                beschreibung,
                invertiere_prefix: Some(invertiere_prefix.to_owned()),
            }],
            flag_kurzformen: name_kurz.iter().cloned().collect(),
            parse: Box::new(move |args| {
                let name_kurz_str = name_kurz.as_ref().map(String::as_str);
                let name_kurz_existiert = name_kurz_str.is_some();
                let mut ergebnis = None;
                let mut nicht_verwendet = Vec::new();
                for arg in args {
                    if let Some(string) = arg.and_then(OsStr::to_str) {
                        if let Some(lang) = string.strip_prefix("--") {
                            if lang == name_lang {
                                ergebnis = Some(konvertiere(true));
                                nicht_verwendet.push(None);
                                continue;
                            } else if let Some(negiert) =
                                lang.strip_prefix(&invertiere_prefix_minus)
                            {
                                if negiert == name_lang {
                                    ergebnis = Some(konvertiere(false));
                                    nicht_verwendet.push(None);
                                    continue;
                                }
                            }
                        } else if name_kurz_existiert {
                            if let Some(kurz) = string.strip_prefix('-') {
                                if kurz.graphemes(true).exactly_one().ok() == name_kurz_str {
                                    ergebnis = Some(konvertiere(true));
                                    nicht_verwendet.push(None);
                                    continue;
                                }
                            }
                        }
                    }
                    nicht_verwendet.push(arg);
                }
                let ergebnis = if let Some(wert) = ergebnis {
                    ParseErgebnis::Wert(wert)
                } else if let Some(wert) = &standard {
                    ParseErgebnis::Wert(wert.clone())
                } else {
                    let mut fehlermeldung =
                        format!("{}: --[{}]{}", fehlende_flag, invertiere_prefix_minus, name_lang);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str(" | -");
                        fehlermeldung.push_str(kurz);
                    }
                    ParseErgebnis::Fehler(NonEmpty::singleton(fehlermeldung))
                };
                (ergebnis, nicht_verwendet)
            }),
        }
    }

    #[inline(always)]
    pub fn wert_deutsch(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl 'static + Fn(&OsStr) -> Result<T, String>,
    ) -> Arg<T> {
        Arg::wert(beschreibung, meta_var, parse, "Fehlender Wert")
    }

    #[inline(always)]
    pub fn value_english(
        beschreibung: ArgBeschreibung<T>,
        meta_var: String,
        parse: impl 'static + Fn(&OsStr) -> Result<T, String>,
    ) -> Arg<T> {
        Arg::wert(beschreibung, meta_var, parse, "Missing Value")
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
        let mut fehlermeldung = format!("{}: --{} {}", fehlender_wert, name_lang, meta_var_clone);
        if let Some(kurz) = &name_kurz {
            fehlermeldung.push_str(" | -");
            fehlermeldung.push_str(kurz);
            fehlermeldung.push_str("[=| ]");
            fehlermeldung.push_str(&meta_var_clone);
        }
        Arg {
            beschreibungen: vec![ArgString::Wert { beschreibung, meta_var }],
            flag_kurzformen: Vec::new(),
            parse: Box::new(move |args| {
                let name_kurz_str = name_kurz.as_ref().map(String::as_str);
                let name_kurz_existiert = name_kurz_str.is_some();
                let mut ergebnis = None;
                let mut fehler = Vec::new();
                let mut name_ohne_wert = false;
                let mut nicht_verwendet = Vec::new();
                let mut parse_auswerten = |arg| {
                    if let Some(wert_os_str) = arg {
                        match parse(wert_os_str) {
                            Ok(wert) => ergebnis = Some(wert),
                            Err(parse_fehler) => fehler.push(parse_fehler),
                        }
                    } else {
                        fehler.push(fehlermeldung.clone())
                    }
                };
                for arg in args {
                    if name_ohne_wert {
                        parse_auswerten(arg);
                        name_ohne_wert = false;
                        continue;
                    } else if let Some(string) = arg.and_then(OsStr::to_str) {
                        if let Some(lang) = string.strip_prefix("--") {
                            if let Some((name, wert_str)) = lang.split_once('=') {
                                if name == name_lang {
                                    parse_auswerten(Some(wert_str.as_ref()));
                                    continue;
                                }
                            } else if lang == name_lang {
                                name_ohne_wert = true;
                                nicht_verwendet.push(None);
                                continue;
                            }
                        } else if name_kurz_existiert {
                            if let Some(kurz) = string.strip_prefix('-') {
                                let mut graphemes = kurz.graphemes(true);
                                if graphemes.next() == name_kurz_str {
                                    let rest = graphemes.as_str();
                                    let wert_str = if let Some(wert_str) = rest.strip_prefix('=') {
                                        wert_str
                                    } else if !rest.is_empty() {
                                        rest
                                    } else {
                                        name_ohne_wert = true;
                                        nicht_verwendet.push(None);
                                        continue;
                                    };
                                    parse_auswerten(Some(wert_str.as_ref()));
                                }
                            }
                        }
                    }
                    nicht_verwendet.push(arg);
                }
                if let Some(fehler) = NonEmpty::from_vec(fehler) {
                    (ParseErgebnis::Fehler(fehler), nicht_verwendet)
                } else if let Some(wert) = ergebnis {
                    (ParseErgebnis::Wert(wert), nicht_verwendet)
                } else if let Some(wert) = &standard {
                    (ParseErgebnis::Wert(wert.clone()), nicht_verwendet)
                } else {
                    (
                        ParseErgebnis::Fehler(NonEmpty::singleton(fehlermeldung.clone())),
                        nicht_verwendet,
                    )
                }
            }),
        }
    }
}

impl<T: 'static> Arg<T> {
    pub fn version_deutsch(self, programm_name: &str, version: &str) -> Arg<T> {
        let beschreibung = ArgBeschreibung {
            lang: "version".to_owned(),
            kurz: Some("v".to_owned()),
            hilfe: Some("Zeigt die aktuelle Version an.".to_owned()),
            standard: None,
        };
        self.zeige_version(beschreibung, programm_name, version)
    }

    pub fn version_english(self, program_name: &str, version: &str) -> Arg<T> {
        let beschreibung = ArgBeschreibung {
            lang: "version".to_owned(),
            kurz: Some("v".to_owned()),
            hilfe: Some("Show the current version.".to_owned()),
            standard: None,
        };
        self.zeige_version(beschreibung, program_name, version)
    }

    pub fn zeige_version(
        self,
        beschreibung: ArgBeschreibung<Void>,
        programm_name: &str,
        version: &str,
    ) -> Arg<T> {
        self.frühes_beenden(beschreibung, format!("{} {}", programm_name, version))
    }

    pub fn hilfe(self, programm_name: &str, name_regex_breite: usize) -> Arg<T> {
        let beschreibung = ArgBeschreibung {
            lang: "hilfe".to_owned(),
            kurz: Some("h".to_owned()),
            hilfe: Some("Zeigt diesen Text an.".to_owned()),
            standard: None,
        };
        self.erstelle_hilfe(beschreibung, programm_name, "OPTIONEN", "standard", name_regex_breite)
    }

    pub fn hilfe_und_version(
        self,
        programm_name: &str,
        version: &str,
        name_regex_breite: usize,
    ) -> Arg<T> {
        self.version_deutsch(programm_name, version)
            .hilfe(&format!("{} {}", programm_name, version), name_regex_breite)
    }

    pub fn help(self, program_name: &str, name_regex_width: usize) -> Arg<T> {
        let beschreibung = ArgBeschreibung {
            lang: "help".to_owned(),
            kurz: Some("h".to_owned()),
            hilfe: Some("Show this text.".to_owned()),
            standard: None,
        };
        self.erstelle_hilfe(beschreibung, program_name, "OPTIONS", "default", name_regex_width)
    }

    pub fn help_and_version(
        self,
        program_name: &str,
        version: &str,
        name_regex_breite: usize,
    ) -> Arg<T> {
        self.version_english(program_name, version)
            .help(&format!("{} {}", program_name, version), name_regex_breite)
    }

    pub fn erstelle_hilfe(
        self,
        eigene_beschreibung: ArgBeschreibung<Void>,
        programm_name: &str,
        optionen: &str,
        standard: &str,
        name_regex_breite: usize,
    ) -> Arg<T> {
        let current_exe = env::current_exe().ok();
        let exe_name = current_exe
            .as_ref()
            .map(PathBuf::as_path)
            .and_then(Path::file_name)
            .and_then(OsStr::to_str)
            .unwrap_or(programm_name);
        let benutzen = format!("./{} [{}]", exe_name, optionen);
        let mut hilfe_text = format!("{}\n{}\n\n{}:\n", programm_name, benutzen, optionen);
        let eigener_arg_string = ArgString::Flag {
            beschreibung: eigene_beschreibung.clone().als_string_beschreibung().0,
            invertiere_prefix: None,
        };
        fn hilfe_zeile(
            standard: &str,
            name_regex_breite: usize,
            hilfe_text: &mut String,
            name_regex: &String,
            beschreibung: &ArgBeschreibung<String>,
        ) {
            hilfe_text.push_str("  ");
            hilfe_text.push_str(name_regex);
            let bisherige_breite = 2 + name_regex.graphemes(true).count();
            let einrücken = " ".repeat(name_regex_breite - bisherige_breite);
            hilfe_text.push_str(&einrücken);
            if let Some(hilfe) = &beschreibung.hilfe {
                hilfe_text.push_str(hilfe);
            }
            if let Some(standard_wert) = &beschreibung.standard {
                if beschreibung.hilfe.is_some() {
                    hilfe_text.push(' ');
                }
                hilfe_text.push('[');
                hilfe_text.push_str(standard);
                hilfe_text.push_str(": ");
                hilfe_text.push_str(standard_wert);
                hilfe_text.push(']');
            }
            hilfe_text.push('\n');
        }
        for beschreibung in self.beschreibungen.iter().chain(iter::once(&eigener_arg_string)) {
            match beschreibung {
                ArgString::Flag { beschreibung, invertiere_prefix } => {
                    let mut name_regex = "--".to_owned();
                    if let Some(prefix) = invertiere_prefix {
                        name_regex.push('[');
                        name_regex.push_str(prefix);
                        name_regex.push_str("]-");
                    }
                    name_regex.push_str(&beschreibung.lang);
                    if let Some(kurz) = &beschreibung.kurz {
                        name_regex.push_str(" | -");
                        name_regex.push_str(kurz);
                    }
                    hilfe_zeile(
                        standard,
                        name_regex_breite,
                        &mut hilfe_text,
                        &name_regex,
                        beschreibung,
                    );
                }
                ArgString::Wert { beschreibung, meta_var } => {
                    let mut name_regex = "--".to_owned();
                    name_regex.push_str(&beschreibung.lang);
                    name_regex.push_str("(=| )");
                    name_regex.push_str(meta_var);
                    if let Some(kurz) = &beschreibung.kurz {
                        name_regex.push_str(" | -");
                        name_regex.push_str(kurz);
                        name_regex.push_str("[=| ]");
                        name_regex.push_str(meta_var);
                    }
                    hilfe_zeile(
                        standard,
                        name_regex_breite,
                        &mut hilfe_text,
                        &name_regex,
                        beschreibung,
                    );
                }
            }
        }
        self.frühes_beenden(eigene_beschreibung, hilfe_text)
    }

    pub fn frühes_beenden(self, beschreibung: ArgBeschreibung<Void>, nachricht: String) -> Arg<T> {
        let Arg { mut beschreibungen, mut flag_kurzformen, parse } = self;
        let name_kurz = beschreibung.kurz.clone();
        let name_lang = beschreibung.lang.clone();
        let (beschreibung, _standard) = beschreibung.als_string_beschreibung();
        if let Some(kurz) = &beschreibung.kurz {
            flag_kurzformen.push(kurz.clone())
        }
        beschreibungen.push(ArgString::Flag { beschreibung, invertiere_prefix: None });
        Arg {
            beschreibungen,
            flag_kurzformen,
            parse: Box::new(move |args| {
                let name_kurz_str = name_kurz.as_ref().map(String::as_str);
                let name_kurz_existiert = name_kurz_str.is_some();
                let mut nicht_verwendet = Vec::new();
                let mut nachrichten = Vec::new();
                let mut zeige_nachricht = || nachrichten.push(nachricht.clone());
                for arg in args {
                    if let Some(string) = arg.and_then(OsStr::to_str) {
                        if let Some(lang) = string.strip_prefix("--") {
                            if lang == name_lang {
                                zeige_nachricht();
                                nicht_verwendet.push(None);
                                continue;
                            }
                        } else if name_kurz_existiert {
                            if let Some(kurz) = string.strip_prefix('-') {
                                if kurz.graphemes(true).exactly_one().ok() == name_kurz_str {
                                    zeige_nachricht();
                                    nicht_verwendet.push(None);
                                    continue;
                                }
                            }
                        }
                    }
                    nicht_verwendet.push(arg);
                }
                if let Some(frühes_beenden) = NonEmpty::from_vec(nachrichten) {
                    (ParseErgebnis::FrühesBeenden(frühes_beenden), nicht_verwendet)
                } else {
                    parse(nicht_verwendet)
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
                #[allow(unused_mut)]
                let mut frühes_beenden = Vec::new();
                $(
                    let (ergebnis, args) = ($args.parse)(args);
                    let $args = match ergebnis {
                        ParseErgebnis::Wert(wert) => Some(wert),
                        ParseErgebnis::FrühesBeenden(nachrichten) => {
                            frühes_beenden.extend(nachrichten);
                            None
                        }
                        ParseErgebnis::Fehler(parse_fehler) => {
                            fehler.extend(parse_fehler);
                            None
                        }
                    };
                )*
                if let Some(fehler) = NonEmpty::from_vec(fehler) {
                    (ParseErgebnis::Fehler(fehler), args)
                } else if let Some(nachrichten) = NonEmpty::from_vec(frühes_beenden) {
                    (ParseErgebnis::FrühesBeenden(nachrichten), args)
                } else {
                    (ParseErgebnis::Wert($funktion($($args.unwrap()),*)), args)
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

#[test]
fn hilfe_test() {
    use std::{convert::identity, ffi::OsString};
    let arg = Arg::hilfe_und_version(
        Arg::flag_deutsch(
            ArgBeschreibung {
                lang: "test".to_owned(),
                kurz: None,
                hilfe: Some("hilfe".to_owned()),
                standard: Some(false),
            },
            identity,
        ),
        "programm",
        "0.test",
        20,
    );
    let hilfe = OsString::from("--hilfe".to_owned());
    match (arg.parse)(vec![Some(&hilfe)]) {
        (ParseErgebnis::FrühesBeenden(nachrichten), nicht_verwendet) => {
            let übrige = nicht_verwendet.iter().filter_map(Option::as_ref).count();
            if übrige > 0 {
                eprintln!("Nicht verwendete Argumente: {:?}", nicht_verwendet);
                std::process::exit(1);
            } else {
                for nachricht in nachrichten {
                    println!("{}", nachricht);
                }
                std::process::exit(0);
            }
        }
        res => {
            eprintln!("Unerwartetes Ergebnis: {:?}", res);
            std::process::exit(2);
        }
    }
}
