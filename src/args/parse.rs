//! Parsen von Kommandozeilen-Argumenten, inklusiver automatisch generierter (deutscher) Hilfe.

use std::{
    ffi::OsStr,
    fmt::{Debug, Display},
};

use itertools::Itertools;
use unicode_segmentation::UnicodeSegmentation;
use version::version;
use void::Void;

use crate::non_empty::NonEmpty;

#[derive(Debug)]
pub struct ArgBeschreibung<T> {
    pub lang: String,
    pub kurz: Option<String>,
    pub hilfe: Option<String>,
    pub standard: Option<T>,
}

impl<T: Display> ArgBeschreibung<T> {
    pub fn als_string_beschreibung(self) -> (ArgBeschreibung<String>, Option<T>) {
        let ArgBeschreibung { lang, kurz, hilfe, standard } = self;
        let standard_string = standard.as_ref().map(ToString::to_string);
        (ArgBeschreibung { lang, kurz, hilfe, standard: standard_string }, standard)
    }
}

#[derive(Debug)]
enum ArgString {
    Flag { beschreibung: ArgBeschreibung<String> },
    Wert { beschreibung: ArgBeschreibung<String>, meta_var: String },
}

#[derive(Debug)]
pub enum ParseErgebnis<T> {
    Wert(T),
    FrühesBeenden(NonEmpty<String>),
    Fehler(NonEmpty<String>),
}

pub struct Arg<T> {
    beschreibungen: Vec<ArgString>,
    flag_kurzformen: Vec<String>,
    parse: Box<dyn Fn(Vec<&OsStr>) -> (ParseErgebnis<T>, Vec<&OsStr>)>,
}

impl<T> Debug for Arg<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Arg")
            .field("beschreibungen", &self.beschreibungen)
            .field("parse", &"<function>")
            .finish()
    }
}

// TODO insert separator to avoid accidental pairings

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
                let name_kurz_existiert = name_kurz_str.is_some();
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
                        } else if name_kurz_existiert {
                            if let Some(kurz) = string.strip_prefix('-') {
                                if kurz.graphemes(true).exactly_one().ok() == name_kurz_str {
                                    ergebnis = Some(konvertiere(true));
                                    continue;
                                }
                            }
                        }
                    }
                    nicht_verwendet.push(*arg);
                }
                if let Some(wert) = ergebnis {
                    (ParseErgebnis::Wert(wert), nicht_verwendet)
                } else if let Some(wert) = &standard {
                    (ParseErgebnis::Wert(wert.clone()), args)
                } else {
                    let mut fehlermeldung =
                        format!("{}: --[{}]{}", fehlende_flag, invertiere_prefix_minus, name_lang);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str(" | -");
                        fehlermeldung.push_str(kurz);
                    }
                    (ParseErgebnis::Fehler(NonEmpty::singleton(fehlermeldung)), nicht_verwendet)
                }
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
                let mut parse_auswerten = |arg| match parse(arg) {
                    Ok(wert) => ergebnis = Some(wert),
                    Err(parse_fehler) => fehler.push(parse_fehler),
                };
                for arg in args.iter() {
                    if name_ohne_wert {
                        parse_auswerten(arg);
                        name_ohne_wert = false;
                        continue;
                    } else if let Some(string) = arg.to_str() {
                        if let Some(lang) = string.strip_prefix("--") {
                            if let Some((name, wert_str)) = lang.split_once('=') {
                                if name == name_lang {
                                    parse_auswerten(wert_str.as_ref());
                                    continue;
                                }
                            } else if lang == name_lang {
                                name_ohne_wert = true;
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
                                        continue;
                                    };
                                    parse_auswerten(wert_str.as_ref());
                                }
                            }
                        }
                    }
                    nicht_verwendet.push(*arg);
                }
                if let Some(fehler) = NonEmpty::from_vec(fehler) {
                    (ParseErgebnis::Fehler(fehler), nicht_verwendet)
                } else if let Some(wert) = ergebnis {
                    (ParseErgebnis::Wert(wert), nicht_verwendet)
                } else if let Some(wert) = &standard {
                    (ParseErgebnis::Wert(wert.clone()), args)
                } else {
                    let mut fehlermeldung =
                        format!("{}: --{} {}", fehlender_wert, name_lang, meta_var_clone);
                    if let Some(kurz) = &name_kurz {
                        fehlermeldung.push_str(" | -");
                        fehlermeldung.push_str(kurz);
                        fehlermeldung.push_str("[=| ]");
                        fehlermeldung.push_str(&meta_var_clone);
                    }
                    (ParseErgebnis::Fehler(NonEmpty::singleton(fehlermeldung)), nicht_verwendet)
                }
            }),
        }
    }
}

impl<T: 'static> Arg<T> {
    pub fn version(self, programm_name: &str) -> Arg<T> {
        Arg::frühes_beenden(
            self,
            ArgBeschreibung {
                lang: "version".to_owned(),
                kurz: Some("v".to_owned()),
                hilfe: Some("Zeigt die aktuelle Version an.".to_owned()),
                standard: None,
            },
            format!("{} {}", programm_name, version!()),
        )
    }

    pub fn frühes_beenden(self, beschreibung: ArgBeschreibung<Void>, nachricht: String) -> Arg<T> {
        let Arg { mut beschreibungen, mut flag_kurzformen, parse } = self;
        let name_kurz = beschreibung.kurz.clone();
        let name_lang = beschreibung.lang.clone();
        let (beschreibung, _standard) = beschreibung.als_string_beschreibung();
        if let Some(kurz) = &beschreibung.kurz {
            flag_kurzformen.push(kurz.clone())
        }
        beschreibungen.push(ArgString::Flag { beschreibung });
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
                    if let Some(string) = arg.to_str() {
                        if let Some(lang) = string.strip_prefix("--") {
                            if lang == name_lang {
                                zeige_nachricht();
                                continue;
                            }
                        } else if name_kurz_existiert {
                            if let Some(kurz) = string.strip_prefix('-') {
                                if kurz.graphemes(true).exactly_one().ok() == name_kurz_str {
                                    zeige_nachricht();
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
