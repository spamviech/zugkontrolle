//! Macros für zugkontrolle.

// Aktiviere alle Warnungen/Lints, außer:
// box_pointers, non_ascii_idents, unstable_features
#![warn(
    absolute_paths_not_starting_with_crate,
    elided_lifetimes_in_paths,
    explicit_outlives_requirements,
    keyword_idents,
    macro_use_extern_crate,
    meta_variable_misuse,
    missing_abi,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    noop_method_call,
    pointer_structural_match,
    rust_2021_incompatible_closure_captures,
    rust_2021_incompatible_or_patterns,
    rust_2021_prefixes_incompatible_syntax,
    rust_2021_prelude_collisions,
    single_use_lifetimes,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unsafe_op_in_unsafe_fn,
    unused_crate_dependencies,
    unused_extern_crates,
    unused_import_braces,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences
)]

use proc_macro::TokenStream;
use syn::parse_macro_input;

pub(crate) mod utils;

mod debug;
#[proc_macro_derive(Debug, attributes(zugkontrolle_debug))]
/// Erzeuge eine [Debug]-Implementierung, ohne Constraints für Generics vorauszusetzen.
pub fn debug_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = parse_macro_input!(input);

    // Build the trait implementation
    debug::impl_debug(&ast).into()
}

mod clone;
#[proc_macro_derive(Clone, attributes(zugkontrolle_clone))]
/// Erzeuge eine [Clone]-Implementierung, ohne Constraints für Generics vorauszusetzen.
pub fn clone_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = parse_macro_input!(input);

    // Build the trait implementation
    clone::impl_clone(&ast).into()
}

mod nachschlagen;
#[proc_macro_attribute]
/// Erzeuge eine Struktur und zugehörige [zugkontrolle::nachschlagen::Nachschlagen]-Implementierung für das Enum.
pub fn impl_nachschlagen(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    nachschlagen::impl_nachschlagen(args, ast).into()
}

mod erstelle_enum;
#[proc_macro_attribute]
/// Erzeuge ein Enum mit identischen Varianten, ohne assoziierte Daten.
pub fn make_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    erstelle_enum::erstelle_enum(args, ast).into()
}

mod chain;
#[proc_macro_attribute]
/// Erzeuge eine identische Methode mit /_chain/-Suffix, die Method-chaining erlaubt.
pub fn chain(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    chain::make_chain(args, ast).into()
}

mod richtung;
#[proc_macro_attribute]
/// Erzeuge ein Richtung-Enum mit identischen Varianten bis auf /Anfang/,
/// sowie eine zugehörige [zugkontrolle::nachschlagen::Nachschlagen]-Struktur.
pub fn erstelle_richtung(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    richtung::erstelle_richtung(args, ast).into()
}

mod alias;
#[proc_macro_attribute]
/// Erzeuge /*Serialisiert/ und /'Unit/ Typ-Synonyme,
/// sowie [zugkontrolle::anschluss::de_serialisieren::Serialisiere]
/// und [zugkontrolle::::anschluss::de_serialisieren::Reserviere] Implementierungen.
///
/// Internes Macro mit sehr spezifischen Voraussetzungen.
///
/// Es wird erwartet, dass der default-Typ ein Option ist und eine Konvertierung in den Serialisiert-Typ
/// (Argument) über eine `serialisiere`-Methode möglich ist!
pub fn alias_serialisiert_unit(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item);

    alias::alias_serialisiert_unit(attr.into(), ast).into()
}

mod daten;
#[proc_macro_attribute]
/// Erstelle spezialisierte Methoden für alle Gleis-Typen mit entsprechendem Suffix.
/// Notwendig, damit `DatenAuswahl` kein Teil des APIs wird.
///
/// Internes Macro mit sehr spezifischen Voraussetzungen.
///
/// Es wird erwartet, dass die Funktion genau einen generic Typ hat,
// das erste Argument &mut self ist und alle anderen Argumente reine Namen-Pattern sind.
// Assoziierte Typen werden dem Zeichnen-Trait zugehörig angenommen.
pub fn erstelle_daten_methoden(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item);

    daten::erstelle_methoden(attr.into(), ast).into()
}

mod sum_type_from;
#[proc_macro_derive(From)]
/// Erzeuge [From]-Implementierung für alle Varianten eines Enums, die genau ein Element halten.
pub fn derive_from(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input);

    sum_type_from::impl_from(ast).into()
}
