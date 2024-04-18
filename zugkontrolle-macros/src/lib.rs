//! Macros für zugkontrolle.

use proc_macro::TokenStream;
use syn::{parse_macro_input, punctuated::Punctuated};

pub(crate) mod util;

mod debug;
#[proc_macro_derive(Debug, attributes(zugkontrolle_debug))]
/// Erzeuge eine [`Debug`]-Implementierung, ohne Constraints für Generics vorauszusetzen.
pub fn debug_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = parse_macro_input!(input);

    // Build the trait implementation
    debug::impl_debug(&ast).into()
}

mod clone;
#[proc_macro_derive(Clone, attributes(zugkontrolle_clone))]
/// Erzeuge eine [`Clone`]-Implementierung, ohne Constraints für Generics vorauszusetzen.
pub fn clone_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = parse_macro_input!(input);

    // Build the trait implementation
    clone::impl_clone(&ast).into()
}

mod nachschlagen;
#[proc_macro_attribute]
/// Erzeuge eine Struktur und zugehörige `zugkontrolle::nachschlagen::Nachschlagen`-Implementierung für das Enum.
pub fn impl_nachschlagen(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with Punctuated::parse_terminated);
    let ast = parse_macro_input!(item);

    nachschlagen::impl_nachschlagen(&args, &ast).into()
}

mod erstelle_enum;
#[proc_macro_attribute]
/// Erzeuge ein Enum mit identischen Varianten, ohne assoziierte Daten.
pub fn erstelle_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    erstelle_enum::erstelle_enum(args, &ast).into()
}

mod chain;
#[proc_macro_attribute]
/// Erzeuge eine identische Methode mit /_chain/-Suffix, die Method-chaining erlaubt.
pub fn chain(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    chain::make_chain(&args, &ast).into()
}

mod richtung;
#[proc_macro_attribute]
/// Erzeuge ein Richtung-Enum mit identischen Varianten bis auf /Anfang/,
/// sowie eine zugehörige `zugkontrolle::nachschlagen::Nachschlagen`-Struktur.
pub fn erstelle_richtung(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr);
    let ast = parse_macro_input!(item);

    richtung::erstelle_richtung(&args, &ast).into()
}

mod alias;
#[proc_macro_attribute]
/// Erzeuge /*Serialisiert/ und /'Unit/ Typ-Synonyme,
/// sowie `zugkontrolle::anschluss::de_serialisieren::Serialisiere`
/// und `zugkontrolle::::anschluss::de_serialisieren::Reserviere` Implementierungen.
///
/// Internes Macro mit sehr spezifischen Voraussetzungen.
///
/// Es wird erwartet, dass der default-Typ ein Option ist und eine Konvertierung in den Serialisiert-Typ
/// (Argument) über eine `serialisiere`-Methode möglich ist!
pub fn alias_serialisiert_unit(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item);

    alias::alias_serialisiert_unit(&attr.into(), &ast).into()
}

mod daten;
#[proc_macro_attribute]
#[deprecated]
/// Erstelle spezialisierte Methoden für alle Gleis-Typen mit entsprechendem Suffix.
/// Notwendig, damit `DatenAuswahl` kein Teil des APIs wird.
///
/// Internes Macro mit sehr spezifischen Voraussetzungen.
///
/// Die Funktion muss einen generic Typ mit DatenAuswahl-Constraint haben;
/// das Constraint darf nicht in der `where`-Klausel stehen.
/// Das erste Argument muss `&mut self`, oder `&'t mut self` und
/// alle anderen Argumente reine Namen-Pattern sein.
/// Die `where`-Klausel wird nicht inspiziert oder kopiert.
/// Für assoziierte Typen wird eine vollständig qualifizierte Form `<T as Trait>::Typ` empfohlen.
pub fn erstelle_daten_methoden(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item);

    daten::erstelle_methoden(&attr.into(), &ast).into()
}

mod sum_type_from;
#[proc_macro_derive(From)]
/// Erzeuge [`From`]-Implementierung für alle Varianten eines Enums, die genau ein Element halten.
pub fn derive_from(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input);

    sum_type_from::impl_from(ast).into()
}

mod metadata;
#[proc_macro]
/// Parse `cargo metadata` um verwendete crates für ein target zu erhalten.
pub fn verwendete_crates(input: TokenStream) -> TokenStream {
    let target = parse_macro_input!(input);

    metadata::verwendete_crates(&target).into()
}

#[proc_macro]
/// Parse `cargo metadata` um verwendete crates für das verwendete target zu erhalten.
/// Dazu werden viele über cfg-Aufrufe von [`verwendete_crates!`] erzeugt.
/// Die targets werden über `rustc --print target-list` ausgelesen.
pub fn target_crates(input: TokenStream) -> TokenStream {
    metadata::target_crates(&input.into()).into()
}
