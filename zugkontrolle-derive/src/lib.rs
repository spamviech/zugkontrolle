use proc_macro::TokenStream;
use syn;

pub(crate) mod utils;

mod debug;
#[proc_macro_derive(Debug)]
pub fn debug_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    debug::impl_debug(&ast).into()
}

mod clone;
#[proc_macro_derive(Clone)]
pub fn clone_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    clone::impl_clone(&ast).into()
}

mod lookup;
#[proc_macro_attribute]
pub fn impl_lookup(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    lookup::impl_lookup(args, ast).into()
}

mod modus;
#[proc_macro_attribute]
pub fn make_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    modus::make_enum(args, ast).into()
}

mod chain;
#[proc_macro_attribute]
pub fn chain(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    chain::make_chain(args, ast).into()
}

mod richtung;
#[proc_macro_attribute]
pub fn create_richtung(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    richtung::create_richtung(args, ast).into()
}

mod alias;
#[proc_macro_attribute]
/// Internes Macro mit sehr spezifischen Vorraussetzungen.
///
/// Es wird erwartet, dass der default-Typ ein Option ist und eine Konvertierung in den Serialisiert-Typ
/// (Argument) über eine `serialisiere`-Methode möglich ist!
pub fn alias_serialisiert_unit(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item);

    alias::alias_serialisiert_unit(attr.into(), ast).into()
}

mod maps_methoden;
#[proc_macro_attribute]
/// Internes Macro mit sehr spezifischen Vorraussetzungen.
///
/// Es wird erwartet, dass die Funktion genau einen generic Typ hat,
// das erste Argument &mut self ist und alle anderen Argumente reine Namen-Pattern sind.
// Assoziierte Typen werden dem Zeichnen-Trait zugehörig angenommen.
pub fn erstelle_maps_methoden(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(item);

    maps_methoden::erstelle_methoden(ast).into()
}
