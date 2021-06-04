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

mod weiche;
#[proc_macro_attribute]
pub fn create_richtung(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    weiche::create_richtung(args, ast).into()
}
#[proc_macro_attribute]
pub fn alias_save_unit(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(attr);
    let ast = syn::parse_macro_input!(item);

    weiche::alias_save_unit(args, ast).into()
}
