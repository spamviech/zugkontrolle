use proc_macro::TokenStream;
use syn;

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
#[proc_macro_derive(Lookup)]
pub fn anchor_lookup_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).expect("Failed to parse input!");

    // Build the trait implementation
    lookup::impl_anchor_lookup(&ast).into()
}
