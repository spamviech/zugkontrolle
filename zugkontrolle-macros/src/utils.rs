//! Mehrfach verwendete Utility Funktionen.

use std::collections::HashMap;

use syn::parse::Parser;

#[allow(single_use_lifetimes)]
pub(crate) fn mark_fields_generic<'t, T>(
    fields: impl Iterator<Item = &'t syn::Field>,
    generic_types: &mut HashMap<&syn::Ident, (T, bool)>,
) {
    for field in fields {
        match &field.ty {
            syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
                if let Some(syn::PathSegment { ident, .. }) = segments.first() {
                    if let Some((_, v)) = generic_types.get_mut(ident) {
                        *v = true
                    }
                }
            }
            _ => {}
        }
    }
}

pub(crate) fn parse_attributes_fn(
    attrs: &Vec<syn::Attribute>,
    name: &str,
) -> Result<impl Iterator<Item = syn::WherePredicate>, syn::Error> {
    let intermediate: Vec<syn::punctuated::Punctuated<syn::WherePredicate, syn::Token!(,)>> = attrs
        .iter()
        .filter(|syn::Attribute { path: syn::Path { segments, .. }, .. }| {
            segments.len() == 1 && segments[0].ident == name
        })
        .map(|syn::Attribute { tokens, .. }| {
            let tokens_str = tokens.to_string();
            let len = tokens_str.len();
            // entferne Klammern als String, syn::parenthesized weigert sich zu kooperieren
            let slice = &tokens_str[1..len - 1];
            let parser = syn::punctuated::Punctuated::parse_terminated;
            parser.parse_str(slice)
        })
        .collect::<Result<_, _>>()?;
    Ok(intermediate.into_iter().flat_map(syn::punctuated::Punctuated::into_iter))
}

macro_rules! parse_attributes {
    ($attrs:expr, $name:expr) => {
        match crate::utils::parse_attributes_fn($attrs, $name) {
            Ok(vec) => vec,
            Err(parse_error) => {
                let error = format!(
                    "Parse-error parsing constraints for zugkontrolle_debug: {:?}",
                    parse_error
                );
                return quote! {
                    compile_error! { #error }
                };
            }
        }
    };
}
pub(crate) use parse_attributes;
