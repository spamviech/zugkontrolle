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
            },
            _ => {},
        }
    }
}

/// Look for attributes prefixed with name and construct a where-clause from it.
/// E.g. #[zugkontrolle_clone(M: Clone, <C as Trait>::A: Clone)]
/// becomes `M: Clone, <C as Trait>::A: Clone` to allow more flexibility than the normal derive-macros.
pub(crate) fn parse_attributes_fn(
    attrs: &Vec<syn::Attribute>,
    name: &str,
) -> Result<impl Iterator<Item = syn::WherePredicate>, syn::Error> {
    let intermediate: Vec<syn::punctuated::Punctuated<syn::WherePredicate, syn::Token!(,)>> = attrs
        .iter()
        .filter_map(|attr| match attr {
            syn::Attribute {
                meta:
                    syn::Meta::List(syn::MetaList { path: syn::Path { segments, .. }, tokens, .. }),
                ..
            } if segments.len() == 1 && segments[0].ident == name => {
                let parser = syn::punctuated::Punctuated::parse_terminated;
                Some(parser.parse2(tokens.clone()))
            },
            _ => None,
        })
        .collect::<Result<_, _>>()?;
    Ok(intermediate.into_iter().flat_map(syn::punctuated::Punctuated::into_iter))
}

macro_rules! parse_attributes {
    ($attrs:expr, $name:expr) => {
        match crate::utils::parse_attributes_fn($attrs, $name) {
            Ok(vec) => vec,
            Err(parse_error) => {
                let error =
                    format!("Parse-error parsing constraints for {}: {:?}", $name, parse_error);
                return quote! {
                    compile_error! { #error }
                };
            },
        }
    };
}
pub(crate) use parse_attributes;
