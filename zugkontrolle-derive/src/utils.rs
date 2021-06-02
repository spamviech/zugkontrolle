//! Mehrfach verwendete Utility Funktionen.

use std::collections::HashMap;

pub fn mark_fields_generic<'t>(
    fields: impl Iterator<Item = &'t syn::Field>,
    generic_types: &mut HashMap<&syn::Ident, bool>,
) {
    for field in fields {
        match &field.ty {
            syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
                if let Some(syn::PathSegment { ident, arguments }) = segments.first() {
                    if &ident.to_string() == "PhantomData" {
                        if let syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments { args, .. },
                        ) = arguments
                        {
                            if let Some(syn::GenericArgument::Type(first)) = args.first() {
                                mark_type_generic(first, generic_types)
                            }
                        }
                    }
                }
            },
            _ => {},
        }
    }
}

pub fn mark_type_generic(ty: &syn::Type, generic_types: &mut HashMap<&syn::Ident, bool>) {
    match ty {
        syn::Type::Path(p) => {
            if let Some(segment) = p.path.segments.first() {
                if let Some(v) = generic_types.get_mut(&segment.ident) {
                    *v = false
                }
            }
        },
        syn::Type::BareFn(syn::TypeBareFn {
            output: syn::ReturnType::Type(_r_arrow, ty), ..
        }) => mark_type_generic(ty, generic_types),
        syn::Type::Ptr(syn::TypePtr { elem, .. }) => mark_type_generic(elem, generic_types),
        syn::Type::Reference(syn::TypeReference { elem, .. }) => {
            mark_type_generic(elem, generic_types)
        },
        syn::Type::Tuple(syn::TypeTuple { elems, .. }) => {
            for elem in elems {
                mark_type_generic(elem, generic_types)
            }
        },
        _ => {
            panic!("Unsupported type in PhantomData generic: {:?}", ty)
        },
    }
}
