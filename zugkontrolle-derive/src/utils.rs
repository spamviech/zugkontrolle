//! Mehrfach verwendete Utility Funktionen.

use std::collections::HashMap;

pub fn mark_fields_generic<'t>(
    fields: impl Iterator<Item = &'t syn::Field>,
    generic_types: &mut HashMap<&syn::Ident, bool>,
) {
    for field in fields {
        match &field.ty {
            syn::Type::Path(syn::TypePath { path: syn::Path { segments, .. }, .. }) => {
                if let Some(syn::PathSegment { ident, .. }) = segments.first() {
                    if let Some(v) = generic_types.get_mut(ident) {
                        *v = true
                    }
                }
            },
            _ => {},
        }
    }
}
