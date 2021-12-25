//! Mehrfach verwendete Utility Funktionen.

use std::collections::HashMap;

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
