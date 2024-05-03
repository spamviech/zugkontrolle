//! Mehrfach verwendete Utility Funktionen.

use std::collections::HashMap;

use syn::{
    parse::Parser, punctuated::Punctuated, token::Plus, Attribute, Error, Field, GenericParam,
    Generics, Ident, LifetimeParam, Meta, MetaList, Path, PathSegment, Token, Type, TypeParamBound,
    TypePath, WherePredicate,
};

/// Markiere die generischen Typen mit `true`, die im `fields`-Iterator vorkommen.
#[allow(single_use_lifetimes)]
pub(crate) fn mark_fields_generic<'t, T>(
    fields: impl Iterator<Item = &'t Field>,
    generic_types: &mut HashMap<&Ident, (T, bool)>,
) {
    for field in fields {
        if let Type::Path(TypePath { path: Path { segments, .. }, .. }) = &field.ty {
            if let Some(PathSegment { ident, .. }) = segments.first() {
                if let Some((_, gefunden)) = generic_types.get_mut(ident) {
                    *gefunden = true;
                }
            }
        }
    }
}

/// Relevante Informationen aus [`Generics`], partitioniert nach ihrer Art.
pub(crate) struct PartitionierteGenericParameter<'t> {
    /// Lifetime-Parameter
    pub(crate) lifetimes: Vec<&'t LifetimeParam>,
    /// Typ-Parameter mit Trait-Bounds. Der [bool]-eintrag wird u.a. in [`mark_fields_generic`] verwendet.
    pub(crate) types: HashMap<&'t Ident, (&'t Punctuated<TypeParamBound, Plus>, bool)>,
    /// Typ-Parameter ohne Trait-Bounds
    pub(crate) type_names: Vec<&'t Ident>,
}

/// Extrahiere relevante Informationen aus den [`Generics`] und partitioniere sie nach ihrer Art.
pub(crate) fn partitioniere_generics(generics: &Generics) -> PartitionierteGenericParameter<'_> {
    generics.params.iter().fold(
        PartitionierteGenericParameter {
            lifetimes: Vec::new(),
            types: HashMap::new(),
            type_names: Vec::new(),
        },
        |PartitionierteGenericParameter { mut lifetimes, mut types, mut type_names }, generic| {
            match generic {
                GenericParam::Lifetime(lt) => lifetimes.push(lt),
                GenericParam::Type(ty) => {
                    type_names.push(&ty.ident);
                    let _ = types.insert(&ty.ident, (&ty.bounds, false));
                },
                GenericParam::Const(_c) => {},
            }
            PartitionierteGenericParameter { lifetimes, types, type_names }
        },
    )
}

/// Look for attributes prefixed with name and construct a where-clause from it.
/// E.g. `#[zugkontrolle_clone(M: Clone, <C as Trait>::A: Clone)]`
/// becomes `M: Clone, <C as Trait>::A: Clone` to allow more flexibility than the normal derive-macros.
pub(crate) fn parse_attributes_fn(
    attrs: &[Attribute],
    name: &str,
) -> Result<impl Iterator<Item = WherePredicate>, Error> {
    let intermediate: Vec<Punctuated<WherePredicate, Token!(,)>> = attrs
        .iter()
        .filter_map(|attr| {
            // sichergestellt durch `segments.len() == 1`
            #[allow(clippy::indexing_slicing)]
            match attr {
                Attribute {
                    meta: Meta::List(MetaList { path: Path { segments, .. }, tokens, .. }),
                    ..
                } if segments.len() == 1 && segments[0].ident == name => {
                    let parser = Punctuated::parse_terminated;
                    Some(parser.parse2(tokens.clone()))
                },
                _ => None,
            }
        })
        .collect::<Result<_, _>>()?;
    Ok(intermediate.into_iter().flat_map(Punctuated::into_iter))
}

/// Helper für [`parse_attributes_fn`]: Match das [Result] und gebe bei [`Err`] direkt `quote!(compile_error(#fehler));` zurück.
macro_rules! parse_attributes {
    ($attrs:expr, $name:expr) => {
        match $crate::util::parse_attributes_fn($attrs, $name) {
            Ok(vec) => vec,
            Err(parse_error) => {
                let error =
                    format!("Parse-error parsing constraints for {}: {:?}", $name, parse_error);
                return quote! {
                    compile_error!(#error);
                };
            },
        }
    };
}
pub(crate) use parse_attributes;
