//! Erzeuge Type-Alias für das letzte Generic.

use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{
    punctuated::Punctuated, token::Comma, Field, Fields, FieldsNamed, GenericParam, Ident,
    ItemStruct, Path, PathSegment, Type, TypeParam, TypePath, Visibility,
};

/// Teile Felder in deren Typ der Generic-Ident vorkommt/nicht vorkommt.
fn partition_generic_fields<'f>(
    generic: &Ident,
    named_fields: &'f Punctuated<Field, Comma>,
) -> (Vec<&'f Field>, Vec<&'f Field>) {
    named_fields.iter().partition(|Field { ty, .. }| {
        if let Type::Path(TypePath { path: Path { segments, .. }, .. }) = ty {
            if let Some(PathSegment { ident, .. }) = segments.first() {
                ident == generic
            } else {
                false
            }
        } else {
            false
        }
    })
}

/// Erzeuge den [`TokenStream`] für die neuen Definitionen.
#[allow(clippy::too_many_arguments)]
fn erzeuge_typ_definitionen(
    crate_ident: &Ident,
    arg: &TokenStream,
    vis: &Visibility,
    ident: &Ident,
    serialisiert_ident: &Ident,
    unit_ident: &Ident,
    default_type: &Type,
    params: &[&GenericParam],
    params_start: &TokenStream,
    param_fields: &[&Option<Ident>],
    other_fields: &[&Option<Ident>],
) -> TokenStream {
    quote! {
        /// Eine serialisierbare Repräsentation.
        #vis type #serialisiert_ident<#(#params),*> = #ident<#params_start Option<#arg>>;

        /// Eine Variante ohne Anschlüsse.
        #vis type #unit_ident<#(#params),*> = #ident<#params_start ()>;

        impl<#(#params),*> #crate_ident::anschluss::de_serialisieren::Serialisiere<
            #serialisiert_ident<#(#params),*>
        > for #ident<#(#params),*>
        {
            fn serialisiere(&self) -> #serialisiert_ident<#(#params),*> {
                let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                #serialisiert_ident {
                    #(#other_fields: #other_fields.clone()),*,
                    #(
                        #param_fields: #param_fields.as_ref().map(
                            |steuerung| steuerung.serialisiere()
                        )
                    ),*
                }
            }
            fn anschlüsse(self) -> #crate_ident::anschluss::de_serialisieren::Anschlüsse {
                let mut anschlüsse = #crate_ident::anschluss::de_serialisieren::Anschlüsse::default();
                #(
                    if let Some(steuerung) = self.#param_fields {
                        anschlüsse.anhängen(steuerung.anschlüsse());
                    }
                )*
                anschlüsse
            }
        }

        impl<#(#params),*> #crate_ident::anschluss::de_serialisieren::Reserviere<#ident<#(#params),*>> for #serialisiert_ident<#(#params),*> {
            #[allow(unused_qualifications)]
            type MoveArg = <Option<#arg> as #crate_ident::anschluss::de_serialisieren::Reserviere<#default_type>>::MoveArg;
            type RefArg = <Option<#arg> as #crate_ident::anschluss::de_serialisieren::Reserviere<#default_type>>::RefArg;
            type MutRefArg = <Option<#arg> as #crate_ident::anschluss::de_serialisieren::Reserviere<#default_type>>::MutRefArg;

            fn reserviere(
                self,
                lager: &mut #crate_ident::anschluss::Lager,
                anschlüsse: #crate_ident::anschluss::de_serialisieren::Anschlüsse,
                move_arg: Self::MoveArg,
                ref_arg: &Self::RefArg,
                mut_ref_arg: &mut Self::MutRefArg,
            ) -> #crate_ident::anschluss::de_serialisieren::Ergebnis<#ident<#(#params),*>> {
                let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                (#(#param_fields),*)
                    .reserviere(lager, anschlüsse, move_arg, ref_arg, mut_ref_arg)
                    .konvertiere(|(#(#param_fields),*)| {
                        #ident { #(#other_fields),*, #(#param_fields),* }
                    })
            }
        }

        impl<#(#params),*> #ident<#(#params),*> {
            /// Clone in eine äquivalente Darstellung ohne Anschlüsse.
            pub fn mit_unit(&self) -> #unit_ident<#(#params),*> {
                let #ident { #(#other_fields),*, .. } = self;
                #unit_ident {
                    #(#other_fields: #other_fields.clone()),*,
                    #(#param_fields: ()),*
                }
            }
        }

        impl<#(#params),*> #unit_ident<#(#params),*> {
            /// Clone in eine äquivalente Darstellung mit [`None`] als Anschlüsse.
            pub fn mit_none<T>(&self) -> #ident<#params_start Option<T>> {
                let #ident { #(#other_fields),*, .. } = self;
                #ident {
                    #(#other_fields: #other_fields.clone()),*,
                    #(#param_fields: None),*
                }
            }
        }
    }
}

/// [`crate::alias_serialisiert_unit`]
#[allow(clippy::single_call_fn)]
pub(crate) fn alias_serialisiert_unit(arg: &TokenStream, item: &ItemStruct) -> TokenStream {
    let mut errors = Vec::new();

    let ItemStruct { vis, ident, fields, generics, .. } = &item;
    let mut type_definitionen = None;

    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let crate_ident: Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        if let Some((
            GenericParam::Type(TypeParam { ident: generic, default: Some(default_type), .. }),
            params,
        )) = generics.params.iter().collect::<Vec<_>>().split_last()
        {
            if let Fields::Named(FieldsNamed { named, .. }) = fields {
                let (param_fields, other_fields) = partition_generic_fields(generic, named);
                let param_fields: Vec<_> =
                    param_fields.into_iter().map(|field| &field.ident).collect();
                let other_fields: Vec<_> =
                    other_fields.into_iter().map(|field| &field.ident).collect();
                let serialisiert_ident = format_ident!("{}Serialisiert", ident);
                let unit_ident = format_ident!("{}Unit", ident);
                let params_start = if params.is_empty() { quote!() } else { quote!(#(#params),*,) };
                type_definitionen = Some(erzeuge_typ_definitionen(
                    &crate_ident,
                    arg,
                    vis,
                    ident,
                    &serialisiert_ident,
                    &unit_ident,
                    default_type,
                    params,
                    &params_start,
                    &param_fields,
                    &other_fields,
                ));
            } else {
                errors.push(String::from("Only named field supported!"));
            }
        } else {
            errors.push(String::from("Missing generics!"));
        }
    } else {
        errors.push(String::from("`zugkontrolle` missing in `Cargo.toml`"));
    }

    if !errors.is_empty() {
        let error_message = errors.join("\n");
        return quote! {
            compile_error!(#error_message);
            #item
        };
    }

    quote! {
        #item
        #type_definitionen
    }
}
