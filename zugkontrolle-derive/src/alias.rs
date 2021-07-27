//! Erzeuge Type-Alias für das letzte Generic.

use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub fn alias_save_unit(arg: TokenStream, item: syn::ItemStruct) -> TokenStream {
    let mut errors = Vec::new();

    let syn::ItemStruct { vis, ident, fields, generics, .. } = &item;
    let mut type_definitionen = None;

    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };
        if let Some((
            syn::GenericParam::Type(syn::TypeParam { ident: g, default: Some(_type), .. }),
            params,
        )) = generics.params.iter().collect::<Vec<_>>().split_last()
        {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                let (param_fields, other_fields): (Vec<_>, Vec<_>) =
                    named.iter().partition(|syn::Field { ty, .. }| {
                        if let syn::Type::Path(syn::TypePath {
                            path: syn::Path { segments, .. },
                            ..
                        }) = ty
                        {
                            if let Some(syn::PathSegment { ident, .. }) = segments.first() {
                                ident == g
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    });
                let param_fields: Vec<_> =
                    param_fields.into_iter().map(|field| &field.ident).collect();
                let other_fields: Vec<_> =
                    other_fields.into_iter().map(|field| &field.ident).collect();
                let save_ident = format_ident!("{}Save", ident);
                let unit_ident = format_ident!("{}Unit", ident);
                let params_start = if params.is_empty() { quote!() } else { quote!(#(#params),*,) };
                type_definitionen = Some(quote! {
                    #vis type #save_ident<#(#params),*> = #ident<#params_start Option<#arg>>;
                    #vis type #unit_ident<#(#params),*> = #ident<#params_start ()>;
                    impl<#(#params),*> #base_ident::anschluss::speichern::ToSave for #ident<#(#params),*> {
                        type Save = #save_ident<#(#params),*>;
                        fn to_save(&self) -> #save_ident<#(#params),*> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            #save_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(
                                    #param_fields: #param_fields.as_ref().map(
                                        |steuerung| steuerung.to_save()
                                    )
                                ),*
                            }
                        }
                    }
                    impl<#(#params),*> #base_ident::anschluss::speichern::Reserviere<#ident<#(#params),*>> for #save_ident<#(#params),*> {
                        fn reserviere(self, anschlüsse: &mut #base_ident::anschluss::Anschlüsse) -> Result<#ident<#(#params),*>, #base_ident::anschluss::Error> {
                            let #ident { #(#other_fields),*, #(#param_fields),* } = self;
                            Ok(#ident {
                                #(#other_fields),*,
                                #(
                                    #param_fields: #param_fields.map(
                                        |steuerung| steuerung.reserviere(anschlüsse)
                                    ).transpose()?
                                ),*
                            })
                        }
                    }
                    impl<#(#params),*> #ident<#(#params),*> {
                        pub fn to_unit(&self) -> #unit_ident<#(#params),*> {
                            let #ident { #(#other_fields),*, .. } = self;
                            #unit_ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(#param_fields: ()),*
                            }
                        }
                    }
                    impl<#(#params),*> #unit_ident<#(#params),*> {
                        pub fn to_option<T>(&self) -> #ident<#params_start Option<T>> {
                            let #ident { #(#other_fields),*, .. } = self;
                            #ident {
                                #(#other_fields: #other_fields.clone()),*,
                                #(#param_fields: None),*
                            }
                        }
                    }
                })
            } else {
                errors.push("Only named field supported!".to_string())
            }
        } else {
            errors.push("Missing generics!".to_string())
        }
    } else {
        errors.push("`zugkontrolle` missing in `Cargo.toml`".to_string())
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
