//! Erzeuge Richtung enum und RichtungAnschlüsse(Serialisiert) Strukturen mit Lookup-Implementierung.

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};

pub(crate) fn erstelle_richtung(args: TokenStream, item: syn::ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let syn::ItemEnum { vis, variants, .. } = &item;
    if !args.is_empty() {
        errors.push(format!("No args supported, but {:?} was given!", args));
    }

    let mut enum_definition = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let base_ident: syn::Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let enum_variants: Vec<_> = variants
            .iter()
            .filter_map(
                |syn::Variant { ident, .. }| {
                    if ident.to_string() == "Anfang" {
                        None
                    } else {
                        Some(ident)
                    }
                },
            )
            .collect();
        let default_variant = enum_variants[0];
        let enum_variants_str = enum_variants.iter().map(ToString::to_string);
        let struct_fields: Vec<syn::Ident> = enum_variants
            .iter()
            .map(|variant| format_ident!("{}", &variant.to_string().to_snake_case()))
            .collect();
        let unit_args: Vec<_> = enum_variants.iter().map(|_| quote!(())).collect();
        let stacked_unit_tuple_args = enum_variants.iter().fold(None, |tokens, _ident| {
            Some(if let Some(tokens) = tokens {
                quote! {((), #tokens)}
            } else {
                quote! {()}
            })
        });

        enum_definition = Some(quote! {
            #[zugkontrolle_macros::impl_nachschlagen(#base_ident::anschluss::OutputAnschluss, RichtungAnschlüsse, Debug)]
            #[zugkontrolle_macros::impl_nachschlagen(#base_ident::anschluss::OutputSerialisiert, RichtungAnschlüsseSerialisiert, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
            /// Mögliche Richtungen zum Schalten.
            #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
            #vis enum Richtung {
                #(
                    #[allow(missing_docs)]
                    #enum_variants
                ),*
            }
            impl Default for Richtung {
                fn default() -> Self {
                    Richtung::#default_variant
                }
            }
            impl std::fmt::Display for Richtung {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}",
                        match self {
                            #( Richtung::#enum_variants =>  #enum_variants_str ),*
                        }
                    )
                }
            }
            #[allow(unused_qualifications)]
            impl crate::steuerung::weiche::MitRichtung<Richtung> for Richtung {
                fn aktuelle_richtung(&self) -> Option<Richtung> {
                    Some(*self)
                }
            }
            impl #base_ident::anschluss::de_serialisieren::Serialisiere<RichtungAnschlüsseSerialisiert> for RichtungAnschlüsse {
                fn serialisiere(&self) -> RichtungAnschlüsseSerialisiert {
                    let RichtungAnschlüsse { #(#struct_fields),* } = self;
                    RichtungAnschlüsseSerialisiert { #(#struct_fields: #struct_fields.serialisiere()),* }
                }
                fn anschlüsse(self) -> #base_ident::anschluss::de_serialisieren::Anschlüsse {
                    let mut anschlüsse = #base_ident::anschluss::de_serialisieren::Anschlüsse::default();
                    #(
                        anschlüsse.anhängen(self.#struct_fields.anschlüsse());
                    )*
                    anschlüsse
                }
            }
            impl #base_ident::anschluss::de_serialisieren::Reserviere<RichtungAnschlüsse> for RichtungAnschlüsseSerialisiert {
                type MoveArg = ();
                type RefArg = ();
                type MutRefArg = ();

                fn reserviere(
                    self,
                    lager: &mut #base_ident::anschluss::Lager,
                    anschlüsse: #base_ident::anschluss::de_serialisieren::Anschlüsse,
                    _move_arg: Self::MoveArg,
                    _ref_arg: &Self::RefArg,
                    _mut_ref_arg: &mut Self::MutRefArg,
                ) -> #base_ident::anschluss::de_serialisieren::Ergebnis<RichtungAnschlüsse> {
                    let RichtungAnschlüsseSerialisiert { #(#struct_fields),* } = self;
                    #[allow(unused_parens)]
                    (#(#struct_fields),*)
                        .reserviere(lager, anschlüsse, (#(#unit_args),*), &#stacked_unit_tuple_args, &mut #stacked_unit_tuple_args)
                        .konvertiere(|(#(#struct_fields),*)| RichtungAnschlüsse { #(#struct_fields),* })
                }
            }
            impl Default for RichtungAnschlüsseSerialisiert {
                fn default() -> Self {
                    RichtungAnschlüsseSerialisiert {
                        #(#struct_fields: #base_ident::anschluss::OutputSerialisiert::Pin {pin:0, polarität: #base_ident::anschluss::polarität::Polarität::Normal}),*
                    }
                }
            }
        })
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
        #enum_definition
    }
}
