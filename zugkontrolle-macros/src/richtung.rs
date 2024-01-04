//! Erzeuge Richtung enum und RichtungAnschlüsse(Serialisiert) Strukturen mit Lookup-Implementierung.

use heck::ToSnakeCase;
use proc_macro2::TokenStream;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{format_ident, quote};
use syn::{Ident, ItemEnum, Variant, Visibility};

/// Erzeuge die Definition für die neuen Typen und zugehörige Trait-Implementierungen.
fn erzeuge_enum_definition(
    crate_ident: &Ident,
    vis: &Visibility,
    enum_variants: &[&Ident],
    default_variant: &Ident,
    struct_fields: &[Ident],
    stacked_unit_tuple_args: &Option<TokenStream>,
) -> TokenStream {
    let enum_variants_str = enum_variants.iter().map(ToString::to_string);
    quote! {
        #[zugkontrolle_macros::impl_nachschlagen(#crate_ident::anschluss::OutputAnschluss, RichtungAnschlüsse, Debug)]
        #[zugkontrolle_macros::impl_nachschlagen(#crate_ident::anschluss::OutputSerialisiert, RichtungAnschlüsseSerialisiert, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
        impl #crate_ident::anschluss::de_serialisieren::Serialisiere<RichtungAnschlüsseSerialisiert> for RichtungAnschlüsse {
            fn serialisiere(&self) -> RichtungAnschlüsseSerialisiert {
                let RichtungAnschlüsse { #(#struct_fields),* } = self;
                RichtungAnschlüsseSerialisiert { #(#struct_fields: #struct_fields.serialisiere()),* }
            }
            fn anschlüsse(self) -> #crate_ident::anschluss::de_serialisieren::Anschlüsse {
                let mut anschlüsse = #crate_ident::anschluss::de_serialisieren::Anschlüsse::default();
                #(
                    anschlüsse.anhängen(self.#struct_fields.anschlüsse());
                )*
                anschlüsse
            }
        }
        impl #crate_ident::anschluss::de_serialisieren::Reserviere<RichtungAnschlüsse> for RichtungAnschlüsseSerialisiert {
            type MoveArg = ();
            type RefArg = ();
            type MutRefArg = ();

            fn reserviere(
                self,
                lager: &mut #crate_ident::anschluss::Lager,
                anschlüsse: #crate_ident::anschluss::de_serialisieren::Anschlüsse,
                _move_arg: Self::MoveArg,
                _ref_arg: &Self::RefArg,
                _mut_ref_arg: &mut Self::MutRefArg,
            ) -> #crate_ident::anschluss::de_serialisieren::Ergebnis<RichtungAnschlüsse> {
                let RichtungAnschlüsseSerialisiert { #(#struct_fields),* } = self;
                #[allow(unused_parens)]
                (#(#struct_fields),*)
                    .reserviere(lager, anschlüsse, #stacked_unit_tuple_args, &#stacked_unit_tuple_args, &mut #stacked_unit_tuple_args)
                    .konvertiere(|(#(#struct_fields),*)| RichtungAnschlüsse { #(#struct_fields),* })
            }
        }
        impl Default for RichtungAnschlüsseSerialisiert {
            fn default() -> Self {
                RichtungAnschlüsseSerialisiert {
                    #(#struct_fields: #crate_ident::anschluss::OutputSerialisiert::Pin {pin:0, polarität: #crate_ident::anschluss::polarität::Polarität::Normal}),*
                }
            }
        }
    }
}

/// [`crate::erstelle_richtung`]
pub(crate) fn erstelle_richtung(args: &TokenStream, item: &ItemEnum) -> TokenStream {
    let mut errors = Vec::new();

    let ItemEnum { vis, variants, .. } = &item;
    if !args.is_empty() {
        errors.push(format!("No args supported, but {args:?} was given!",));
    }

    let mut enum_definition = None;
    if let Ok(zugkontrolle) = crate_name("zugkontrolle") {
        let crate_ident: Ident = match zugkontrolle {
            FoundCrate::Itself => format_ident!("{}", "crate"),
            FoundCrate::Name(name) => format_ident!("{}", name),
        };

        let enum_variants: Vec<_> = variants
            .iter()
            .filter_map(|Variant { ident, .. }| if ident == "Anfang" { None } else { Some(ident) })
            .collect();
        if let Some(default_variant) = enum_variants.first() {
            let struct_fields: Vec<Ident> = enum_variants
                .iter()
                .map(|variant| format_ident!("{}", &variant.to_string().to_snake_case()))
                .collect();
            let stacked_unit_tuple_args = enum_variants.iter().fold(None, |tokens, _ident| {
                Some(if let Some(tokens) = tokens {
                    quote! {((), #tokens)}
                } else {
                    quote! {()}
                })
            });

            enum_definition = Some(erzeuge_enum_definition(
                &crate_ident,
                vis,
                &enum_variants,
                default_variant,
                &struct_fields,
                &stacked_unit_tuple_args,
            ));
        } else {
            errors.push(String::from("Mindestens eine Variante benötigt!"));
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
        #enum_definition
    }
}
