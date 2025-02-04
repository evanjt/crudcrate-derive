use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Fields, Lit, Meta, parse_macro_input};

/// Generates a "create" model struct.
/// Fields marked with #[update = false] are skipped.
#[proc_macro_derive(ToCreateModel, attributes(update))]
pub fn to_create_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let create_name = format_ident!("{}Create", name);

    // Ensure we have a struct with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToCreateModel only supports structs with named fields");
        }
    } else {
        panic!("ToCreateModel can only be derived for structs");
    };

    // Filter out fields with #[update = false].
    let filtered_fields = fields
        .into_iter()
        .filter(|field| {
            field.attrs.iter().all(|attr| {
                if attr.path().is_ident("update") {
                    // Use the already parsed meta (no need for tokens).
                    if let Meta::NameValue(nv) = attr.meta.clone() {
                        // The value is now an expression; match on it to extract the literal.
                        if let syn::Expr::Lit(expr_lit) = nv.value {
                            if let Lit::Bool(lit_bool) = expr_lit.lit {
                                // If the attribute is #[update = false], then lit_bool.value is false.
                                // Return that boolean to determine if this field should be included.
                                return lit_bool.value;
                            }
                        }
                    }
                }
                true
            })
        })
        .map(|field| {
            let ident = field.ident;
            let ty = field.ty;
            quote! {
                pub #ident: #ty
            }
        });

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #create_name {
            #(#filtered_fields),*
        }
    };

    TokenStream::from(expanded)
}

/// Generates an "update" model struct.
/// Each field is wrapped in Option<Option<T>> (with serde attributes)
/// so that you can distinguish between "field omitted" and "provided as null".
#[proc_macro_derive(ToUpdateModel, attributes(update))]
pub fn to_update_model(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let update_name = format_ident!("{}Update", name);

    // Ensure we have a struct with named fields.
    let fields = if let Data::Struct(data) = input.data {
        if let Fields::Named(named) = data.fields {
            named.named
        } else {
            panic!("ToUpdateModel only supports structs with named fields");
        }
    } else {
        panic!("ToUpdateModel can only be derived for structs");
    };

    // Filter out fields with #[update = false].
    let filtered_fields = fields
        .into_iter()
        .filter(|field| {
            field.attrs.iter().all(|attr| {
                if attr.path().is_ident("update") {
                    if let Meta::NameValue(nv) = attr.meta.clone() {
                        if let syn::Expr::Lit(expr_lit) = nv.value {
                            if let Lit::Bool(lit_bool) = expr_lit.lit {
                                return lit_bool.value;
                            }
                        }
                    }
                }
                true
            })
        })
        .map(|field| {
            let ident = field.ident;
            let ty = field.ty;
            quote! {
                #[serde(
                    default,
                    skip_serializing_if = "Option::is_none",
                    with = "::serde_with::rust::double_option"
                )]
                pub #ident: Option<Option<#ty>>
            }
        });

    let expanded = quote! {
        #[derive(Serialize, Deserialize, ToSchema)]
        pub struct #update_name {
            #(#filtered_fields),*
        }
    };

    TokenStream::from(expanded)
}
